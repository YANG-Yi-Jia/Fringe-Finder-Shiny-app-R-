# R/mod_venue.R
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)
library(leaflet)
library(htmltools)

fix_url <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", NA_character_,
         ifelse(grepl("^https?://", x), x, paste0("https://", x)))
}

radius_fun <- function(n) pmax(3, sqrt(n))

# ---------- UI ----------
mod_venue_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-wrap",
    layout_columns(
      col_widths = c(2, 10),
      
      # Sidebar
      div(
        class = "glass side-sticky",
        div(
          class = "glass-inner",
          
          tags$div(class = "venue-help-title",
                   "Explore venues on the 2025 Fringe map"),
          tags$div(class = "venue-help-subtitle",
                   "See where 2025 shows are happening across Edinburgh."),
          tags$div(class = "venue-help-highlight",
                   "Click a venue on the map to view its profile, shows and official website."),
          
          tags$hr(style = "opacity:0.35;"),
          
          textInput(ns("q"), label = NULL,
                    placeholder = "Search name/address..."),
          
          tags$div(style="height:10px;"),
          
          tags$div(class="filter-title", "Performances ▼"),
          radioButtons(
            ns("sort_mode"), label = NULL,
            choices  = c("All venue", "Low to high", "High to low", "Accessible venues only"),
            selected = "All venue"
          )
        )
      ),
      
      # Main content
      div(
        div(
          class = "glass",
          div(
            class = "glass-inner",
            leafletOutput(ns("map"), height = "62vh"),
            tags$div(style="height:12px;"),
            uiOutput(ns("detail"))
          )
        ),
        
        tags$div(style="height:14px;"),
        
        div(
          class = "glass",
          div(
            class = "glass-inner",
            DTOutput(ns("table"))
          )
        )
      )
    )
  )
}

mod_venue_server <- function(id, data_path = "venue_table.xlsx") {
  moduleServer(id, function(input, output, session) {
    
    df0 <- reactiveVal(NULL)
    selected_code <- reactiveVal(NULL)
    
    map_id   <- session$ns("map")
    table_id <- session$ns("table")
    dt_proxy <- DT::dataTableProxy(table_id, session = session)
    
    fix_url <- function(x) {
      x <- as.character(x)
      ifelse(is.na(x) | x == "", NA_character_,
             ifelse(grepl("^https?://", x), x, paste0("https://", x)))
    }
    radius_fun <- function(n) pmax(3, sqrt(n))
    
    # read once
    observeEvent(TRUE, {
      if (!file.exists(data_path)) stop("Venue table file not found: ", data_path)
      
      df <- readxl::read_excel(data_path)
      
      needed <- c("Venue_code","Venue","Address","Postcode","Date(s)",
                  "Performances","Unique_events","Website","Description",
                  "Accessibility","Latitude","Longitude","Accessible_flag")
      miss <- setdiff(needed, names(df))
      if (length(miss) > 0) stop("Venue table missing columns: ", paste(miss, collapse = ", "))
      
      df <- df %>%
        mutate(
          Venue_code = str_squish(as.character(Venue_code)),
          Venue      = str_squish(as.character(Venue)),
          Address    = str_squish(as.character(Address)),
          Postcode   = str_squish(as.character(Postcode)),
          `Date(s)`  = as.character(.data[["Date(s)"]]),
          Website    = fix_url(Website),
          Description   = as.character(Description),
          Accessibility = as.character(Accessibility),
          Latitude  = suppressWarnings(as.numeric(Latitude)),
          Longitude = suppressWarnings(as.numeric(Longitude)),
          Unique_events = suppressWarnings(as.integer(Unique_events)),
          Performances  = suppressWarnings(as.numeric(Performances)), 
          accessible_flag = as.logical(Accessible_flag)
        ) %>%
        filter(!is.na(Latitude), !is.na(Longitude), !is.na(Venue_code), Venue_code != "")
      
      df0(df)
      
      code <- isolate(selected_code())
      if (!is.null(code) && !any(df$Venue_code == code)) selected_code(NULL)
      
    }, once = TRUE)
    
    # shared filter
    base_filtered <- reactive({
      df <- df0()
      req(df)
      
      q <- str_squish(tolower(if (is.null(input$q)) "" else input$q))
      if (nzchar(q)) {
        df <- df %>%
          filter(
            str_detect(tolower(Venue), fixed(q)) |
              str_detect(tolower(Address), fixed(q)) |
              str_detect(tolower(Postcode), fixed(q))
          )
      }
      
      df
    })
    
    # MAP dataset (stable): search + accessible
    map_df <- reactive({
      df <- base_filtered()
      req(df)
      
      mode <- if (is.null(input$sort_mode)) "All venue" else input$sort_mode
      if (mode == "Accessible venues only") df <- df %>% filter(accessible_flag %in% TRUE)
      
      df
    })
    
    # TABLE dataset: search + accessible + performances sorting
    table_df <- reactive({
      df <- base_filtered()
      req(df)
      
      mode <- if (is.null(input$sort_mode)) "All venue" else input$sort_mode
      
      if (mode == "Accessible venues only") {
        df <- df %>% filter(accessible_flag %in% TRUE)
      } else if (mode == "Low to high") {
        df <- df %>% arrange(coalesce(Performances, -Inf), Venue)
      } else if (mode == "High to low") {
        df <- df %>% arrange(desc(coalesce(Performances, -Inf)), Venue)
      }
      df
    })
    
    # ---------- MAP ----------
    output$map <- renderLeaflet({
      df <- map_df()
      req(df)
      
      pal <- colorNumeric("YlOrRd", domain = df$Unique_events)
      
      popup_html <- with(df, paste0(
        "<b>", ifelse(is.na(Venue) | Venue == "", "(Unnamed venue)", htmlEscape(Venue)), "</b><br/>",
        "Unique events: <b>", Unique_events, "</b><br/>",
        ifelse(!is.na(Address) & Address != "", paste0(htmlEscape(Address), "<br/>"), ""),
        ifelse(!is.na(Postcode) & Postcode != "", paste0(htmlEscape(Postcode), "<br/>"), ""),
        ifelse(!is.na(`Date(s)`) & `Date(s)` != "", paste0("Dates: <b>", htmlEscape(`Date(s)`), "</b><br/>"), ""),
        ifelse(!is.na(Website) & Website != "",
               paste0('<a href="', Website, '" target="_blank" rel="noopener">Website</a><br/>'), ""),
        ifelse(!is.na(Accessibility) & Accessibility != "",
               paste0("Accessibility: ", htmlEscape(Accessibility), "<br/>"), ""),
        ifelse(!is.na(Description) & Description != "",
               paste0("<small>", htmlEscape(Description), "</small>"), "")
      ))
      
      leaflet(df, options = leafletOptions(zoomControl = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,
          radius = ~radius_fun(Unique_events),
          color = ~pal(Unique_events),
          fillOpacity = 0.75, stroke = FALSE,
          label = ~Venue,
          popup = popup_html,
          layerId = ~Venue_code
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Unique_events,
                  title = "Unique events", opacity = 1) %>%
        addScaleBar(position = "bottomleft")
    })
    
    # map click -> selected_code
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      req(click$id)
      selected_code(str_squish(as.character(click$id)))
    }, ignoreInit = TRUE)
    
    # table click -> selected_code
    observeEvent(input$table_rows_selected, {
      df <- table_df()
      req(df)
      idx <- input$table_rows_selected
      if (length(idx) == 1) selected_code(df$Venue_code[idx])
    }, ignoreInit = TRUE)
    
    # selected_code -> select row in DT
    observeEvent(selected_code(), {
      code <- selected_code()
      req(code)
      
      df <- table_df()
      req(df)
      
      idx <- match(code, df$Venue_code)
      if (is.na(idx)) return()
      
      DT::selectRows(dt_proxy, idx)
      page_len <- 50
      page <- as.integer((idx - 1) %/% page_len + 1)
      DT::selectPage(dt_proxy, page)
      
    }, ignoreInit = TRUE)
    
    # Detail panel
    output$detail <- renderUI({
      df <- table_df()
      req(df)
      
      code <- selected_code()
      if (is.null(code) || !any(df$Venue_code == code)) {
        div(class = "venue-detail",
            tags$div(class="venue-detail-title", "Venue profile"),
            tags$p("Click a venue marker on the map or a row in the table to view details here.")
        )
      } else {
        v <- df %>% filter(Venue_code == code) %>% slice(1)
        
        website_link <- if (!is.na(v$Website[1]) && v$Website[1] != "") {
          tags$a(href = v$Website[1], target = "_blank", rel="noopener", "Open official website")
        } else tags$span("(No website listed)")
        
        div(class = "venue-detail",
            tags$div(class="venue-detail-title", v$Venue[1]),
            tags$p(tags$b("Dates: "), v$`Date(s)`[1],
                   "   |   ",
                   tags$b("Unique events: "), v$Unique_events[1],
                   "   |   ",
                   tags$b("Total performances: "), v$Performances[1]),
            tags$p(tags$b("Address: "), v$Address[1]),
            tags$p(tags$b("Postcode: "), v$Postcode[1]),
            tags$p(tags$b("Website: "), website_link),
            if (!is.na(v$Accessibility[1]) && v$Accessibility[1] != "")
              tags$p(tags$b("Accessibility: "), v$Accessibility[1]),
            if (!is.na(v$Description[1]) && v$Description[1] != "")
              tags$p(tags$b("Description: "), v$Description[1])
        )
      }
    })
    
    # DT table
    output$table <- renderDT({
      df <- table_df()
      req(df)
      
      show_df <- df %>%
        transmute(
          Venue,
          Address,
          Postcode,
          `Date(s)` = .data[["Date(s)"]],
          Performances = ifelse(is.na(Performances), "", format(Performances, trim = TRUE)),
          Website = ifelse(!is.na(Website) & Website != "",
                           paste0('<a href="', Website, '" target="_blank" rel="noopener">Website</a>'),
                           "")
        )
      
      datatable(
        show_df,
        rownames = FALSE,
        escape = FALSE,
        selection = "single",
        options = list(
          pageLength = 50,
          searching = FALSE,
          info = TRUE,
          paging = TRUE,
          ordering = FALSE,  
          scrollX = TRUE,
          scrollY = "38vh",
          dom = "tip"
        )
      )
    })
  })
}