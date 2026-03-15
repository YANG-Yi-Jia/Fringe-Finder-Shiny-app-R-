# R/mod_insights.R
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(readxl)

source("R/mod_ins_general.R")
source("R/mod_ins_price.R")
source("R/mod_ins_research.R")


to_number <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(gsub("[^0-9.]+", "", x)))
}

first_nonblank <- function(x){
  x <- as.character(x)
  x <- x[!is.na(x) & stringr::str_trim(x) != ""]
  if (length(x)) x[1] else NA_character_
}
min_pos <- function(x){
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x) & is.finite(x) & x > 0]
  if (length(x)) min(x) else NA_real_
}

pick_col <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) NA_character_ else hit[1]
}

list_year_files <- function(data_dir = "data") {
  if (!dir.exists(data_dir)) return(data.frame(year = integer(), path = character()))
  files <- list.files(data_dir, pattern = "^\\d{4}\\.xlsx$", full.names = TRUE)
  if (!length(files)) return(data.frame(year = integer(), path = character()))
  yrs <- as.integer(gsub("\\.xlsx$", "", basename(files)))
  out <- data.frame(year = yrs, path = files, stringsAsFactors = FALSE)
  out[order(out$year), , drop = FALSE]
}

read_one_year <- function(path, year_value, skip = 2) {
  df <- readxl::read_excel(path, skip = skip)
  nms <- names(df)
  
  # column mapping (best effort)
  col_year    <- pick_col(nms, c("Year", "year"))
  col_code    <- pick_col(nms, c("Code", "code", "Event code", "event_code", "ID", "Id"))
  col_genre   <- pick_col(nms, c("Genre", "genre"))
  col_country <- pick_col(nms, c("Country", "country"))
  
  col_venue <- pick_col(nms, c("Venue", "venue"))
  col_vcode <- pick_col(nms, c("Venue code", "Venue_code", "venue_code"))
  col_vpost <- pick_col(nms, c("Venue postcode", "Venue_postcode", "venue_postcode", "Postcode", "postcode"))
  col_vaddr <- pick_col(nms, c("Venue address", "Venue_address", "venue_address", "Address", "address"))
  col_vweb  <- pick_col(nms, c("Venue website", "Venue_website", "venue_website", "Website", "website"))
  col_vacc  <- pick_col(nms, c("Venue accessibility", "Venue_accessibility", "venue_accessibility"))
  
  col_age   <- pick_col(nms, c("Age category", "Age", "age", "age_category", "Age_category"))
  col_full  <- pick_col(nms, c("Lowest full price", "lowest full price", "lowest_full_price"))
  col_conc  <- pick_col(nms, c("Lowest concession price", "lowest concession price", "lowest_concession_price"))
  
  col_perf  <- pick_col(nms, c("Performances #", "Performances", "performances", "Performances#"))
  col_fdate <- pick_col(nms, c("First performance date", "First date", "First_date"))
  col_ldate <- pick_col(nms, c("Last performance date", "Last date", "Last_date"))
  
  col_lat <- pick_col(nms, c("Latitude", "lat", "LATITUDE"))
  col_lng <- pick_col(nms, c("Longitude", "lng", "LNG", "LONGITUDE"))
  
  # safe vectors
  year_vec <- if (!is.na(col_year)) as.integer(df[[col_year]]) else as.integer(year_value)
  
  out <- df %>%
    mutate(
      year  = as.integer(year_vec),
      code  = if (!is.na(col_code))  str_squish(as.character(.data[[col_code]]))  else NA_character_,
      genre = if (!is.na(col_genre)) str_squish(as.character(.data[[col_genre]])) else NA_character_,
      country = if (!is.na(col_country)) str_squish(as.character(.data[[col_country]])) else NA_character_,
      
      venue = if (!is.na(col_venue)) str_squish(as.character(.data[[col_venue]])) else NA_character_,
      venue_code = if (!is.na(col_vcode)) str_squish(as.character(.data[[col_vcode]])) else NA_character_,
      venue_postcode = if (!is.na(col_vpost)) str_squish(as.character(.data[[col_vpost]])) else NA_character_,
      venue_address  = if (!is.na(col_vaddr)) str_squish(as.character(.data[[col_vaddr]])) else NA_character_,
      venue_website  = if (!is.na(col_vweb))  str_squish(as.character(.data[[col_vweb]]))  else NA_character_,
      venue_accessibility = if (!is.na(col_vacc)) str_squish(as.character(.data[[col_vacc]])) else NA_character_,
      
      age = if (!is.na(col_age)) str_squish(as.character(.data[[col_age]])) else NA_character_,
      
      lowest_full_price       = if (!is.na(col_full)) to_number(.data[[col_full]]) else NA_real_,
      lowest_concession_price = if (!is.na(col_conc)) to_number(.data[[col_conc]]) else NA_real_,
      
      performances = if (!is.na(col_perf)) suppressWarnings(as.integer(to_number(.data[[col_perf]]))) else NA_integer_,
      first_date = if (!is.na(col_fdate)) suppressWarnings(as.Date(.data[[col_fdate]])) else as.Date(NA),
      last_date  = if (!is.na(col_ldate)) suppressWarnings(as.Date(.data[[col_ldate]])) else as.Date(NA),
      
      latitude  = if (!is.na(col_lat)) suppressWarnings(to_number(.data[[col_lat]])) else NA_real_,
      longitude = if (!is.na(col_lng)) suppressWarnings(to_number(.data[[col_lng]])) else NA_real_
    ) %>%
    filter(!is.na(code), code != "")
  
  out
}

# --------- UI ----------
mod_insights_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-wrap",
    layout_columns(
      col_widths = c(2, 10),
      
      div(
        class = "glass side-sticky",
        div(
          class = "glass-inner",
          tags$div(class = "ins-help-title", "Insights hub"),
          tags$div(
            class = "ins-help-body",
            tagList(
              "All charts are derived from the official Fringe API spreadsheets in ",
              tags$code("data/2022.xlsx–2025.xlsx"),
              "."
            )
          ),
          tags$hr(style = "opacity:0.35;"),
          tags$div(class = "ins-dl-title", "Year range"),
          uiOutput(ns("year_ui")),
          tags$hr(style = "opacity:0.35;"),
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              "Data update (Admin)",
              tags$div(class = "ins-help-body",
                       tags$b("Detected year files"),
                       uiOutput(ns("years_detected")))
            )
          )
        )
      ),
      
      div(
        div(
          class = "glass",
          div(
            class = "glass-inner",
            
            # plotly resize fix on tab switch (prevents “squeezed in corner”)
            tags$script(HTML(sprintf("
              (function(){
                var id = '%s';
                $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"]', function(){
                  try { if(window.Plotly){ window.Plotly.Plots.resizeAll && window.Plotly.Plots.resizeAll(); } } catch(e){}
                  setTimeout(function(){
                    try {
                      $('.plotly.html-widget').each(function(){
                        if(this.id && window.Plotly){ Plotly.Plots.resize(this); }
                      });
                    } catch(e){}
                  }, 120);
                });
              })();
            ", ns("ins_page")))),
            
            tabsetPanel(
              id = ns("ins_page"),
              type = "tabs",
              tabPanel("General dashboard",  mod_ins_general_ui(ns("gen"))),
              tabPanel("Price Insights",     mod_ins_price_ui(ns("pri"))),
              tabPanel("Research dashboard", mod_ins_research_ui(ns("res")))
            )
          )
        )
      )
    )
  )
}

# --------- Server ----------
mod_insights_server <- function(id, data_dir = "data", skip = 2, dedup_year_code = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    years_tbl <- reactive(list_year_files(data_dir))
    
 
    ev_all <- reactive({
      yf <- years_tbl()
      validate(need(nrow(yf) > 0, "No year files found in data/. Expected files like data/2022.xlsx"))
      
      pieces <- lapply(seq_len(nrow(yf)), function(i) {
        read_one_year(yf$path[i], yf$year[i], skip = skip)
      })
      
      ev <- bind_rows(pieces) %>%
        mutate(
          year = as.integer(year),
          code = str_squish(as.character(code)),
          venue_code = str_squish(as.character(venue_code))
        ) %>%
        filter(!is.na(year), !is.na(code), code != "")
      
      
      if (isTRUE(dedup_year_code)) {
        ev <- ev %>%
          group_by(year, code) %>%
          summarise(
            genre   = first_nonblank(genre),
            country = first_nonblank(country),
            age     = first_nonblank(age),
            
            venue      = first_nonblank(venue),
            venue_code = first_nonblank(venue_code),
            venue_postcode = first_nonblank(venue_postcode),
            venue_address  = first_nonblank(venue_address),
            venue_website  = first_nonblank(venue_website),
            venue_accessibility = first_nonblank(venue_accessibility),
            
            # numeric fields: keep conservative/robust values
            lowest_full_price       = min_pos(lowest_full_price),
            lowest_concession_price = min_pos(lowest_concession_price),
            performances = suppressWarnings(max(as.integer(performances), na.rm = TRUE)),
            
            # dates: run window union (earliest start, latest end)
            first_date = suppressWarnings(min(as.Date(first_date), na.rm = TRUE)),
            last_date  = suppressWarnings(max(as.Date(last_date),  na.rm = TRUE)),
            
            # geo: take first usable coordinates (or NA)
            latitude  = suppressWarnings(first(latitude[is.finite(latitude)])),
            longitude = suppressWarnings(first(longitude[is.finite(longitude)])),
            
            .groups = "drop"
          ) %>%
          mutate(
            performances = ifelse(is.infinite(performances), NA_integer_, performances),
            first_date = ifelse(is.infinite(first_date), as.Date(NA), as.Date(first_date)),
            last_date  = ifelse(is.infinite(last_date),  as.Date(NA), as.Date(last_date))
          )
      }
      
      ev %>% arrange(year, code)
    })
    
    output$year_ui <- renderUI({
      yf <- years_tbl()
      req(nrow(yf) > 0)
      yrs <- yf$year
      
      sliderInput(
        session$ns("year_range"),
        label = NULL,
        min = min(yrs),
        max = max(yrs),
        value = c(min(yrs), max(yrs)),
        step = 1,
        sep = ""
      )
    })
    
    output$years_detected <- renderUI({
      yf <- years_tbl()
      if (!nrow(yf)) return(tags$div("None."))
      tags$ul(lapply(seq_len(nrow(yf)), function(i) {
        tags$li(paste0(yf$year[i], " (", basename(yf$path[i]), ")"))
      }))
    })
    
    years_active <- reactive({
      ev <- ev_all(); req(ev)
      yr <- input$year_range
      if (is.null(yr) || length(yr) != 2) return(sort(unique(ev$year)))
      sort(unique(ev$year[ev$year >= yr[1] & ev$year <= yr[2]]))
    })
    
    ev_filtered <- reactive({
      ev <- ev_all(); req(ev)
      yr <- input$year_range
      if (is.null(yr) || length(yr) != 2) return(ev)
      ev %>% filter(year >= yr[1], year <= yr[2])
    })
    
    # share filtered data
    mod_ins_general_server("gen", ev_filtered = ev_filtered)
    mod_ins_price_server("pri", ev_filtered = ev_filtered)
    mod_ins_research_server("res", ev_filtered = ev_filtered)
    
  })
}