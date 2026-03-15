# R/mod_home.R
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(stringr)

# ---------- UI ----------
mod_home_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-wrap",
    layout_columns(
      col_widths = c(2, 10),
      
      # Left sidebar
      div(
        class = "glass side-sticky",
        div(
          class = "glass-inner",
          actionLink(ns("nav_browse"),  "Browse all shows",    class = "side-link"),
          actionLink(ns("nav_map"),     "Plan by map",         class = "side-link"),
          actionLink(ns("nav_methods"), "See stats & methods", class = "side-link")
        )
      ),
      
      # Right main
      uiOutput(ns("home_main"))
    )
  )
}

# ---------- Server ----------
mod_home_server <- function(id, go_tab, reset_trigger = reactive(0), data_dir = "data") {
  moduleServer(id, function(input, output, session) {
    
    # ---- jumps ----
    observeEvent(input$nav_browse,  { go_tab("discovery") }, ignoreInit = TRUE)
    observeEvent(input$go_trending, { go_tab("discovery") }, ignoreInit = TRUE)
    
    observeEvent(input$nav_map, { go_tab("venue") }, ignoreInit = TRUE)
    observeEvent(input$go_hot,  { go_tab("venue") }, ignoreInit = TRUE)
    
    observeEvent(input$nav_methods, { go_tab("insights") }, ignoreInit = TRUE)
    observeEvent(reset_trigger(), { invisible(NULL) }, ignoreInit = TRUE)
    
    # ---- KPI (official + from 2025.xlsx) ----
    kpi <- reactive({
      data_path <- file.path(data_dir, "2025.xlsx")
      if (!file.exists(data_path)) {
        return(list(ok = FALSE, msg = paste0("Missing file: ", data_path)))
      }
      
     
      raw <- readxl::read_excel(data_path, skip = 2)
      
      # required columns in this file (after skip=2)
      need <- c("Code", "Genre", "Country")
      miss <- setdiff(need, names(raw))
      if (length(miss) > 0) {
        return(list(ok = FALSE, msg = paste("2025.xlsx missing columns:", paste(miss, collapse = ", "))))
      }
      
      df <- raw %>%
        transmute(
          Code    = str_squish(as.character(.data[["Code"]])),
          Genre   = str_squish(as.character(.data[["Genre"]])),
          Country = str_squish(as.character(.data[["Country"]]))
        ) %>%
        filter(!is.na(Code), Code != "") %>%
        distinct(Code, .keep_all = TRUE)
      
      # ---- Genres included (your preferred "11" logic):
      # Split "Dance Physical Theatre and Circus" into 2 buckets => +1
      g <- df %>% filter(!is.na(Genre), Genre != "") %>% pull(Genre)
      g_n <- dplyr::n_distinct(g)
      if (any(g == "Dance Physical Theatre and Circus")) g_n <- g_n + 1
      
      # ---- Countries included (your preferred "60" logic):
      # Collapse UK nations into one "UNITED KINGDOM", keep "COUNTRY NOT DECLARED"
      norm_country <- function(x) {
        x <- toupper(str_squish(x))
        if (startsWith(x, "UNITED KINGDOM")) return("UNITED KINGDOM")
        x
      }
      ctry <- df %>%
        filter(!is.na(Country), Country != "") %>%
        mutate(Country2 = vapply(Country, norm_country, character(1))) %>%
        pull(Country2)
      
      c_n <- dplyr::n_distinct(ctry)
      
      # ---- Official headline numbers (Fringe Society wrap-up)
      # Tickets issued: 2,604,404; Performances: 53,942; Shows: 3,893
      list(
        ok = TRUE,
        tickets = "2.6 million",
        total_performances = "53000+",
        registered_shows = 3893,
        genres_included = g_n,
        countries_included = "62"
      )
    })
    
    output$home_main <- renderUI({
      info <- kpi()
      
      tagList(
        div(class = "welcome", "Welcome to FringeFinder!"),
        div(class = "tagline", "Find your 2025 Fringe favourites in seconds."),
        
        # KPI TABLE
        if (!isTRUE(info$ok)) {
          # Error state
          div(class = "glass kpi-board",
              div(class = "glass-inner",
                  div(class = "kpi-title", "Festival at-a-glance"),
                  div(class = "kpi-sub", info$msg)
              )
          )
        } else {
          # Success state
          div(
            class = "kpi-board", 
            
            # The Grid Container
            div(class = "kpi-matrix",
                
                # 1. Ticket Issued (Big Left)
                div(class = "kpi-cell kpi-cell--ticket",
                    div(class = "kpi-label", "TICKET ISSUED"),
                    tags$a(
                      href = "https://www.edfringe.com/about-us/news-and-blog/an-exciting-and-energising-edinburgh-festival-fringe-2025-comes-to-a-close/",
                      target = "_blank",
                      class = "kpi-link",
                      div(class = "kpi-value", info$tickets)
                    )
                ),
                
                # 2. Total Performances (Top Middle)
                div(class = "kpi-cell kpi-cell--tp",
                    div(class = "kpi-label", "TOTAL PERFORMANCES"),
                    div(class = "kpi-value kpi-value--small", format(info$total_performances, big.mark = ","))
                ),
                
                # 3. Total Events (Top Right)
                div(class = "kpi-cell kpi-cell--te",
                    div(class = "kpi-label", "REGISTERED SHOWS"),
                    div(class = "kpi-value kpi-value--small", format(info$registered_shows, big.mark = ","))
                ),
                
                # 4. Genres Included (Bottom Middle)
                div(class = "kpi-cell kpi-cell--gi",
                    div(class = "kpi-label", "GENRES INCLUDED"),
                    div(class = "kpi-value kpi-value--small", format(info$genres_included, big.mark = ","))
                ),
                
                # 5. Countries Included (Bottom Right)
                div(class = "kpi-cell kpi-cell--ci",
                    div(class = "kpi-label", "COUNTRIES INCLUDED"),
                    div(class = "kpi-value kpi-value--small", format(info$countries_included, big.mark = ","))
                )
            )
          )
        },
        
        
        div(
          class = "panel-stack home-stack",
          div(
            class = "hero hero-wrap",
            tags$img(src = "home1.jpg"),
            div(class = "hero-label", "Trending shows in 2025"),
            actionButton(session$ns("go_trending"), label = NULL, class = "hero-click")
          ),
          div(
            class = "hero hero-wrap",
            tags$img(src = "home2.jpg"),
            div(class = "hero-label", "Hot venues to watch in 2025"),
            actionButton(session$ns("go_hot"), label = NULL, class = "hero-click")
          )
        )
      )
    })
  })
}