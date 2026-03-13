# app.R

library(shiny)
library(bslib)

# Modules
source("R/mod_home.R")
source("R/mod_discovery.R")
source("R/mod_venue.R")
source("R/mod_insights.R")
source("R/mod_summary.R")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  page_navbar(
    title = tags$span(class = "brand", "Fringe Finder"),
    id    = "topnav",
    theme = bs_theme(version = 5),
    
    nav_panel("HOME", value = "home", mod_home_ui("home")),
    nav_panel("Discovery", value = "discovery", mod_discovery_ui("disc")),
    nav_panel("Venue", value = "venue", mod_venue_ui("ven")),
    nav_panel("Fringe Insights 2022-2025", value = "insights", mod_insights_ui("ins")),
    nav_panel("SUMMARY", value = "summary", mod_summary_ui("summary")),
    nav_spacer()
  )
)

server <- function(input, output, session) {
  
  app_dir  <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  data_dir <- file.path(app_dir, "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # data
  home_2025_path   <- file.path(data_dir, "2025.xlsx")
  discovery_path   <- file.path(data_dir, "2025_discovery_table.xlsx")
  venue_path       <- file.path(data_dir, "venue_table.xlsx")
  events_cleanpath <- file.path(data_dir, "events_2022_2025_clean.xlsx")
  
  required_files <- c(home_2025_path, discovery_path, venue_path, events_cleanpath)
  #stop if failed
  observeEvent(TRUE, {
    missing <- required_files[!file.exists(required_files)]
    if (length(missing) > 0) {
      showModal(modalDialog(
        title = "Missing data files in /data",
        tags$pre(
          style = "white-space:pre-wrap;",
          paste0(
            "These files were not found:\n\n",
            paste(missing, collapse = "\n"),
            "\n\nFix: put the files under:\n",
            data_dir,
            "\n\nThen restart the app."
          )
        ),
        easyClose = FALSE,
        footer = tagList(
          modalButton("OK"),
          actionButton("stop_now", "Stop app")
        )
      ))
    }
  }, once = TRUE)
  
  observeEvent(input$stop_now, {
    stopApp()
  }, ignoreInit = TRUE)
  
  # If any required file missing, do not start modules
  if (any(!file.exists(required_files))) return()
  
  # HOME reset
  home_reset_counter <- reactiveVal(0)
  home_reset_trigger <- reactive(home_reset_counter())
  
  observeEvent(input$go_home, {
    bslib::nav_select(id = "topnav", selected = "home", session = session)
    home_reset_counter(home_reset_counter() + 1)
  }, ignoreInit = TRUE)
  
  go_tab <- function(target) {
    bslib::nav_select(id = "topnav", selected = target, session = session)
  }
  
  # start
  tryCatch(
    mod_home_server(
      "home",
      go_tab = go_tab,
      reset_trigger = home_reset_trigger,
      data_dir = data_dir
    ),
    error = function(e) showNotification(
      paste("HOME module error:", e$message),
      type = "error", duration = NULL
    )
  )
  
  tryCatch(
    mod_discovery_server("disc", data_path = discovery_path),
    error = function(e) showNotification(
      paste("Discovery module error:", e$message),
      type = "error", duration = NULL
    )
  )
  
  tryCatch(
    mod_venue_server("ven", data_path = venue_path),
    error = function(e) showNotification(
      paste("Venue module error:", e$message),
      type = "error", duration = NULL
    )
  )
  
  tryCatch(
    mod_insights_server(
      "ins",
      data_dir = data_dir,
      skip = 2
    ),
    error = function(e) showNotification(
      paste("Insights module error:", e$message),
      type = "error", duration = NULL
    )
  )
  

  tryCatch(
    mod_summary_server("summary"),
    error = function(e) showNotification(
      paste("SUMMARY module error:", e$message),
      type = "error", duration = NULL
    )
  )
}

shinyApp(ui = ui, server = server)