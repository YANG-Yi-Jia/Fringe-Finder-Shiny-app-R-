# R/mod_discovery.R
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(stringr)
library(DT)

# -------- UI --------
mod_discovery_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-wrap",
    layout_columns(
      col_widths = c(2, 10),
      
      # left panel
      div(
        class = "glass side-sticky",
        div(
          class = "glass-inner",
          
          textInput(ns("q"), label = NULL, placeholder = "Search"),
          
          tags$div(style = "height:10px;"),
          
          
          tags$div(class="filter-title", "Price ▼"),
          radioButtons(
            ns("price"), label = NULL,
            choices = c("Low to high","High to low","Free","0-15","15-30","30+"),
            selected = "Low to high"
          ),
          
          tags$div(style = "height:10px;"),
          
          # Genres
          tags$div(class="filter-title", "Genres ▼"),
          selectizeInput(
            ns("genre"), label = NULL,
            choices = character(0), selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Select genres...")
          ),
          
          tags$div(style = "height:10px;"),
          
          # Tags
          tags$div(class="filter-title", "Tags ▼"),
          selectizeInput(
            ns("tag"), label = NULL,
            choices = character(0), selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Select tags...")
          ),
          
          tags$div(style = "height:10px;"),
          
          # Countries
          tags$div(class="filter-title", "Countries ▼"),
          selectizeInput(
            ns("country"), label = NULL,
            choices = character(0), selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Select countries...")
          ),
          
          tags$div(style = "height:10px;"),
          
          # Ages
          tags$div(class="filter-title", "Ages ▼"),
          selectizeInput(
            ns("age"), label = NULL,
            choices = character(0), selected = character(0),
            multiple = TRUE,
            options = list(placeholder = "Select ages...")
          )
        )
      ),
      
      # right table
      div(
        class = "glass",
        style = "height: 86vh;",
        div(
          class = "glass-inner",
          style = "height: 100%;",
          DTOutput(ns("table"))
        )
      )
    )
  )
}

# -------- Server --------
mod_discovery_server <- function(id, data_path = "2025_discovery_table.xlsx") {
  moduleServer(id, function(input, output, session){
    
    df0 <- reactiveVal(NULL)
    
    observeEvent(TRUE, {
      if (!file.exists(data_path)) stop("Discovery data file not found: ", data_path)
      df <- readxl::read_excel(data_path)
      
      # ---- accept both price column names ----
      price_col <- dplyr::case_when(
        "Lowest full price" %in% names(df) ~ "Lowest full price",
        "Lowest_full_price" %in% names(df) ~ "Lowest_full_price",
        TRUE ~ NA_character_
      )
      
      needed <- c("Code","Title","Artist","Genre","Tag","Venue","Country","Age","Performances","Dates")
      miss <- setdiff(needed, names(df))
      if (length(miss) > 0) stop("Discovery table missing columns: ", paste(miss, collapse = ", "))
      if (is.na(price_col)) stop("Discovery table missing columns: Lowest full price (or Lowest_full_price)")
      
      # Standardise + parse
      df <- df %>%
        transmute(
          Code    = str_squish(as.character(.data[["Code"]])),
          Title   = str_squish(as.character(.data[["Title"]])),
          Artist  = str_squish(as.character(.data[["Artist"]])),
          Genre   = str_squish(as.character(.data[["Genre"]])),
          Tag     = str_squish(as.character(.data[["Tag"]])),
          Venue   = str_squish(as.character(.data[["Venue"]])),
          Country = str_squish(as.character(.data[["Country"]])),
          Age     = str_squish(as.character(.data[["Age"]])),
          Performances = suppressWarnings(as.integer(.data[["Performances"]])),
          Dates = str_squish(as.character(.data[["Dates"]])),
          
          # unify: create a display column named `Lowest full price`
          `Lowest full price` = as.character(.data[[price_col]]),
          price_num = suppressWarnings(as.numeric(gsub("[^0-9.]+", "", as.character(.data[[price_col]]))))
        ) %>%
        filter(!is.na(Code), Code != "") %>%
        distinct(Code, .keep_all = TRUE)
      
      df0(df)
      
      # ---- populate filters from data ----
      genres <- sort(unique(df$Genre[df$Genre != "" & !is.na(df$Genre)]))
      ages   <- sort(unique(df$Age[df$Age != "" & !is.na(df$Age)]))
      countries <- sort(unique(df$Country[df$Country != "" & !is.na(df$Country)]))
      
      tags_vec <- df$Tag[df$Tag != "" & !is.na(df$Tag)]
      tags_vec <- unlist(strsplit(tags_vec, "\\s*,\\s*"))
      tags_vec <- sort(unique(str_squish(tags_vec)))
      tags_vec <- tags_vec[tags_vec != ""]

      updateSelectizeInput(session, "genre",   choices = genres,    server = TRUE)
      updateSelectizeInput(session, "age",     choices = ages,      server = TRUE)
      updateSelectizeInput(session, "country", choices = countries, server = TRUE)
      updateSelectizeInput(session, "tag",     choices = tags_vec,  server = TRUE)
      
    }, once = TRUE)
    
    filtered <- reactive({
      df <- df0()
      req(df)
      
      # 1) search
      q <- str_squish(tolower(if (is.null(input$q)) "" else input$q))
      if (nzchar(q)) {
        df <- df %>%
          filter(
            str_detect(tolower(Title),   fixed(q)) |
              str_detect(tolower(Artist),  fixed(q)) |
              str_detect(tolower(Tag),     fixed(q)) |
              str_detect(tolower(Venue),   fixed(q)) |
              str_detect(tolower(Country), fixed(q))
          )
      }
      
      # 2) Genre
      if (!is.null(input$genre) && length(input$genre) > 0) {
        df <- df %>% filter(Genre %in% input$genre)
      }
      
      # 3) Tags
      if (!is.null(input$tag) && length(input$tag) > 0) {
        sel <- str_replace_all(input$tag, "([\\W])", "\\\\\\1")
        pat <- paste0("(^|,)\\s*(", paste(sel, collapse="|"), ")\\s*(,|$)")
        df <- df %>% filter(str_detect(Tag, regex(pat, ignore_case = TRUE)))
      }
      
      # 4) Country
      if (!is.null(input$country) && length(input$country) > 0) {
        df <- df %>% filter(Country %in% input$country)
      }
      
      # 5) Age
      if (!is.null(input$age) && length(input$age) > 0) {
        df <- df %>% filter(Age %in% input$age)
      }
      
      # 6) Price filter + sort
      price_mode <- if (is.null(input$price)) "Low to high" else input$price
      
      if (price_mode == "Free") {
        df <- df %>% filter(!is.na(price_num), price_num == 0)
      } else if (price_mode == "0-15") {
        df <- df %>% filter(!is.na(price_num), price_num >= 0, price_num <= 15)
      } else if (price_mode == "15-30") {
        df <- df %>% filter(!is.na(price_num), price_num > 15, price_num <= 30)
      } else if (price_mode == "30+") {
        df <- df %>% filter(!is.na(price_num), price_num > 30)
      }
      
      if (price_mode == "Low to high") {
        df <- df %>% arrange(price_num)
      } else if (price_mode == "High to low") {
        df <- df %>% arrange(desc(price_num))
      }
      
      df
    })
    
    output$table <- renderDT({
      df <- filtered()
      req(df)
      
      show_df <- df %>%
        select(
          Title, Artist, Genre, Tag, Venue, Country,
          `Lowest full price`,
          Age, Performances,
          Dates
        )
      
      DT::datatable(
        show_df,
        rownames = FALSE,
        escape = TRUE,
        options = list(
          pageLength = 50,
          lengthMenu = c(20, 50, 100, 200),
          searching = FALSE,
          info = TRUE,
          paging = TRUE,
          scrollX = TRUE,
          scrollY = "85vh",
          dom = "tip"
        )
      )
    })
  })
}