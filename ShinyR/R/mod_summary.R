# R/mod_summary.R
library(shiny)
library(bslib)
library(htmltools)

mod_summary_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "page-wrap",
    layout_columns(
      col_widths = c(3, 9),
      
      div(
        class = "glass side-sticky",
        div(
          class = "glass-inner",
          tags$h3("Summary"),
          tags$p(class = "venue-help-highlight",
                 "Data provenance, reproducibility notes, media credits, and downloads."),
          tags$hr(),
          tags$ul(
            class = "list-unstyled",
            tags$li(tags$a(href = "#data",      "1) Data source")),
            tags$li(tags$a(href = "#coverage",  "2) Coverage & definitions")),
            tags$li(tags$a(href = "#plots",     "3) Figures & workflow")),
            tags$li(tags$a(href = "#gallery",   "4) Plot gallery")),
            tags$li(tags$a(href = "#media",     "5) Images & credits")),
            tags$li(tags$a(href = "#downloads", "6) Downloads")),
            tags$li(tags$a(href = "#license",   "7) Attribution & license"))
          )
        )
      ),
      
      div(
        class = "glass",
        div(
          class = "glass-inner",
          
          tags$h2("Project Summary"),
          tags$hr(),
          
          tags$h3(id = "data", "1) Data source"),
          tags$p(
            "The raw Fringe datasets (2022–2025) are official XLSX exports downloaded via the Edinburgh Festival City (Fringe) API. ",
            "Access was obtained by contacting the API administrators to request an API key and to confirm the official license agreement. ",
            "API documentation: ",
            tags$a(
              href = "https://api.edinburghfestivalcity.com/documentation",
              target = "_blank",
              "https://api.edinburghfestivalcity.com/documentation"
            ),
            "."
          ),
          
          tags$h3(id = "coverage", "2) Coverage & definitions"),
          tags$ul(
            tags$li(tags$b("Coverage: "), "This project focuses on 2022–2025 to keep venue-based statistics consistent and comparable."),
            tags$li(tags$b("Scope note: "), "2021 contains a high share of online-only entries during COVID-era disruptions, which can bias venue summaries."),
            tags$li(tags$b("Unit of analysis: "), "Event-level records (with identifiers such as event code, venue code, dates, performances, and prices)."),
            tags$li(tags$b("Typical cleaning decisions: "),
                    "standardising text fields (trim/squish), fixing website URLs, handling missing postcodes/lat-long, and deduplicating by event code where appropriate.")
          ),
          
          tags$hr(),
          
          tags$h3(id = "plots", "3) Figures & workflow"),
          tags$p(
            "All charts were generated in R by reading the official XLSX files, cleaning and standardising key fields (e.g., venue codes, postcodes, coordinates, dates, prices), ",
            "and then producing statistical graphics using ggplot2. The resulting plots are integrated into the Shiny website UI alongside interactive tables and maps."
          ),
          
          tags$hr(),
          
          tags$h3(id = "gallery", "4) Plot gallery"),
          tags$p(
            "The plots below are static ggplot figures exported during the pre-build analysis stage (before integrating the views into the Shiny app). ",
            "They are loaded from www/fig/ (png/jpg/jpeg) and displayed in a fixed order."
          ),
          uiOutput(ns("gallery_ui")),
          
          tags$hr(),
          
          tags$h3(id = "media", "5) Images & credits"),
          tags$ul(
            tags$li(
              tags$b("Background images: "),
              "All background photos used across pages (Home / Discovery / Venue / Insights / Summary) were taken by Yijia Yang (© personal photography). ",
              "Not for reuse without permission."
            ),
            tags$li(
              tags$b("Homepage tile images: "),
              "home1.jpg source: ",
              tags$a(
                href = "https://tse3.mm.bing.net/th/id/OIP.MrDJO6HtkpikzPcRabZ7egHaFy?pid=ImgDet&w=177&h=138&c=7&dpr=2&o=7&rm=3",
                target = "_blank",
                "Bing-hosted image"
              ),
              "; home2.jpg source: ",
              tags$a(
                href = "https://i.etsystatic.com/5502210/r/il/2fa535/2070236126/il_1140xN.2070236126_tgjv.jpg",
                target = "_blank",
                "Etsy static image"
              ),
              ". Third-party images are used for on-page display with attribution."
            ),
            tags$li(
              tags$b("Map tiles: "),
              "Interactive maps use OpenStreetMap base tiles (OpenStreetMap.Mapnik) via leaflet."
            ),
            tags$li(
              tags$b("Reuse note: "),
              "Third-party images are credited above and are not redistributed outside the website context unless explicitly permitted by the original publisher."
            )
          ),
          
          tags$hr(),
          
          tags$h3(id = "downloads", "6) Downloads"),
          tags$p("Download the XLSX tables used in the website."),
          
          tags$div(
            class = "mt-2",
            tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
                     downloadButton(ns("dl_discovery"), "Download 2025_discovery_table.xlsx", class = "btn btn-outline-light"),
                     downloadButton(ns("dl_venue"), "Download venue_table.xlsx", class = "btn btn-outline-light"),
                     downloadButton(ns("dl_events_clean"), "Download events_2022_2025_clean.xlsx", class = "btn btn-outline-light")
            ),
            uiOutput(ns("dl_meta_ui"))
          ),
          
          tags$hr(),
          
          tags$h3(id = "license", "7) Attribution & license"),
          tags$ul(
            tags$li(tags$b("Code license: "), "MIT License (project code)."),
            tags$li(
              tags$b("Data attribution: "),
              "Edinburgh Festival City / Fringe API official exports (2022–2025). Use is permitted under the provider terms (e.g., “Permitted use for all use types”) and requires a registered API key."
            ),
            tags$li(
              tags$b("API compliance notes: "),
              "Applications should check for updates from the API at least every 24 hours, and Fringe show links should deep-link to the corresponding edfringe.com page rather than alternative ticketing sites."
            ),
            tags$li(
              tags$b("Downloads reuse: "),
              "Curated XLSX tables under data/ are shareable with attribution to Yijia Yang; they are derived from the official exports with cleaning and schema stabilisation."
            ),
            tags$li(
              tags$b("Media attribution: "),
              "Third-party homepage images are credited in section 5; background photography is © personal photography (Yijia Yang)."
            ),
            tags$li(
              tags$b("Map tiles attribution: "),
              "Basemap tiles © OpenStreetMap contributors (served via leaflet provider tiles)."
            ),
            tags$li(
              tags$b("Disclaimer: "),
              "This website is an academic project showcase and is not an official product of the Fringe / Edinburgh Festival City API provider."
            ),
            tags$li(tags$b("Last updated: "), format(Sys.Date(), "%Y-%m-%d"))
          )
        )
      )
    )
  )
}

mod_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$gallery_ui <- renderUI({
      fig_dir <- file.path("www", "fig")
      if (!dir.exists(fig_dir)) {
        return(div(class = "text-muted",
                   "Folder not found: www/fig/. Create it and add plot images (png/jpg/jpeg)."))
      }
      
      want <- c(
        "fig4_genre_price_boxplots",
        "fig4_concessions_by_genre_year",
        "fig4_hhi_trend",
        "fig4_lorenz_curves",
        "fig4_price_trend_median_iqr"
      )
      
      files_all <- list.files(
        fig_dir,
        pattern = "\\.(png|jpg|jpeg)$",
        ignore.case = TRUE,
        full.names = FALSE
      )
      
      if (length(files_all) == 0) {
        return(div(
          class = "text-muted",
          tags$p("No images found yet."),
          tags$ol(
            tags$li("Create folder: www/fig/"),
            tags$li("Export ggplot figures as PNG/JPG into that folder"),
            tags$li("Reload the app — images will appear here automatically")
          )
        ))
      }
      
      stems <- tools::file_path_sans_ext(files_all)
      keep_idx <- match(want, stems)
      present <- !is.na(keep_idx)
      ordered_files <- files_all[keep_idx[present]]
      
      missing <- want[!present]
      if (length(ordered_files) == 0) {
        return(div(
          class = "text-muted",
          tags$p("None of the expected gallery figures were found in www/fig/."),
          tags$p("Expected file stems:"),
          tags$ul(lapply(want, tags$li))
        ))
      }
      
      tags$div(
        if (length(missing) > 0) {
          div(
            class = "text-muted",
            style = "margin-bottom:10px;",
            tags$b("Missing figures: "),
            paste(missing, collapse = ", "),
            ". Please add them to www/fig/."
          )
        },
        style = "display:flex; gap:14px; flex-wrap:wrap; align-items:flex-start;",
        lapply(ordered_files, function(f) {
          tags$div(
            style = "width: 280px;",
            tags$img(
              src = file.path("fig", f),
              loading = "lazy",
              decoding = "async",
              style = "width:100%; border-radius:16px; border:1px solid rgba(255,255,255,0.25);"
            ),
            tags$div(class = "text-muted", style = "margin-top:6px;", f)
          )
        })
      )
    })
    
    FILES <- list(
      discovery = list(
        filename = "2025_discovery_table.xlsx",
        abs_path = "/Users/yangyijia/Desktop/project math double/ShinyR/data/2025_discovery_table.xlsx",
        rel_path = file.path("data", "2025_discovery_table.xlsx")
      ),
      venue = list(
        filename = "venue_table.xlsx",
        abs_path = "/Users/yangyijia/Desktop/project math double/ShinyR/data/venue_table.xlsx",
        rel_path = file.path("data", "venue_table.xlsx")
      ),
      events_clean = list(
        filename = "events_2022_2025_clean.xlsx",
        abs_path = "/Users/yangyijia/Desktop/project math double/ShinyR/data/events_2022_2025_clean.xlsx",
        rel_path = file.path("data", "events_2022_2025_clean.xlsx")
      )
    )
    
    pick_src <- function(x) {
      if (file.exists(x$abs_path)) return(x$abs_path)
      if (file.exists(x$rel_path)) return(x$rel_path)
      NA_character_
    }
    
    fmt_bytes <- function(n) {
      if (is.na(n)) return(NA_character_)
      units <- c("B","KB","MB","GB","TB")
      i <- 1
      x <- as.numeric(n)
      while (x >= 1024 && i < length(units)) { x <- x/1024; i <- i + 1 }
      sprintf("%.1f %s", x, units[i])
    }
    
    file_meta <- function(spec) {
      src <- pick_src(spec)
      if (is.na(src)) {
        return(list(
          ok = FALSE,
          text = paste0("Missing: ", spec$filename,
                        " (place it under ./data/ for deployment, or set a valid local abs_path for development).")
        ))
      }
      info <- file.info(src)
      list(
        ok = TRUE,
        text = paste0(
          spec$filename, " — ",
          fmt_bytes(info$size), " — last modified ",
          format(info$mtime, "%Y-%m-%d %H:%M")
        )
      )
    }
    
    output$dl_meta_ui <- renderUI({
      metas <- list(
        discovery    = file_meta(FILES$discovery),
        venue        = file_meta(FILES$venue),
        events_clean = file_meta(FILES$events_clean)
      )
      
      tags$div(
        style = "margin-top:10px;",
        lapply(metas, function(m) {
          if (isTRUE(m$ok)) {
            tags$div(class = "text-muted", style = "font-size: 12px; line-height: 1.35;", m$text)
          } else {
            tags$div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; opacity: 0.9;",
              tags$span(style="color: #ffd27d; font-weight:600;", "Note: "),
              m$text
            )
          }
        })
      )
    })
    
    make_dl <- function(btn_id, spec) {
      output[[btn_id]] <- downloadHandler(
        filename = function() spec$filename,
        content = function(file) {
          src <- pick_src(spec)
          validate(need(
            !is.na(src),
            paste0(
              "File not found: ", spec$filename, ". ",
              "For deployment, place it under ./data/. ",
              "Tried: ", spec$abs_path, " and ", spec$rel_path
            )
          ))
          file.copy(src, file, overwrite = TRUE)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
    
    make_dl("dl_discovery", FILES$discovery)
    make_dl("dl_venue", FILES$venue)
    make_dl("dl_events_clean", FILES$events_clean)
  })
}