# R/mod_ins_price.R  (OPTIMIZED COVER VERSION — + highlight cards, responsive plotly)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)
library(scales)
library(htmlwidgets)

mod_ins_price_ui <- function(id) {
  ns <- NS(id)
  
  hi_card <- function(output_id) {
    div(
      class = "highlight-card",
      style = paste0(
        "margin: 10px 0 12px 0; padding: 12px 14px; border-radius: 14px;",
        "background: rgba(255,255,255,0.06); border: 1px solid rgba(255,255,255,0.10);"
      ),
      tags$div("Highlights", style = "font-weight: 700; margin-bottom: 6px; opacity: 0.95;"),
      htmlOutput(ns(output_id))
    )
  }
  
  tagList(
    div(
      style = "display:flex; flex-direction:column; gap:18px;",
      
      # 1) Genre price distribution
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Genre × ticket price distribution", style = "margin: 0 0 6px 0;"),
              tags$p(
                "Each panel is a genre. Within each panel, boxplots show the distribution of lowest full ticket prices by year (2022–2025), enabling cross-year comparison within the same genre.",
                style = "margin: 0 0 10px 0; opacity: 0.85;"
              ),
              plotlyOutput(ns("p_genre_price_box"), height = "980px"),
              hi_card("hi_genre_price"),
              tags$p(
                "Each facet fixes the genre and compares yearly price distributions (median line, IQR box, whiskers and outliers) across 2022–2025 on the same x-axis. Pattern: upward shifts in the median across years indicate within-genre price inflation, while wider IQRs suggest greater heterogeneity in entry-level pricing. Interpretation: Genres are ordered by their overall median price (high->low), so you can quickly distinguish consistently high-price genres from those whose medians change most over time, without being dominated by a small number of extreme listings.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 2) Age × Genre × Year
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Age × genre × year (median full price)", style = "margin: 0 0 6px 0;"),
              tags$p("Top 10 genres. Bars show the median lowest full price by age band within genre (event-level, deduped by year × code).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_age_genre_year"), height = "780px"),
              hi_card("hi_age_genre_year"),
              tags$p(
                "For the top genres, bars show the median lowest full price within each age band, split by year. Pattern: Within a genre, differences across age bands reflect audience targeting; across years, shifts capture broad price movement within comparable segments. Interpretation: The median is used because prices are skewed and may include premium outliers; it better represents a typical entry price. Limiting to the top 10 genres improves robustness by keeping cell counts larger and reducing noise from sparsely observed genre–age combinations.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 3) Venue offer rate (line)
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Top 10 venues: concession offer rate", style = "margin: 0 0 6px 0;"),
              tags$p("Offer rate = share of events with a valid concession price strictly below full price (event-level, deduped by year × code).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_venue_offer_rate"), height = "700px"),
              hi_card("hi_venue_offer"),
              tags$p(
                "Each line tracks a high-traffic venue’s concession offer rate over time. Pattern: Venue-to-venue separation indicates persistent policy differences, while slope changes show whether concession availability is expanding or contracting at specific sites. Interpretation: This is an availability metric (whether concessions exist), not discount depth; focusing on the top 10 venues stabilises the comparison because rates from tiny event counts are volatile.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 4) Genre concessions (line)
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Concessions by genre and year", style = "margin: 0 0 6px 0;"),
              tags$p("Two trends per genre: offer rate (prevalence) and average discount (depth conditional on concessions existing).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_genre_concessions"), height = "980px"),
              hi_card("hi_genre_concessions"),
              tags$p(
                "Two metrics are shown as line trends: offer rate (share of events with conc < full) and average discount (mean 1 − conc/full among events offering concessions). Pattern: A genre can have frequent but shallow concessions, or rare but generous ones, so the two panels need not move together. Interpretation: Separating prevalence from depth avoids conflating ‘availability’ with ‘generosity’, and the legend interaction lets you isolate a subset of genres when the plot is dense.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      )
    )
  )
}

mod_ins_price_server <- function(
    id,
    ev_filtered,
    years_vec = 2022:2025,
    top_genres_k = 12
) {
  moduleServer(id, function(input, output, session) {
    
    need_cols <- function(df, cols) {
      miss <- setdiff(cols, names(df))
      validate(need(length(miss) == 0, paste0("Missing column(s): ", paste(miss, collapse = ", "))))
      TRUE
    }
    
    # ---- small helpers (fast + stable) ----
    to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]+", "", as.character(x))))
    first_nonblank <- function(x) {
      x <- as.character(x)
      x <- x[!is.na(x) & str_trim(x) != "" & !x %in% c("NA","N/A")]
      if (length(x)) x[1] else NA_character_
    }
    min_pos <- function(x) { x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x) & x > 0]; if (length(x)) min(x) else NA_real_ }
    min_nonneg <- function(x) { x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x) & x >= 0]; if (length(x)) min(x) else NA_real_ }
    
    bullets_ui <- function(items) {
      items <- items[!is.na(items) & nzchar(items)]
      if (!length(items)) return(HTML("<div style='opacity:0.85;'>No clear highlights detected for the selected years.</div>"))
      HTML(paste0("<ul style='margin:0; padding-left:18px; opacity:0.92;'>",
                  paste0("<li>", items, "</li>", collapse = ""), "</ul>"))
    }
    
    pp <- function(x, digits = 1) {
      if (!is.finite(x)) return("NA")
      x2 <- round(x, 6) # 防浮点误差
      paste0(ifelse(x2 >= 0, "+", ""), round(100 * x2, digits), " pp")
    }
    
    # ---- years active (from ev_filtered slider) ----
    years_active <- reactive({
      ev <- ev_filtered(); req(ev)
      ys <- sort(unique(as.integer(ev$year)))
      ys <- ys[!is.na(ys)]
      if (length(ys)) ys else years_vec
    })
    
    # =========================================================
    # Chart 1 (boxplots): base once
    # =========================================================
    genre_price_year_data <- reactive({
      ev <- ev_filtered(); req(ev)
      need_cols(ev, c("year", "lowest_full_price", "genre"))
      ys <- years_active()
      
      df <- ev %>%
        transmute(
          year  = as.integer(year),
          genre = str_squish(as.character(genre)),
          price = to_num(lowest_full_price)
        ) %>%
        filter(year %in% ys, is.finite(price), price > 0, !is.na(genre), genre != "")
      
      df2 <- df %>%
        mutate(yearF = factor(year, levels = ys))
      
      genre_levels <- df2 %>%
        group_by(genre) %>%
        summarise(med_all = median(price, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(med_all), genre) %>%
        pull(genre)
      
      df2 %>%
        mutate(
          genreF = factor(genre, levels = genre_levels),
          tooltip = paste0(
            "Genre: ", genre,
            "<br>Year: ", year,
            "<br>Price: £", sprintf("%.2f", price)
          )
        )
    })
    
    output$p_genre_price_box <- renderPlotly({
      df <- genre_price_year_data(); req(df)
      validate(need(nrow(df) > 0, "No price data for the selected years."))
      
      p <- ggplot(
        df,
        aes(
          x = yearF, y = price,
          group = yearF,
          fill = yearF
        )
      ) +
        
        geom_boxplot(
          width = 0.55,
          linewidth = 0.45,
          outlier.shape = 16,
          outlier.size  = 0.20,
          outlier.stroke = 0,
          outlier.alpha = 0.55,
          na.rm = TRUE
        ) +
        
        stat_summary(
          fun = median,
          geom = "point",
          size = 0.5,
          stroke = 0.35,
          shape = 21,
          fill = "red",
          na.rm = TRUE
        ) +
       
        geom_text(aes(label = tooltip), alpha = 0, size = 0, na.rm = TRUE) +
        facet_wrap(~ genreF, ncol = 2, scales = "free_y") +
        scale_x_discrete(drop = FALSE) +
        labs(x = NULL, y = "Price (GBP)") +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "none",
          plot.margin = margin(8, 10, 8, 10, "pt")
        )
      
      ggplotly(p, tooltip = "label") %>%  
        layout(
          showlegend = FALSE,
          margin = list(l = 70, r = 30, t = 25, b = 55),
          autosize = TRUE
        ) %>%
        config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })
    
    
    # =========================================================
    # Chart 2 (age bars): aggregate once
    # =========================================================
    age_levels <- c("0+","3+","5+","8+","12+","14+","16+","18+")
    
    age_genre_year_medians <- reactive({
      ev <- ev_filtered(); req(ev)
      need_cols(ev, c("year", "code", "genre", "age", "lowest_full_price"))
      ys <- years_active()
      
      base <- ev %>%
        transmute(
          Year  = as.integer(year),
          Code  = str_squish(as.character(code)),
          Genre = str_squish(as.character(genre)),
          Age   = str_squish(as.character(age)),
          full  = to_num(lowest_full_price)
        ) %>%
        filter(Year %in% ys, !is.na(Code), Code != "",
               !is.na(Genre), Genre != "",
               !is.na(Age), Age %in% age_levels,
               is.finite(full), full > 0) %>%
        group_by(Year, Code) %>%
        summarise(
          Genre = first_nonblank(Genre),
          Age   = first_nonblank(Age),
          full  = min_pos(full),
          .groups = "drop"
        )
      
      top_genres <- base %>% count(Genre, sort = TRUE) %>% slice_head(n = 10) %>% pull(Genre)
      
      med_yga <- base %>%
        filter(Genre %in% top_genres) %>%
        mutate(YearF = factor(Year, levels = ys), AgeF = factor(Age, levels = age_levels)) %>%
        group_by(YearF, Genre, AgeF) %>%
        summarise(median_full = median(full, na.rm = TRUE), .groups = "drop")
      
      last_y <- max(ys)
      genre_levels <- med_yga %>%
        filter(YearF == as.character(last_y)) %>%
        group_by(Genre) %>%
        summarise(m = median(median_full, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(m), Genre) %>%
        pull(Genre)
      
      if (!length(genre_levels)) {
        genre_levels <- med_yga %>%
          group_by(Genre) %>%
          summarise(m = median(median_full, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(m), Genre) %>%
          pull(Genre)
      }
      
      med_yga %>%
        mutate(GenreF = factor(Genre, levels = rev(genre_levels)))
    })
    
    output$p_age_genre_year <- renderPlotly({
      d <- age_genre_year_medians(); req(d)
      ys <- years_active()
      
      d <- d %>%
        mutate(
          hover = paste0("Year: ", as.character(YearF),
                         "<br>Genre: ", as.character(GenreF),
                         "<br>Age: ", as.character(AgeF),
                         "<br>Median full price: £", sprintf("%.2f", median_full))
        )
      
      p <- ggplot(d, aes(x = median_full, y = GenreF, fill = AgeF, text = hover)) +
        geom_col(position = position_dodge(width = 0.78), width = 0.72, na.rm = TRUE) +
        facet_wrap(~ YearF, nrow = 1) +
        scale_x_continuous(labels = scales::number_format(accuracy = 1),
                           expand = expansion(mult = c(0, 0.22))) +
        labs(x = "Median price (GBP)", y = "Genre", fill = "Age") +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "right",
          plot.margin = margin(16, 34, 16, 22, "pt")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 220, r = 70, t = 20, b = 90), autosize = TRUE) %>%
        config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })
    
    # =========================================================
    # Shared concession base (ONE time)
    # =========================================================
    conc_event_base <- reactive({
      ev <- ev_filtered(); req(ev)
      need_cols(ev, c("year", "code", "venue", "venue_code", "genre",
                      "lowest_full_price", "lowest_concession_price"))
      ys <- years_active()
      
      ev %>%
        transmute(
          Year  = as.integer(year),
          Code  = str_squish(as.character(code)),
          Venue = str_squish(as.character(venue)),
          VCode = str_squish(as.character(venue_code)),
          Genre = str_squish(as.character(genre)),
          full  = to_num(lowest_full_price),
          conc  = to_num(lowest_concession_price)
        ) %>%
        filter(Year %in% ys, !is.na(Code), Code != "",
               !is.na(VCode), VCode != "",
               !is.na(Venue), Venue != "", !Venue %in% c("NA","N/A")) %>%
        group_by(Year, Code) %>%
        summarise(
          Venue = first_nonblank(Venue),
          VCode = first_nonblank(VCode),
          Genre = first_nonblank(Genre),
          full  = min_pos(full),
          conc  = min_nonneg(conc),
          .groups = "drop"
        ) %>%
        mutate(
          has_conc_strict = is.finite(full) & full > 0 & is.finite(conc) & conc >= 0 & conc < full,
          discount = if_else(has_conc_strict, 1 - conc / full, NA_real_)
        )
    })
    
    # =========================================================
    # Chart 3: venue offer rate (line guarded)
    # =========================================================
    venue_offer <- reactive({
      df <- conc_event_base(); req(df)
      ys <- years_active()
      
      top_vcodes <- df %>%
        count(VCode, name = "events") %>%
        arrange(desc(events), VCode) %>%
        slice_head(n = 10) %>%
        pull(VCode)
      
      out <- df %>%
        filter(VCode %in% top_vcodes) %>%
        group_by(Year, VCode) %>%
        summarise(
          Venue      = first_nonblank(Venue),
          n_events   = n(),
          offer_rate = mean(coalesce(has_conc_strict, FALSE)),
          .groups = "drop"
        )
      
      last_y <- max(ys)
      rank_key <- out %>%
        group_by(VCode) %>%
        summarise(avg = mean(offer_rate, na.rm = TRUE), last = max(offer_rate[Year == last_y], na.rm = TRUE), .groups="drop") %>%
        mutate(last = ifelse(is.infinite(last), NA_real_, last),
               rank_value = ifelse(is.na(last), avg, last)) %>%
        arrange(desc(rank_value), VCode)
      
      ordered_vcodes <- rank_key$VCode
      
      label_map <- out %>%
        distinct(VCode, Venue) %>%
        mutate(Venue = ifelse(is.na(Venue) | Venue == "" | Venue %in% c("NA","N/A"), as.character(VCode), Venue))
      labels_vec <- setNames(label_map$Venue, label_map$VCode)
      
      out %>%
        mutate(
          YearN = as.integer(Year),
          VenueLabel = factor(labels_vec[as.character(VCode)], levels = labels_vec[ordered_vcodes])
        ) %>%
        arrange(YearN, VenueLabel)
    })
    
    output$p_venue_offer_rate <- renderPlotly({
      d <- venue_offer(); req(d)
      ys <- years_active()
      
      d <- d %>%
        mutate(hover = paste0("Year: ", YearN,
                              "<br>Venue: ", as.character(VenueLabel),
                              "<br>Offer rate: ", scales::percent(offer_rate, accuracy = 1),
                              "<br>Unique events: ", n_events))
      
      only_one_year <- length(unique(d$YearN)) < 2
      
      p <- ggplot(d, aes(x = YearN, y = offer_rate, color = VenueLabel, group = VenueLabel, text = hover)) +
        { if (!only_one_year) geom_line(linewidth = 0.9, alpha = 0.95, na.rm = TRUE) } +
        geom_point(size = 2.2, na.rm = TRUE) +
        scale_x_continuous(breaks = ys, limits = range(ys, na.rm = TRUE),
                           expand = expansion(mult = c(0.02, 0.08))) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           expand = expansion(mult = c(0.02, 0.05))) +
        labs(x = "Year", y = "Offer rate", color = "Venue") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "right",
          legend.title = element_text(face = "bold"),
          plot.margin = margin(16, 28, 16, 18, "pt")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 90, r = 220, t = 20, b = 70),
          legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top"),
          autosize = TRUE
        ) %>%
        config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })
    
    # =========================================================
    # Chart 4: genre concessions (line guarded)
    # =========================================================
    genre_concessions_lines <- reactive({
      df <- conc_event_base(); req(df)
      ys <- years_active()
      last_y <- max(ys)
      
      offer_rate <- df %>%
        filter(!is.na(Genre), Genre != "") %>%
        group_by(Year, Genre) %>%
        summarise(n_events = n(), offer_rate = mean(coalesce(has_conc_strict, FALSE)), .groups = "drop")
      
      depth_mean <- df %>%
        filter(!is.na(Genre), Genre != "", has_conc_strict) %>%
        group_by(Year, Genre) %>%
        summarise(n_conc = n(), mean_disc = mean(discount, na.rm = TRUE), .groups = "drop")
      
      ordering <- offer_rate %>%
        filter(Year == last_y) %>%
        arrange(desc(offer_rate), Genre) %>%
        pull(Genre) %>%
        unique()
      
      if (!length(ordering)) {
        ordering <- offer_rate %>%
          group_by(Genre) %>%
          summarise(avg = mean(offer_rate, na.rm = TRUE), .groups="drop") %>%
          arrange(desc(avg), Genre) %>%
          pull(Genre)
      }
      
      bind_rows(
        offer_rate %>% transmute(Year, Genre, metric = "Offer rate", value = offer_rate, n_base = n_events),
        depth_mean %>% transmute(Year, Genre, metric = "Average discount", value = mean_disc, n_base = n_conc)
      ) %>%
        complete(Year = ys, Genre = ordering, metric = c("Offer rate", "Average discount")) %>%
        mutate(
          YearN = as.integer(Year),
          GenreF = factor(Genre, levels = ordering),
          metricF = factor(metric, levels = c("Offer rate", "Average discount")),
          hover = paste0(
            "Metric: ", metric,
            "<br>Year: ", YearN,
            "<br>Genre: ", as.character(GenreF),
            "<br>Value: ", ifelse(is.na(value), "NA", scales::percent(value, accuracy = 1)),
            "<br>N: ", ifelse(is.na(n_base), "NA", n_base)
          )
        ) %>%
        arrange(metricF, GenreF, YearN)
    })
    
    output$p_genre_concessions <- renderPlotly({
      d <- genre_concessions_lines(); req(d)
      ys <- years_active()
      only_one_year <- length(unique(d$YearN)) < 2
      
      p <- ggplot(d, aes(x = YearN, y = value, color = GenreF, group = GenreF, text = hover)) +
        { if (!only_one_year) geom_line(linewidth = 0.9, alpha = 0.95, na.rm = TRUE) } +
        geom_point(size = 2.1, na.rm = TRUE) +
        facet_wrap(~ metricF, ncol = 1) +
        scale_x_continuous(breaks = ys, limits = range(ys, na.rm = TRUE),
                           expand = expansion(mult = c(0.02, 0.08))) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           expand = expansion(mult = c(0.02, 0.06))) +
        labs(x = "Year", y = NULL, color = "Genre") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "right",
          legend.title = element_text(face = "bold"),
          plot.margin = margin(14, 28, 14, 18, "pt")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 90, r = 260, t = 20, b = 70),
          legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top"),
          autosize = TRUE
        ) %>%
        config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })
    
    # =========================================================
    # Highlights (reuse aggregated data; more robust)
    # =========================================================
    output$hi_genre_price <- renderUI({
      df <- genre_price_year_data(); req(df)
      ys <- years_active()
      if (length(ys) < 1) return(bullets_ui("No data for selected years."))
      
      y0 <- min(ys); y1 <- max(ys)
      
      # yearly medians by genre (consistent with boxplot median)
      med_by <- df %>%
        group_by(year, genreF) %>%
        summarise(med = median(price, na.rm = TRUE), .groups = "drop")
      
      # top 2 priciest genres in the latest year (y1)
      med_y1 <- med_by %>% filter(year == y1) %>% arrange(desc(med))
      top2 <- med_y1 %>% slice_head(n = 2)
      
      # compute change in medians between y0 and y1 (only for genres observed in both)
      wide <- med_by %>%
        filter(year %in% c(y0, y1)) %>%
        pivot_wider(names_from = year, values_from = med)
      
      if (!(as.character(y0) %in% names(wide)) || !(as.character(y1) %in% names(wide))) {
        return(bullets_ui("Not enough overlapping data to compute changes."))
      }
      
      wide2 <- wide %>%
        filter(is.finite(.data[[as.character(y0)]]) & is.finite(.data[[as.character(y1)]])) %>%
        mutate(change = round(.data[[as.character(y1)]] - .data[[as.character(y0)]], 6))
      
      if (nrow(wide2) == 0) {
        return(bullets_ui("No genres have data in both the first and last selected years."))
      }
      
      up <- wide2 %>% arrange(desc(change)) %>% slice_head(n = 1)
      dn <- wide2 %>% arrange(change) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>", y1, " highest-median genres:</b> ",
               paste0(as.character(top2$genreF), " (£", sprintf("%.1f", top2$med), ")", collapse = ", "), "."),
        paste0("<b>Largest median rise (", y0, "->", y1, "):</b> ", as.character(up$genreF),
               " (+£", sprintf("%.1f", up$change), ")."),
        paste0("<b>Largest median fall (", y0, "->", y1, "):</b> ", as.character(dn$genreF),
               " (£", sprintf("%.1f", dn$change), ").")
      ))
    })
    
    output$hi_age_genre_year <- renderUI({
      d <- age_genre_year_medians(); req(d)
      ys <- years_active()
      if (length(ys) < 1) return(bullets_ui("No data for selected years."))
      
      y1 <- max(ys)
      
      last_age <- d %>%
        filter(as.integer(as.character(YearF)) == y1) %>%
        group_by(AgeF) %>%
        summarise(m = median(median_full, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(m)) %>%
        slice_head(n = 1)
      
      spread <- d %>%
        filter(as.integer(as.character(YearF)) == y1) %>%
        group_by(GenreF) %>%
        summarise(spread = max(median_full, na.rm = TRUE) - min(median_full, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(spread)) %>%
        slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>", y1, " priciest age band (overall):</b> ", as.character(last_age$AgeF),
               " (median ≈ £", sprintf("%.1f", last_age$m), ")."),
        paste0("<b>", y1, " strongest age segmentation:</b> ", as.character(spread$GenreF),
               " (age-band median range ≈ £", sprintf("%.1f", spread$spread), ").")
      ))
    })
    
    output$hi_venue_offer <- renderUI({
      d <- venue_offer(); req(d)
      ys <- years_active()
      if (length(ys) < 2) return(bullets_ui("Select at least two years to summarise changes."))
      
      y0 <- min(ys); y1 <- max(ys)
      
      last_rank <- d %>%
        filter(YearN == y1) %>%
        arrange(desc(offer_rate)) %>%
        slice_head(n = 2)
      
      wide <- d %>%
        filter(YearN %in% c(y0, y1)) %>%
        select(YearN, VenueLabel, offer_rate) %>%
        pivot_wider(names_from = YearN, values_from = offer_rate)
      
      if (!(as.character(y0) %in% names(wide)) || !(as.character(y1) %in% names(wide))) {
        return(bullets_ui("Not enough overlapping venue data to compute changes."))
      }
      
      wide <- wide %>% mutate(change = round(.data[[as.character(y1)]] - .data[[as.character(y0)]], 6))
      up <- wide %>% arrange(desc(change)) %>% slice_head(n = 1)
      dn <- wide %>% arrange(change) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>", y1, " highest offer-rate venues:</b> ",
               paste0(as.character(last_rank$VenueLabel), " (", scales::percent(last_rank$offer_rate, 0.1), ")", collapse = ", "), "."),
        paste0("<b>Biggest rise (", y0, "->", y1, "):</b> ", as.character(up$VenueLabel), " ", pp(up$change, 1), "."),
        paste0("<b>Biggest fall (", y0, "->", y1, "):</b> ", as.character(dn$VenueLabel), " ", pp(dn$change, 1), ".")
      ))
    })
    
    output$hi_genre_concessions <- renderUI({
      d <- genre_concessions_lines(); req(d)
      ys <- years_active()
      if (length(ys) < 2) return(bullets_ui("Select at least two years to summarise changes."))
      
      y0 <- min(ys); y1 <- max(ys)
      
      offer_last <- d %>%
        filter(metricF == "Offer rate", YearN == y1) %>%
        arrange(desc(value)) %>%
        slice_head(n = 2)
      
      disc_last <- d %>%
        filter(metricF == "Average discount", YearN == y1) %>%
        arrange(desc(value)) %>%
        slice_head(n = 2)
      
      wide <- d %>%
        filter(metricF == "Offer rate", YearN %in% c(y0, y1)) %>%
        select(YearN, GenreF, value) %>%
        pivot_wider(names_from = YearN, values_from = value)
      
      if (!(as.character(y0) %in% names(wide)) || !(as.character(y1) %in% names(wide))) {
        return(bullets_ui("Not enough overlapping genre data to compute offer-rate changes."))
      }
      
      wide <- wide %>% mutate(change = round(.data[[as.character(y1)]] - .data[[as.character(y0)]], 6))
      up <- wide %>% arrange(desc(change)) %>% slice_head(n = 1)
      dn <- wide %>% arrange(change) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>", y1, " highest offer-rate genres:</b> ",
               paste0(as.character(offer_last$GenreF), " (", scales::percent(offer_last$value, 0.1), ")", collapse = ", "), "."),
        paste0("<b>", y1, " deepest average discounts:</b> ",
               paste0(as.character(disc_last$GenreF), " (", scales::percent(disc_last$value, 0.1), ")", collapse = ", "), "."),
        paste0("<b>Offer-rate biggest rise (", y0, "->", y1, "):</b> ", as.character(up$GenreF), " ", pp(up$change, 1), "."),
        paste0("<b>Offer-rate biggest fall (", y0, "->", y1, "):</b> ", as.character(dn$GenreF), " ", pp(dn$change, 1), ".")
      ))
    })
    
  })
}