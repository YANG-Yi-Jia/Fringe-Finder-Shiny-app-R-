# R/mod_ins_general.R
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(patchwork)
library(stringr)

mod_ins_general_ui <- function(id) {
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
      
      # 1) Shows & venues
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Shows & venues per year", style = "margin: 0 0 6px 0;"),
              tags$p("Lines show yearly counts: left axis = unique shows (code), right axis = unique venues (venue_code).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_shows_venues"), height = "520px"),
              hi_card("hi_shows_venues"),
              tags$p(
                "Compares annual scale on two axes—unique shows (distinct code) and unique venues (distinct venue_code). Pattern: Parallel movement suggests broad growth, while divergence shows whether expansion comes from more productions or more locations. Interpretation: Faster growth in shows than venues implies higher utilisation and concentration within existing venues; faster growth in venues suggests spatial footprint expansion. Using distinct counts limits inflation from repeated listings, so year-to-year change reflects real variety rather than duplicate records.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 2) Price trend
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Ticket price trend", style = "margin: 0 0 6px 0;"),
              tags$p("Tracks the median lowest full ticket price by year, with the interquartile range (25th–75th percentile) as uncertainty band.",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_price"), height = "520px"),
              hi_card("hi_price"),
              tags$p(
                "The line is the yearly median of lowest_full_price; the shaded band is the IQR (25th–75th percentiles). Pattern: The median shows the typical entry-level price, while band width shows within-year dispersion (uniform vs segmented pricing). Interpretation: IQR is used because prices are often skewed and may include outliers; the 25th–75th window is robust and captures the central half of the market. Rising median indicates broad-based inflation; widening IQR suggests increasing price differentiation across shows.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 3) Genre composition
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Genre composition", style = "margin: 0 0 6px 0;"),
              tags$p("Shows each year’s genre mix as shares of unique events (deduplicated by year × code × genre).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_mix"), height = "520px"),
              hi_card("hi_mix"),
              tags$p(
                "Each year’s bars show the share of unique events by genre, where events are deduplicated at year × code × genre. Pattern: Shifts in bar lengths across years reflect programme restructuring, not just overall size change. Interpretation: Genres are ordered by the latest year share from small to large, so category positions are stable and comparisons are visually consistent. Using shares isolates composition effects from total volume, and deduplication avoids over-counting the same show due to repeated listings or multi-venue runs.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 4) Genre event trend
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Unique events per genre by year", style = "margin: 0 0 6px 0;"),
              tags$p("Trends of event counts across different genres (selected years).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotOutput(ns("p_genre_trend"), height = "720px"),
              hi_card("hi_genre_trend"),
              tags$p(
                "Small multiples show yearly counts of unique events per genre, computed from distinct year × code × genre records (each show contributes at most once per genre per year). Pattern: Panel slopes highlight steady growth, volatility, or reversals within each genre. Interpretation: This separates scale from composition—genres can gain share by growing or by others shrinking. Deduplicating by code reduces bias from duplicated imports, and zero-filling missing year–genre pairs prevents missing data from being misread as ‘no events’.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      ),
      
      # 5) Postcode sector distribution
      div(class = "glass",
          div(class = "glass-inner",
              tags$h3("Venue count by postcode sector", style = "margin: 0 0 6px 0;"),
              tags$p("Top 15 postcode sectors by number of unique venues (selected per year).",
                     style = "margin: 0 0 10px 0; opacity: 0.85;"),
              plotlyOutput(ns("p_postcode_dist"), height = "640px"),
              hi_card("hi_postcode"),
              tags$p(
                "Each panel shows that year’s top 15 postcode sectors, ranked by the number of unique venues. Pattern: Rank shifts reflect where the venue footprint expands or contracts year-to-year. Interpretation: Using per-year top 15 keeps the chart responsive to real churn at the margin, while explicit rank labels (01, 02, …) make it easy to track leaders vs movers. Sector extraction is restricted to valid patterns to avoid malformed postcodes creating artificial locations.",
                style = "margin: 12px 0 0 0; opacity: 0.9;"
              )
          )
      )
    )
  )
}

mod_ins_general_server <- function(id, ev_filtered) {
  moduleServer(id, function(input, output, session) {
    
    # ---------- helpers ----------
    years_vec <- reactive({
      ev <- ev_filtered(); req(ev)
      ys <- sort(unique(as.integer(ev$year)))
      ys <- ys[!is.na(ys)]
      ys
    })
    
    bullets_ui <- function(items) {
      items <- items[!is.na(items) & nzchar(items)]
      if (!length(items)) {
        return(HTML("<div style='opacity:0.85;'>No clear highlights detected for the selected years.</div>"))
      }
      HTML(paste0(
        "<ul style='margin:0; padding-left:18px; opacity:0.92;'>",
        paste0("<li>", items, "</li>", collapse = ""),
        "</ul>"
      ))
    }
    
    pct <- function(x, digits = 0) {
      if (!is.finite(x)) return("NA")
      paste0(round(100 * x, digits), "%")
    }
    
    pp <- function(x, digits = 1) {
      if (!is.finite(x)) return("NA")
      x2 <- round(x, 6)
      paste0(ifelse(x2 >= 0, "+", ""), round(100 * x2, digits), " pp")
    }
    
    # summary
    shows_venues_by_year <- reactive({
      ev <- ev_filtered(); req(ev)
      ev %>%
        transmute(
          year = as.integer(year),
          code = as.character(code),
          venue_code = as.character(venue_code)
        ) %>%
        filter(!is.na(year)) %>%
        group_by(year) %>%
        summarise(
          shows  = n_distinct(code),
          venues = n_distinct(venue_code[!is.na(venue_code) & venue_code != ""]),
          .groups = "drop"
        ) %>%
        arrange(year)
    })
    
    price_by_year <- reactive({
      ev <- ev_filtered(); req(ev)
      ev %>%
        transmute(year = as.integer(year), p = as.numeric(lowest_full_price)) %>%
        filter(!is.na(year), is.finite(p)) %>%
        group_by(year) %>%
        summarise(
          n = n(),
          median_price = median(p, na.rm = TRUE),
          p25 = as.numeric(stats::quantile(p, 0.25, na.rm = TRUE)),
          p75 = as.numeric(stats::quantile(p, 0.75, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        arrange(year)
    })
    
    genre_mix <- reactive({
      ev <- ev_filtered(); req(ev)
      base <- ev %>%
        transmute(year = as.integer(year), code = as.character(code), genre = str_squish(as.character(genre))) %>%
        filter(!is.na(year), !is.na(code), code != "", !is.na(genre), genre != "") %>%
        distinct(year, code, genre)
      
      mix <- base %>%
        count(year, genre, name = "events") %>%
        group_by(year) %>%
        mutate(share = events / sum(events)) %>%
        ungroup()
      
      y_last <- suppressWarnings(max(mix$year, na.rm = TRUE))
      
      order_levels <- mix %>%
        group_by(genre) %>%
        summarise(
          share_last = max(share[year == y_last], na.rm = TRUE),
          total = sum(events),
          .groups = "drop"
        ) %>%
        mutate(share_last = ifelse(is.infinite(share_last), 0, share_last)) %>%
        arrange(share_last, total) %>%
        pull(genre)
      
      mix %>%
        mutate(
          genre = factor(genre, levels = order_levels),
          yearF = factor(year, levels = years_vec())
        )
    })
    
    genre_trend_data <- reactive({
      ev <- ev_filtered(); req(ev)
      ys <- years_vec(); if (!length(ys)) ys <- 2022:2025
      
      base <- ev %>%
        transmute(year = as.integer(year), code = as.character(code), genre = str_squish(as.character(genre))) %>%
        filter(year %in% ys, !is.na(code), code != "", !is.na(genre), genre != "") %>%
        distinct(year, code, genre)
      
      order_levels <- base %>% count(genre, sort = TRUE) %>% pull(genre)
      
      base %>%
        count(year, genre, name = "events") %>%
        complete(year = ys, genre, fill = list(events = 0)) %>%
        mutate(genre = factor(genre, levels = order_levels))
    })
    
    # postcode: per-year top1
    postcode_ranked <- reactive({
      ev <- ev_filtered(); req(ev)
      ys <- years_vec(); if (!length(ys)) ys <- 2022:2025
      
      df <- ev %>%
        transmute(
          year = as.integer(year),
          venue_code = str_squish(as.character(venue_code)),
          venue_postcode = str_squish(as.character(venue_postcode))
        ) %>%
        filter(year %in% ys, !is.na(venue_code), venue_code != "") %>%
        group_by(year, venue_code) %>%
        summarise(
          pc = first(venue_postcode[!is.na(venue_postcode) & venue_postcode != ""]),
          .groups = "drop"
        ) %>%
        filter(!is.na(pc), pc != "") %>%
        mutate(
          sector = str_to_upper(str_replace(pc, "^([A-Za-z]{1,2}\\d{1,2}).*$", "\\1"))
        ) %>%
        filter(str_detect(sector, "^[A-Z]{1,2}\\d{1,2}$"))
      
      ranked <- df %>%
        count(year, sector, name = "n") %>%
        group_by(year) %>%
        arrange(desc(n), sector) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 15) %>%
        ungroup() %>%
        mutate(
          yearF = factor(year, levels = ys),
          sector_rank_lab = paste0(sprintf("%02d", rank), ". ", sector),
          sector_rank_key = paste0(sector_rank_lab, " | ", year)
        )
      
      levs <- ranked %>%
        arrange(year, rank) %>%
        pull(sector_rank_key)
      
      ranked %>%
        mutate(sector_rank_key = factor(sector_rank_key, levels = rev(unique(levs))))
    })
    
    
    output$p_shows_venues <- renderPlotly({
      df <- shows_venues_by_year(); req(df)
      validate(need(nrow(df) > 0, "No data."))
      
      r1 <- range(df$shows, finite = TRUE); if (!is.finite(r1[1])) r1 <- c(0, 1)
      r2 <- range(df$venues, finite = TRUE); if (!is.finite(r2[1])) r2 <- c(0, 1)
      buf1 <- max(diff(r1) * 0.1, 1e-6); buf2 <- max(diff(r2) * 0.1, 1e-6)
      lim1 <- c(r1[1] - buf1, r1[2] + buf1)
      lim2 <- c(r2[1] - buf2, r2[2] + buf2)
      
      a <- diff(lim2) / diff(lim1)
      b <- lim2[1] - a * lim1[1]
      ticks1 <- pretty(lim1, n = 5)
      ticks2 <- a * ticks1 + b
      
      plotly::plot_ly(df, x = ~year) %>%
        add_trace(
          y = ~shows, name = "Shows",
          type = "scatter", mode = "lines+markers",
          marker = list(symbol = "circle", size = 9, line = list(color = "white", width = 1))
        ) %>%
        add_trace(
          y = ~venues, name = "Venues", yaxis = "y2",
          type = "scatter", mode = "lines+markers",
          marker = list(symbol = "circle", size = 9, line = list(color = "white", width = 1))
        ) %>%
        layout(
          xaxis = list(title = "Year", tickmode = "array", tickvals = df$year,
                       range = c(min(df$year) - 0.5, max(df$year) + 0.5), showgrid = FALSE),
          yaxis  = list(title = "Shows", range = lim1, tickvals = ticks1, zeroline = FALSE),
          yaxis2 = list(title = "Venues", overlaying = "y", side = "right",
                        range = lim2, tickvals = ticks2, ticktext = round(ticks2, 0),
                        showgrid = FALSE, zeroline = FALSE),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
          margin = list(r = 50),
          autosize = TRUE
        ) %>%
        config(responsive = TRUE)
    })
    
    output$p_price <- renderPlotly({
      df <- price_by_year(); req(df)
      validate(need(nrow(df) > 0, "No valid prices."))
      
      line_col <- "#2c3e50"
      fill_col <- "rgba(44, 62, 80, 0.15)"
      
      plotly::plot_ly(df, x = ~year) %>%
        add_lines(y = ~p25, line = list(width = 0), showlegend = FALSE, hoverinfo = "none") %>%
        add_lines(
          y = ~p75, line = list(width = 0),
          fill = "tonexty", fillcolor = fill_col,
          name = "IQR (25th-75th)", hoverinfo = "none"
        ) %>%
        add_trace(
          y = ~median_price,
          type = "scatter", mode = "lines+markers",
          name = "Median Price",
          line = list(color = line_col, width = 2.5),
          marker = list(color = line_col, size = 10, symbol = "circle", line = list(color = "white", width = 1.5)),
          text = ~paste0(
            "<b>Year: ", year, "</b><br>",
            "Median: £", format(round(median_price, 2), nsmall = 2), "<br>",
            "IQR: £", round(p25, 2), " - £", round(p75, 2)
          ),
          hoverinfo = "text"
        ) %>%
        layout(
          hovermode = "closest",
          xaxis = list(title = "Year", tickmode = "array", tickvals = df$year,
                       range = c(min(df$year) - 0.4, max(df$year) + 0.4), showgrid = FALSE),
          yaxis = list(title = "Price (GBP)", zeroline = FALSE,
                       range = c(min(df$p25) * 0.8, max(df$p75) * 1.2)),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05),
          margin = list(t = 50),
          autosize = TRUE
        ) %>%
        config(responsive = TRUE)
    })
    
    output$p_mix <- renderPlotly({
      df <- genre_mix(); req(df)
      validate(need(nrow(df) > 0, "No genre data."))
      
      p <- ggplot(
        df,
        aes(
          x = share, y = genre, fill = genre,
          text = paste0(genre, "\nShare: ", scales::percent(share, accuracy = 0.1))
        )
      ) +
        geom_col(width = 0.7, show.legend = FALSE) +
        facet_wrap(~ yearF, nrow = 1) +
        scale_x_continuous(
          breaks = c(0, 0.2, 0.4),
          labels = scales::percent_format(accuracy = 1),
          expand = expansion(mult = c(0.005, 0.02))
        ) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(color = "#333333", face = "bold"),
          strip.text = element_text(face = "bold", size = 12)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 100), autosize = TRUE) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    })
    
    output$p_genre_trend <- renderPlot({
      df <- genre_trend_data(); req(df)
      validate(need(nrow(df) > 0, "No genre trend data."))
      
      ys <- sort(unique(df$year))
      genres <- levels(df$genre)
      only_one_year <- length(ys) < 2
      
      make_one <- function(g) {
        d <- df %>% filter(genre == g)
        ggplot(d, aes(x = year, y = events)) +
          { if (!only_one_year) geom_line(linewidth = 0.45) } +
          geom_point(size = 1.3) +
          geom_text(aes(label = events), vjust = -0.6, size = 2.2) +
          scale_x_continuous(breaks = ys, expand = expansion(mult = c(0.02, 0.02))) +
          scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
          labs(title = g, x = NULL, y = NULL) +
          theme_minimal(base_size = 9) +
          theme(
            plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
            panel.grid.minor = element_blank(),
            plot.margin = margin(6, 6, 6, 6, "pt")
          )
      }
      
      plist <- lapply(genres, make_one)
      wrap_plots(plist, ncol = 3)
    })
    
    output$p_postcode_dist <- renderPlotly({
      d <- postcode_ranked(); req(d)
      validate(need(nrow(d) > 0, "No valid postcode sectors."))
      
      d <- d %>%
        mutate(
          hover = paste0(
            "Year: ", as.character(yearF),
            "<br>Sector: ", sector,
            "<br>Rank: #", rank,
            "<br>Venues: ", n
          ),
          sectorF = factor(sector)
        )
      
      max_n <- max(d$n, na.rm = TRUE)
      
      p <- ggplot(d, aes(x = n, y = sector_rank_key, fill = sectorF, text = hover)) +
        geom_col(width = 0.70, show.legend = FALSE) +
        facet_wrap(~ yearF, nrow = 1, scales = "free_y") + 
        scale_x_continuous(limits = c(0, max_n * 1.1)) + 
        scale_y_discrete(labels = function(x) str_replace(x, "\\s\\|\\s\\d+$", ""), drop = TRUE) +
        guides(fill = "none") +   
        labs(x = "Number of venues", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", size = 12),
          plot.margin = margin(6, 10, 6, 10) 
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          showlegend = FALSE,
          margin = list(l = 150, r = 30, t = 40, b = 50),
          xaxis = list(anchor = "fixed"), 
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE, responsive = TRUE)
    })
    
    # highlight cards
    output$hi_shows_venues <- renderUI({
      df <- shows_venues_by_year(); req(df)
      df <- df %>% arrange(year)
      if (nrow(df) < 2) return(bullets_ui("Select at least two years to summarise change."))
      
      y0 <- df$year[1]; y1 <- df$year[nrow(df)]
      s0 <- df$shows[1]; s1 <- df$shows[nrow(df)]
      v0 <- df$venues[1]; v1 <- df$venues[nrow(df)]
      
      gS <- if (s0 > 0) (s1 - s0) / s0 else NA_real_
      gV <- if (v0 > 0) (v1 - v0) / v0 else NA_real_
      
      r0 <- if (v0 > 0) s0 / v0 else NA_real_
      r1 <- if (v1 > 0) s1 / v1 else NA_real_
      gR <- if (is.finite(r0) && r0 > 0) (r1 - r0) / r0 else NA_real_
      
      bullets_ui(c(
        paste0("<b>", y0, "->", y1, " scale:</b> shows ", s0, "->", s1, " (", pct(gS, 0),
               "), venues ", v0, "->", v1, " (", pct(gV, 0), ")."),
        paste0("<b>Shows per venue:</b> ", round(r0, 2), "->", round(r1, 2),
               " (", pct(gR, 0), ") -> ",
               ifelse(!is.finite(gR), "direction unclear.", ifelse(gR > 0, "more utilisation/concentration.", "broader footprint expansion.")))
      ))
    })
    
    output$hi_price <- renderUI({
      df <- price_by_year(); req(df)
      df <- df %>% arrange(year)
      if (nrow(df) < 2) return(bullets_ui("Select at least two years to summarise change."))
      
      y0 <- df$year[1]; y1 <- df$year[nrow(df)]
      m0 <- df$median_price[1]; m1 <- df$median_price[nrow(df)]
      i0 <- df$p75[1] - df$p25[1]
      i1 <- df$p75[nrow(df)] - df$p25[nrow(df)]
      
      dM <- if (is.finite(m0) && m0 > 0) (m1 - m0) / m0 else NA_real_
      dI <- if (is.finite(i0) && i0 > 0) (i1 - i0) / i0 else NA_real_
      
      bullets_ui(c(
        paste0("<b>Median (", y0, "->", y1, "):</b> £", round(m0, 2), "->£", round(m1, 2), " (", pct(dM, 0), ")."),
        paste0("<b>IQR width:</b> £", round(i0, 2), "->£", round(i1, 2), " (", pct(dI, 0), ") -> ",
               ifelse(!is.finite(dI), "direction unclear.", ifelse(dI > 0, "greater price differentiation.", "more uniform pricing.")))
      ))
    })
    
    output$hi_mix <- renderUI({
      df <- genre_mix(); req(df)
      years <- sort(unique(as.integer(as.character(df$yearF))))
      if (length(years) < 2) return(bullets_ui("Select at least two years to compute share change."))
      
      y0 <- min(years); y1 <- max(years)
      
      last2 <- df %>%
        filter(as.integer(as.character(yearF)) == y1) %>%
        arrange(desc(share)) %>%
        slice_head(n = 2)
      
      delta <- df %>%
        transmute(year = as.integer(as.character(yearF)),
                  genre = as.character(genre),
                  share = as.numeric(share)) %>%
        filter(year %in% c(y0, y1)) %>%
        pivot_wider(names_from = year, values_from = share, values_fill = 0) %>%
        mutate(change = round(.data[[as.character(y1)]] - .data[[as.character(y0)]], 6))
      
      top_rise <- delta %>% arrange(desc(change)) %>% slice_head(n = 1)
      top_fall <- delta %>% arrange(change) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>", y1, " largest genres:</b> ",
               paste0(last2$genre, " (", scales::percent(last2$share, accuracy = 0.1), ")", collapse = ", "), "."),
        paste0("<b>Biggest share rise (", y0, "->", y1, "):</b> ", top_rise$genre, " ", pp(top_rise$change, 1), "."),
        paste0("<b>Biggest share fall (", y0, "->", y1, "):</b> ", top_fall$genre, " ", pp(top_fall$change, 1), ".")
      ))
    })
    
    output$hi_genre_trend <- renderUI({
      df <- genre_trend_data(); req(df)
      ys <- sort(unique(df$year))
      if (length(ys) < 2) return(bullets_ui("Only one year selected: trends reduce to single-year counts."))
      
      wide <- df %>%
        group_by(genre) %>%
        summarise(
          y0 = events[match(min(ys), year)],
          y1 = events[match(max(ys), year)],
          .groups = "drop"
        ) %>%
        mutate(
          y0 = ifelse(is.na(y0), 0, y0),
          y1 = ifelse(is.na(y1), 0, y1),
          diff = y1 - y0
        )
      
      up <- wide %>% arrange(desc(diff)) %>% slice_head(n = 1)
      dn <- wide %>% arrange(diff) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>Largest increase (", min(ys), "->", max(ys), "):</b> ", up$genre, " (", up$y0, "->", up$y1, ")."),
        paste0("<b>Largest decrease (", min(ys), "->", max(ys), "):</b> ", dn$genre, " (", dn$y0, "->", dn$y1, ").")
      ))
    })
    
    output$hi_postcode <- renderUI({
      d <- postcode_ranked(); req(d)
      years <- sort(unique(as.integer(as.character(d$yearF))))
      if (length(years) < 2) return(bullets_ui("Select at least two years to summarise rank changes."))
      
      y0 <- min(years); y1 <- max(years)
      
      last_top3 <- d %>%
        filter(as.integer(as.character(yearF)) == y1) %>%
        arrange(rank) %>%
        slice_head(n = 3) %>%
        transmute(txt = paste0("#", rank, " ", sector, " (", n, ")")) %>%
        pull(txt)
      
      both <- d %>%
        filter(year %in% c(y0, y1)) %>%
        select(year, sector, rank) %>%
        pivot_wider(names_from = year, values_from = rank)
      
      if (nrow(both)) {
        both <- both %>%
          mutate(change = .data[[as.character(y0)]] - .data[[as.character(y1)]])
      }
      
      best_up <- both %>% filter(!is.na(change)) %>% arrange(desc(change)) %>% slice_head(n = 1)
      best_dn <- both %>% filter(!is.na(change)) %>% arrange(change) %>% slice_head(n = 1)
      
      set_y0 <- d %>% filter(year == y0) %>% pull(sector) %>% unique()
      set_y1 <- d %>% filter(year == y1) %>% pull(sector) %>% unique()
      
      entrants <- setdiff(set_y1, set_y0)
      exits    <- setdiff(set_y0, set_y1)
      
      bullets <- c(
        paste0("<b>", y1, " top sectors:</b> ", paste(last_top3, collapse = ", "), ".")
      )
      
      if (nrow(best_up)) {
        bullets <- c(bullets,
                     paste0("<b>Biggest rank improvement (", y0, "->", y1, "):</b> ",
                            best_up$sector, " (#", best_up[[as.character(y0)]], "->#", best_up[[as.character(y1)]], ").")
        )
      }
      
      if (nrow(best_dn)) {
        bullets <- c(bullets,
                     paste0("<b>Biggest rank drop (", y0, "->", y1, "):</b> ",
                            best_dn$sector, " (#", best_dn[[as.character(y0)]], "->#", best_dn[[as.character(y1)]], ").")
        )
      }
      
      if (length(entrants)) bullets <- c(bullets, paste0("<b>New in ", y1, " top 15:</b> ", paste(entrants, collapse = ", "), "."))
      if (length(exits))    bullets <- c(bullets, paste0("<b>Exit by ", y1, ":</b> ", paste(exits, collapse = ", "), "."))
      
      bullets_ui(bullets)
    })
    
  })
}