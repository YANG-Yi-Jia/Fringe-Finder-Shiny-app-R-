# R/mod_ins_research.R
library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(htmlwidgets)

mod_ins_research_ui <- function(id) {
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
      
      # Chart 1: HHI
      div(
        class = "glass",
        div(
          class = "glass-inner",
          tags$h3("Venue concentration (HHI)", style = "margin: 0 0 6px 0;"),
          tags$p(
            "Single-number concentration trend by year: HHI = Σ(venue share of performances)^2.",
            style = "margin: 0 0 10px 0; opacity: 0.85;"
          ),
          plotlyOutput(ns("p_conc"), height = "520px"),
          hi_card("hi_conc"),
          tags$p(
            "HHI summarises how concentrated performances are across venues in each year by squaring venue performance shares (Σ share²), so the largest venues receive disproportionate weight. Pattern: higher HHI means a smaller set of venues accounts for more performances; lower HHI means performances are spread more evenly. Interpretation: HHI is easy to compare across years as a single trend, but it is most sensitive to shifts among the biggest venues rather than changes in the mid-tier.",
            style = "margin: 12px 0 0 0; opacity: 0.9;"
          )
        )
      ),
      
      # Chart 2: Lorenz curve
      div(
        class = "glass",
        div(
          class = "glass-inner",
          tags$h3("Concentration (Lorenz curve)", style = "margin: 0 0 6px 0;"),
          tags$p(
            "Distribution view by year: cumulative performances vs cumulative venues (venues ranked small->large).",
            style = "margin: 0 0 10px 0; opacity: 0.85;"
          ),
          plotlyOutput(ns("p_lorenz"), height = "560px"),
          hi_card("hi_lorenz"),
          tags$p(
            "Lorenz curves plot cumulative share of performances against cumulative share of venues after ranking venues from small to large within each year (x = cumulative venues, y = cumulative performances). Pattern: curves further below the 45° equality line indicate greater concentration. Interpretation: Unlike HHI (one number, top-tail weighted), Lorenz curves show where inequality sits across the whole distribution, so changes can reflect mid-tier reshuffling even when the biggest venues remain stable.",
            style = "margin: 12px 0 0 0; opacity: 0.9;"
          )
        )
      )
    )
  )
}

mod_ins_research_server <- function(id, ev_filtered) {
  moduleServer(id, function(input, output, session) {
    
    need_cols <- function(df, cols) {
      miss <- setdiff(cols, names(df))
      validate(need(length(miss) == 0, paste0("Missing column(s): ", paste(miss, collapse = ", "))))
      TRUE
    }
    
    to_num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]+", "", as.character(x))))
    
    bullets_ui <- function(items) {
      items <- items[!is.na(items) & nzchar(items)]
      if (!length(items)) return(HTML("<div style='opacity:0.85;'>No clear highlights detected for the selected years.</div>"))
      HTML(paste0("<ul style='margin:0; padding-left:18px; opacity:0.92;'>",
                  paste0("<li>", items, "</li>", collapse = ""), "</ul>"))
    }
    
    # “pp” here means percentage points; we stabilise float noise
    pp <- function(x, digits = 1) {
      if (!is.finite(x)) return("NA")
      x2 <- round(x, 6)
      paste0(ifelse(x2 >= 0, "+", ""), round(x2, digits), " pp")
    }
    
    years_active <- reactive({
      ev <- ev_filtered(); req(ev)
      ys <- sort(unique(as.integer(ev$year)))
      ys <- ys[!is.na(ys)]
      ys
    })
    
    # Base: event -> venue-year performances 
    venue_year_perf <- reactive({
      ev <- ev_filtered(); req(ev)
      need_cols(ev, c("year", "venue_code", "performances"))
      
      ev %>%
        transmute(
          year       = as.integer(year),
          venue_code = str_squish(as.character(venue_code)),
          perf       = to_num(performances)
        ) %>%
        filter(!is.na(year), !is.na(venue_code), venue_code != "") %>%
        mutate(perf = ifelse(is.finite(perf), perf, 0)) %>%
        group_by(year, venue_code) %>%
        summarise(performances = sum(perf, na.rm = TRUE), .groups = "drop") %>%
        filter(is.finite(performances), performances > 0)
    })
    

    # Chart 1: HHI by year
    conc_by_year <- reactive({
      df <- venue_year_perf(); req(df)
      
      df %>%
        group_by(year) %>%
        summarise(
          total = sum(performances, na.rm = TRUE),
          hhi   = {
            s <- performances / total
            sum(s * s, na.rm = TRUE)
          },
          n_venues = n_distinct(venue_code),
          .groups = "drop"
        ) %>%
        arrange(year)
    })
    
    output$p_conc <- renderPlotly({
      df <- conc_by_year(); req(df)
      validate(need(nrow(df) > 0, "No performance data to compute concentration in the selected range."))
      
      ys <- years_active()
      if (!length(ys)) ys <- sort(unique(df$year))
      
      d <- df %>%
        arrange(year) %>%
        mutate(
          hover = paste0(
            "Year: ", year,
            "<br>HHI: ", ifelse(is.finite(hhi), sprintf("%.4f", hhi), "NA"),
            "<br>Venues: ", n_venues
          )
        )
      
      d_line <- d %>% filter(is.finite(hhi))
      
      p <- ggplot() +
        geom_line(
          data = d_line,
          aes(x = year, y = hhi, group = 1),
          linewidth = 0.9, alpha = 0.95
        ) +
        geom_point(
          data = d,
          aes(x = year, y = hhi, text = hover),
          size = 2.2
        ) +
        scale_x_continuous(
          breaks = ys,
          limits = range(ys),
          expand = expansion(mult = c(0.02, 0.06))
        ) +
        labs(x = "Year", y = "HHI (performance concentration)") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.margin = margin(12, 14, 12, 14, "pt")
        )
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(margin = list(l = 80, r = 30, t = 15, b = 60), autosize = TRUE) %>%
        plotly::config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        htmlwidgets::onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })
    

    # Chart 2: Lorenz curve data
    lorenz_df <- reactive({
      df <- venue_year_perf(); req(df)
      
      df %>%
        group_by(year) %>%
        arrange(performances, .by_group = TRUE) %>%
        mutate(
          n          = n(),
          total_perf = sum(performances, na.rm = TRUE),
          cum_venues = row_number() / n,
          cum_perf   = cumsum(performances) / total_perf
        ) %>%
        ungroup() %>%
        filter(is.finite(cum_venues), is.finite(cum_perf))
    })
    
    output$p_lorenz <- renderPlotly({
      df <- lorenz_df(); req(df)
      validate(need(nrow(df) > 0, "No venue performance data to draw Lorenz curve."))
      
      ys <- years_active()
      if (!length(ys)) ys <- sort(unique(df$year))
      
      d <- df %>%
        mutate(
          yearF = factor(year, levels = ys),
          hover = paste0(
            "Year: ", year,
            "<br>Cumulative venues: ", scales::percent(cum_venues, accuracy = 0.1),
            "<br>Cumulative performances: ", scales::percent(cum_perf, accuracy = 0.1)
          )
        )
      
      only_one_year <- length(levels(d$yearF)) < 2
      
      p <- ggplot(d, aes(x = cum_venues, y = cum_perf, color = yearF, group = yearF, text = hover)) +
        geom_line(linewidth = 1.0, alpha = 0.95, na.rm = TRUE) +
        geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.6) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(
          x = "Cumulative share of venues",
          y = "Cumulative share of performances",
          color = "Year"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          legend.title = element_text(face = "bold"),
          plot.margin = margin(12, 14, 12, 14, "pt")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 80, r = 30, t = 10, b = 60),
          legend = list(orientation = if (only_one_year) "v" else "h",
                        x = 0.5, xanchor = "center", y = 1.05),
          autosize = TRUE
        ) %>%
        config(displayModeBar = TRUE, scrollZoom = FALSE, responsive = TRUE) %>%
        onRender("function(el){setTimeout(function(){Plotly.Plots.resize(el)}, 50);}")
    })

    # Highlights: HHI card
    output$hi_conc <- renderUI({
      df <- conc_by_year(); req(df)
      df <- df %>% arrange(year)
      validate(need(nrow(df) >= 1, "No data."))
      
      ys <- sort(unique(df$year))
      if (length(ys) < 2) return(bullets_ui("Select at least two years to summarise changes."))
      
      y0 <- min(ys); y1 <- max(ys)
      
      h0 <- (df %>% filter(year == y0) %>% pull(hhi))[1]
      h1 <- (df %>% filter(year == y1) %>% pull(hhi))[1]
      
      max_row <- df %>% arrange(desc(hhi)) %>% slice_head(n = 1)
      min_row <- df %>% arrange(hhi) %>% slice_head(n = 1)
      
      bullets_ui(c(
        paste0("<b>HHI change (", y0, "->", y1, "):</b> ",
               sprintf("%.4f", h0), " -> ", sprintf("%.4f", h1),
               " (", pp(100 * (h1 - h0), 2), ")."),
        paste0("<b>Most concentrated year:</b> ", max_row$year, " (HHI ", sprintf("%.4f", max_row$hhi), ")."),
        paste0("<b>Least concentrated year:</b> ", min_row$year, " (HHI ", sprintf("%.4f", min_row$hhi), ").")
      ))
    })
    
    # Highlights: Lorenz card (Top 10% venues share + Top 20% context)
    # We approximate via Lorenz: bottom (1-x) venues account for y => top x venues account for (1-y).
    output$hi_lorenz <- renderUI({
      df <- lorenz_df(); req(df)
      validate(need(nrow(df) > 0, "No data."))
      
      ys <- sort(unique(df$year))
      if (!length(ys)) return(bullets_ui("No data."))
      
      approx_top_share <- function(d_year, top_frac = 0.10) {
        x0 <- 1 - top_frac
        d <- d_year %>% arrange(cum_venues)
        idx <- which.min(abs(d$cum_venues - x0))
        y_at <- d$cum_perf[idx]
        1 - y_at
      }
      
      top10 <- df %>%
        group_by(year) %>%
        summarise(top10_share = approx_top_share(cur_data(), 0.10), .groups = "drop")
      
      top20 <- df %>%
        group_by(year) %>%
        summarise(top20_share = approx_top_share(cur_data(), 0.20), .groups = "drop")
      
      y0 <- min(ys); y1 <- max(ys)
      
      t0 <- (top10 %>% filter(year == y0) %>% pull(top10_share))[1]
      t1 <- (top10 %>% filter(year == y1) %>% pull(top10_share))[1]
      
      mx <- top10 %>% arrange(desc(top10_share)) %>% slice_head(n = 1)
      mn <- top10 %>% arrange(top10_share) %>% slice_head(n = 1)
      
      t20_y1 <- (top20 %>% filter(year == y1) %>% pull(top20_share))[1]
      
      bullets_ui(c(
        paste0("<b>Top 10% venues share (", y0, "->", y1, "):</b> ",
               scales::percent(t0, accuracy = 0.1), " -> ", scales::percent(t1, accuracy = 0.1),
               " (", pp(100 * (t1 - t0), 1), ")."),
        paste0("<b>Highest top-10% share:</b> ", mx$year, " (", scales::percent(mx$top10_share, accuracy = 0.1), ")."),
        paste0("<b>Lowest top-10% share:</b> ", mn$year, " (", scales::percent(mn$top10_share, accuracy = 0.1), ")."),
        paste0("<b>Context (", y1, "):</b> Top 20% venues account for ", scales::percent(t20_y1, accuracy = 0.1), ".")
      ))
    })
    
  })
}