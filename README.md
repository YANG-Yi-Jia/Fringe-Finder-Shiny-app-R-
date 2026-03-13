# Fringe Finder – Edinburgh Festival Fringe Shiny App

An interactive R Shiny web application that turns official Edinburgh Festival Fringe 
listings data into a practical exploration tool for festivalgoers.

## 🔗 Live App
[Click here to view Fringe Finder](https://izr94x-yijia-yang.shinyapps.io/fringe-finder/)

## 📖 About
Final year dissertation project – School of Mathematics, University of Edinburgh, 2026.

Fringe Finder helps users navigate the Fringe's large programme (nearly 3,900 events 
across 300+ venues in 2025) through structured filtering, map-based venue exploration, 
and cross-year trend analysis — keeping the user in control of all decisions rather than 
relying on algorithmic recommendation.

## 🗂️ App Pages
- **Home** – Entry hub with quick navigation links
- **Discovery** – Browse and filter the 2025 catalogue by genre, tag, country, age, and price
- **Venue** – Interactive map linking venues to a searchable table and venue profile cards
- **Fringe Insights 2022–2025** – Cross-year dashboards covering scale trends, pricing, and venue concentration (HHI / Lorenz)
- **Summary** – Provenance notes and data interpretation guidance

## 📁 Project Structure
- `app.R` – Main application entry point
- `R/` – Page-level Shiny modules (Discovery, Venue, Insights, etc.)
- `data/` – Archived yearly `.xlsx` exports (2022–2025) and derived analysis-ready tables
- `www/` – CSS styling and image assets

## 🛠️ Built With
- R / Shiny + bslib
- plotly, leaflet, DT
- ggplot2 (static prototyping)
- rsconnect (deployment)

## 📊 Data Source
Edinburgh Festivals Listings API (2022–2025 exports).  
Data is used for exploration only; definitive details and ticketing are routed to 
[edfringe.com](https://www.edfringe.com/).
