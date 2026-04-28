library(shiny)
library(bslib)
library(leaflet)
library(DT)

ui <- page_sidebar(
  title = "Apartment Hunt",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 280,

    # --- Sheet connection ---
    card(
      card_header("Google Sheet"),
      textInput(
        "sheet_url",
        label = NULL,
        placeholder = "Paste Sheet URL (optional)"
      ),
      actionButton("reload", "Load / Reload", icon = icon("rotate"),
                   class = "btn-primary w-100")
    ),

    # --- Map encoding ---
    card(
      card_header("Map Encoding"),
      radioButtons(
        "size_col",
        "Marker size:",
        choices = c("Rent" = "Rent", "Total Cost" = "Ttl_Cost"),
        selected = "Ttl_Cost",
        inline = TRUE
      )
    ),

    # --- Filters ---
    card(
      card_header("Filters"),
      selectInput(
        "status_filter",
        "Status:",
        choices  = c("denied", "tour", "open", "ref", "unavail", "reject", "msg"),
        selected = c("tour", "open", "msg", "ref"),
        multiple = TRUE
      ),
      selectInput(
        "type_filter",
        "Type:",
        choices  = c("studio", "1 bdrm"),
        selected = c("studio", "1 bdrm"),
        multiple = TRUE
      ),
      checkboxInput("filter_parking", "Parking / EV only", value = FALSE),
      checkboxInput("filter_laundry", "Laundry only",      value = FALSE),
      checkboxInput("filter_gym",     "Gym only",          value = FALSE),
      checkboxInput("filter_amenities", "Amenities only",  value = FALSE)
    )
  ),

  # --- Main panel ---
  layout_columns(
    col_widths = 12,

    card(
      height = "950px",
      full_screen = TRUE,
      card_header("Map"),
      leafletOutput("map", height = "100%")
    ),

    card(
      card_header("Listings"),
      DTOutput("table")
    )
  )
)
