library(shiny)
library(bslib)
library(leaflet)
library(DT)

ui <- page_sidebar(
  title = "Apartment Hunter",
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
        "color_col",
        "Marker colour:",
        choices = c("Status" = "Status", "Value-Cost" = "Value_Cost"),
        selected = "Status",
        inline = TRUE
      ),
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
        selected = c("tour", "open", "msg"),
        multiple = TRUE
      ),
      selectInput(
        "type_filter",
        "Type:",
        choices  = c("studio", "1 bdrm"),
        selected = c("studio", "1 bdrm"),
        multiple = TRUE
      ),
      sliderInput(
        "cost_filter",
        "Total Cost ($):",
        min   = 0,
        max   = 5000,
        value = c(0, 5000),
        step  = 100,
        pre   = "$"
      ),
      checkboxInput("filter_parking", "Parking / EV", value = FALSE),
      checkboxInput("filter_laundry", "Laundry",      value = FALSE),
      checkboxInput("filter_gym",     "Gym",          value = FALSE),
      checkboxInput("filter_amenities", "Amenities",  value = FALSE)
    )
  ),

  # --- Main panel ---
  layout_columns(
    col_widths = 12,
    min_height = "1200px", # sets height of the rows

    card(
      full_screen = TRUE,
      card_header("Map"),
      leafletOutput("map") # can add height = "1000px" but normally not needed if using full_screen = TRUE
    ),

    card(
      card_header("Listings"),
      DTOutput("table")
    )
  )
)
