library(shiny)
library(leaflet)
library(DT)
library(dplyr)

source("R/fetch_sheet.R")
source("R/geocode_writeback.R")
source("R/build_map.R")

# Columns shown in the summary table
TABLE_COLS <- c("Apartment", "Status", "Rent", "Ttl_Cost",
                "Parking_EV", "Laundry", "Gym", "Storage")

server <- function(input, output, session) {

  # ── Resolve sheet ID ────────────────────────────────────────────────────────
  # Extracts bare sheet ID from a full URL or returns the string unchanged
  # if it's already a bare ID.
  extract_id <- function(s) {
    m <- regmatches(s, regexpr("(?<=/d/)[A-Za-z0-9_-]+", s, perl = TRUE))
    if (length(m) == 1) return(m)
    if (grepl("^[A-Za-z0-9_-]{20,}$", s)) return(s)  # looks like a bare ID
    NULL
  }

  sheet_id <- reactive({
    url <- trimws(input$sheet_url)
    if (nchar(url) > 0) {
      id <- extract_id(url)
      if (!is.null(id)) return(id)
      showNotification("Could not parse Sheet ID from URL.", type = "error")
      return(NULL)
    }
    raw <- Sys.getenv("SHEET_ID")
    if (nchar(raw) == 0) {
      showNotification("No SHEET_ID env var set and no URL provided.", type = "error")
      return(NULL)
    }
    id <- extract_id(raw)
    if (is.null(id)) {
      showNotification("SHEET_ID env var is not a valid ID or URL.", type = "error")
      return(NULL)
    }
    id
  })

  # ── Fetch + geocode (triggered by reload button) ─────────────────────────────
  apt_data <- eventReactive(input$reload, {
    req(sheet_id())
    withProgress(message = "Loading sheet...", value = 0.2, {
      df <- fetch_sheet(sheet_id())
      setProgress(0.5, message = "Geocoding new addresses...")
      df <- tryCatch(
        geocode_writeback(df, sheet_id()),
        error = function(e) {
          showNotification(paste("Geocoding error:", conditionMessage(e)),
                           type = "warning", duration = 10)
          df  # return df without geocoded coords rather than crashing
        }
      )
      setProgress(1, message = "Done")
      df
    })
  }, ignoreNULL = FALSE)

  # ── Filtered data ────────────────────────────────────────────────────────────
  filtered_data <- reactive({
    df <- apt_data()
    if (is.null(df) || nrow(df) == 0) return(df)

    # Status filter
    if (length(input$status_filter) > 0) {
      df <- df |> filter(tolower(Status) %in% tolower(input$status_filter))
    }

    # Boolean filters (show only TRUE when checkbox ticked)
    if (isTRUE(input$filter_parking))   df <- df |> filter(Parking_EV == TRUE)
    if (isTRUE(input$filter_laundry))   df <- df |> filter(Laundry    == TRUE)
    if (isTRUE(input$filter_gym))       df <- df |> filter(Gym        == TRUE)
    if (isTRUE(input$filter_amenities)) df <- df |> filter(Amenities  == TRUE)

    # Type filter
    if (length(input$type_filter) > 0) {
      df <- df |> filter(tolower(trimws(Type)) %in% tolower(input$type_filter))
    }

    df
  })

  # ── Dynamic Status choices (update when sheet reloads) ──────────────────────
  observeEvent(apt_data(), {
    statuses <- sort(unique(tolower(apt_data()$Status)))
    updateSelectInput(session, "status_filter",
                      choices  = statuses,
                      selected = intersect(c("tour", "open", "msg", "ref"), statuses))

    types <- sort(unique(tolower(trimws(apt_data()$Type))))
    updateSelectInput(session, "type_filter",
                      choices  = types,
                      selected = types)
  })

  # ── Map ──────────────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    build_map(filtered_data(), size_col = input$size_col)
  })

  # ── Table ────────────────────────────────────────────────────────────────────
  output$table <- renderDT({
    df <- filtered_data()
    cols <- intersect(TABLE_COLS, names(df))
    datatable(
      df[, cols, drop = FALSE],
      rownames  = FALSE,
      filter    = "top",
      options   = list(pageLength = 15, scrollX = TRUE)
    )
  })
}
