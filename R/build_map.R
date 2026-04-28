library(leaflet)
library(dplyr)
library(scales)

# Colour palette for Type
TYPE_PALETTE <- c(
  "apartment" = "#2196F3",
  "condo"     = "#4CAF50",
  "house"     = "#FF9800",
  "townhouse" = "#9C27B0",
  "studio"    = "#F44336",
  "other"     = "#607D8B"
)

#' Build a leaflet map of apartment listings.
#'
#' @param df       Filtered data frame with lat, lng, Type, and size_col present.
#' @param size_col Column name to use for marker size: "Rent" or "Ttl_Cost".
#' @return A leaflet map object.
build_map <- function(df, size_col = "Rent") {

  # Drop rows without coordinates
  df <- df |> filter(!is.na(lat), !is.na(lng))
  if (nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }

  # ── Colour: Type ──────────────────────────────────────────────────────────
  types       <- tolower(trimws(df$Type))
  known_types <- names(TYPE_PALETTE)
  types_norm  <- ifelse(types %in% known_types, types, "other")

  pal <- colorFactor(
    palette = unname(TYPE_PALETTE),
    levels  = known_types,
    na.color = TYPE_PALETTE[["other"]]
  )
  marker_col <- pal(types_norm)

  # ── Size: Rent or Ttl_Cost ────────────────────────────────────────────────
  size_vals <- df[[size_col]]
  size_vals[is.na(size_vals)] <- min(size_vals, na.rm = TRUE)
  radii <- rescale(size_vals, to = c(6, 18))

  # ── Popups ────────────────────────────────────────────────────────────────
  popup_cols <- c("Apartment", "Status", "Rent", "Ttl_Cost",
                  "Parking_EV", "Laundry", "Gym", "Storage", "Type")
  popup_cols <- intersect(popup_cols, names(df))

  popups <- apply(df[, popup_cols, drop = FALSE], 1, function(row) {
    rows_html <- paste0(
      "<tr><td><b>", popup_cols, "</b></td><td>", row[popup_cols], "</td></tr>",
      collapse = ""
    )
    paste0("<table>", rows_html, "</table>")
  })

  # ── Map ───────────────────────────────────────────────────────────────────
  leaflet(df) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    fitBounds(
      lng1 = min(df$lng, na.rm = TRUE) - 0.01,
      lat1 = min(df$lat, na.rm = TRUE) - 0.01,
      lng2 = max(df$lng, na.rm = TRUE) + 0.01,
      lat2 = max(df$lat, na.rm = TRUE) + 0.01
    ) |>
    addCircleMarkers(
      lng    = ~lng,
      lat    = ~lat,
      radius = radii,
      color  = marker_col,
      fillColor   = marker_col,
      fillOpacity = 0.8,
      opacity     = 1,
      stroke      = TRUE,
      weight      = 1,
      popup  = popups
    ) |>
    addLegend(
      position = "bottomright",
      colors   = unname(TYPE_PALETTE),
      labels   = names(TYPE_PALETTE),
      title    = "Type",
      opacity  = 0.9
    )
}
