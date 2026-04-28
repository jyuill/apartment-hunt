library(leaflet)
library(dplyr)
library(scales)

STATUS_PALETTE <- c(
  "open"    = "#2196F3",
  "tour"    = "#4CAF50",
  "msg"     = "#FF9800",
  "ref"     = "#00BCD4",
  "unavail" = "#9E9E9E",
  "denied"  = "#F44336",
  "reject"  = "#B71C1C",
  "other"   = "#607D8B"
)

TYPE_STROKE <- list(
  "studio" = list(fill_opacity = 0.85, weight = 1),
  "1 bdrm" = list(fill_opacity = 0.15, weight = 3),
  "other"  = list(fill_opacity = 0.85, weight = 1)
)

build_map <- function(df, size_col = "Ttl_Cost") {

  if (is.null(df) || nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }
  if (is.null(size_col) || !size_col %in% names(df)) size_col <- "Ttl_Cost"

  df <- df[!is.na(df$lat) & !is.na(df$lng), ]
  if (nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }

  n <- nrow(df)

  # Colour from Status
  status_norm <- tolower(trimws(df$Status))
  status_norm[!status_norm %in% names(STATUS_PALETTE)] <- "other"
  status_pal <- colorFactor(unname(STATUS_PALETTE),
                             levels   = names(STATUS_PALETTE),
                             na.color = STATUS_PALETTE[["other"]])
  marker_col <- as.character(status_pal(status_norm))

  # Shape from Type using explicit loop to avoid named vectors from sapply
  type_norm <- tolower(trimws(df$Type))
  type_norm[!type_norm %in% names(TYPE_STROKE)] <- "other"
  fill_opacities <- numeric(n)
  stroke_weights <- numeric(n)
  for (i in seq_len(n)) {
    fill_opacities[i] <- TYPE_STROKE[[type_norm[i]]]$fill_opacity
    stroke_weights[i] <- TYPE_STROKE[[type_norm[i]]]$weight
  }

  # Size from cost column
  size_vals <- as.numeric(df[[size_col]])
  size_vals[is.na(size_vals)] <- min(size_vals, na.rm = TRUE)
  radii <- as.numeric(scales::rescale(size_vals, to = c(6, 18)))

  # Popups
  popup_cols <- intersect(
    c("Apartment", "Status", "Type", "Rent", "Ttl_Cost",
      "Parking_EV", "Laundry", "Gym", "Storage"),
    names(df)
  )
  popups <- vapply(seq_len(n), function(i) {
    rows_html <- paste0(
      "<tr><td><b>", popup_cols, "</b></td><td>&nbsp;",
      df[i, popup_cols], "</td></tr>",
      collapse = ""
    )
    paste0("<table style='font-size:12px'>", rows_html, "</table>")
  }, character(1))

  type_legend_html <- paste0(
    "<div style='font-size:12px;background:white;padding:6px;border-radius:4px'>",
    "<b>Type</b><br>",
    "<svg width='14' height='14'><circle cx='7' cy='7' r='6' fill='#888'",
    " fill-opacity='0.85' stroke='#555' stroke-width='1'/></svg>&nbsp;studio<br>",
    "<svg width='14' height='14'><circle cx='7' cy='7' r='6' fill='#888'",
    " fill-opacity='0.15' stroke='#555' stroke-width='3'/></svg>&nbsp;1 bdrm",
    "</div>"
  )

  leaflet(df) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    fitBounds(
      lng1 = min(df$lng) - 0.01, lat1 = min(df$lat) - 0.01,
      lng2 = max(df$lng) + 0.01, lat2 = max(df$lat) + 0.01
    ) |>
    addCircleMarkers(
      lng         = ~lng,
      lat         = ~lat,
      radius      = radii,
      color       = marker_col,
      fillColor   = marker_col,
      fillOpacity = fill_opacities,
      opacity     = 1,
      stroke      = TRUE,
      weight      = stroke_weights,
      popup       = popups
    ) |>
    addLegend(
      position = "bottomright",
      colors   = unname(STATUS_PALETTE),
      labels   = names(STATUS_PALETTE),
      title    = "Status",
      opacity  = 0.9
    ) |>
    addControl(html = type_legend_html, position = "bottomleft")
}
