library(leaflet)
library(dplyr)
library(scales)

# ── Colour: Status ────────────────────────────────────────────────────────────
STATUS_PALETTE <- c(
  "open"    = "#2196F3",  # blue
  "tour"    = "#4CAF50",  # green
  "msg"     = "#FF9800",  # amber
  "unavail" = "#9E9E9E",  # grey
  "denied"  = "#F44336",  # red
  "reject"  = "#B71C1C",  # dark red
  "other"   = "#607D8B"   # blue-grey
)

# ── Shape: Type ───────────────────────────────────────────────────────────────
# leaflet supports awesomeMarkers icons; use font-awesome shapes via icon column,
# or distinguish via circle vs square via addCircleMarkers + addMarkers.
# Simplest cross-browser approach: use strokeDashArray to visually distinguish.
# studio  -> filled circle (dash = solid)
# 1 bdrm  -> ring / hollow circle (fillOpacity low, thick stroke)
# other   -> filled circle, grey

TYPE_STROKE <- list(
  "studio" = list(fill_opacity = 0.85, weight = 1,  dash = NULL),
  "1 bdrm" = list(fill_opacity = 0.15, weight = 3,  dash = NULL),
  "other"  = list(fill_opacity = 0.85, weight = 1,  dash = "4")
)

#' Build a leaflet map of apartment listings.
#'
#' @param df       Filtered data frame with lat, lng, Type, Status, size_col.
#' @param size_col Column name to use for marker size: "Rent" or "Ttl_Cost".
#' @return A leaflet map object.
build_map <- function(df, size_col = "Ttl_Cost") {

  # Drop rows without coordinates
  df <- df |> filter(!is.na(lat), !is.na(lng))
  if (nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }

  # ── Colour: Status ────────────────────────────────────────────────────────
  statuses    <- tolower(trimws(df$Status))
  status_norm <- ifelse(statuses %in% names(STATUS_PALETTE), statuses, "other")

  status_pal <- colorFactor(
    palette  = unname(STATUS_PALETTE),
    levels   = names(STATUS_PALETTE),
    na.color = STATUS_PALETTE[["other"]]
  )
  marker_col <- status_pal(status_norm)

  # ── Shape / stroke: Type ──────────────────────────────────────────────────
  types      <- tolower(trimws(df$Type))
  type_norm  <- ifelse(types %in% names(TYPE_STROKE), types, "other")

  fill_opacities <- sapply(type_norm, function(t) TYPE_STROKE[[t]]$fill_opacity)
  stroke_weights <- sapply(type_norm, function(t) TYPE_STROKE[[t]]$weight)
  stroke_dashes  <- sapply(type_norm, function(t) {
    d <- TYPE_STROKE[[t]]$dash; if (is.null(d)) "" else d
  })

  # ── Size: Rent or Ttl_Cost ────────────────────────────────────────────────
  size_vals <- as.numeric(df[[size_col]])
  size_vals[is.na(size_vals)] <- min(size_vals, na.rm = TRUE)
  radii <- rescale(size_vals, to = c(6, 18))

  # ── Popups ────────────────────────────────────────────────────────────────
  popup_cols <- c("Apartment", "Status", "Type", "Rent", "Ttl_Cost",
                  "Parking_EV", "Laundry", "Gym", "Storage")
  popup_cols <- intersect(popup_cols, names(df))

  popups <- apply(df[, popup_cols, drop = FALSE], 1, function(row) {
    rows_html <- paste0(
      "<tr><td><b>", popup_cols, "</b></td><td>&nbsp;", row[popup_cols], "</td></tr>",
      collapse = ""
    )
    paste0("<table style='font-size:12px'>", rows_html, "</table>")
  })

  # ── Map ───────────────────────────────────────────────────────────────────
  # Build per-type legend HTML for shape key
  type_legend_html <- paste0(
    "<div style='font-size:12px'>",
    "<b>Type</b><br>",
    "<svg width='14' height='14'><circle cx='7' cy='7' r='6' fill='#888' fill-opacity='0.85' stroke='#555' stroke-width='1'/></svg> studio<br>",
    "<svg width='14' height='14'><circle cx='7' cy='7' r='6' fill='#888' fill-opacity='0.15' stroke='#555' stroke-width='3'/></svg> 1 bdrm",
    "</div>"
  )

  leaflet(df) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    fitBounds(
      lng1 = min(df$lng, na.rm = TRUE) - 0.01,
      lat1 = min(df$lat, na.rm = TRUE) - 0.01,
      lng2 = max(df$lng, na.rm = TRUE) + 0.01,
      lat2 = max(df$lat, na.rm = TRUE) + 0.01
    ) |>
    addCircleMarkers(
      lng          = ~lng,
      lat          = ~lat,
      radius       = radii,
      color        = marker_col,
      fillColor    = marker_col,
      fillOpacity  = fill_opacities,
      opacity      = 1,
      stroke       = TRUE,
      weight       = stroke_weights,
      dashArray    = stroke_dashes,
      popup        = popups
    ) |>
    addLegend(
      position = "bottomright",
      colors   = unname(STATUS_PALETTE),
      labels   = names(STATUS_PALETTE),
      title    = "Status",
      opacity  = 0.9
    ) |>
    addControl(
      html     = type_legend_html,
      position = "bottomleft"
    )
}
