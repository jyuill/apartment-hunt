library(leaflet)
library(dplyr)
library(scales)

STATUS_PALETTE <- c(
  "open"    = "#2196F3",
  "tour"    = "#4CAF50",
  "msg"     = "#FF9800",
  "ref"     = "#00BCD4",
  "applied" = "#9C27B0",
  "decide"  = "#FF5722",
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

# Named palette for Essentials service types — add entries as new types appear
ESSENTIALS_PALETTE <- c(
  "grocery"    = "#1A1A1A",   # near-black
  "gym"        = "#AAAAAA",   # light grey
  "transit"    = "#4A4A4A",   # mid-dark grey
  "pharmacy"   = "#6B6B6B",   # mid grey
  "café"       = "#3D3D3D",
  "cafe"       = "#3D3D3D",
  "restaurant" = "#5C5C5C",
  "park"       = "#7A7A7A",
  "other"      = "#8C8C8C"
)

build_map <- function(df, size_col = "Ttl_Cost", color_col = "Status", essentials = NULL, zones = NULL) {

  if (is.null(df) || nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }
  if (is.null(size_col)  || !size_col  %in% names(df)) size_col  <- "Ttl_Cost"
  if (is.null(color_col) || !color_col %in% names(df)) color_col <- "Status"

  df <- df[!is.na(df$lat) & !is.na(df$lng), ]
  if (nrow(df) == 0) {
    return(leaflet() |> addTiles() |>
             setView(lng = -123.1207, lat = 49.2827, zoom = 12))
  }

  n <- nrow(df)

  # Colour: Status (categorical) or value_cost (continuous, lower = better = green)
  if (color_col == "Value_Cost") {
    vc_raw <- as.numeric(df$Value_Cost)
    vc_domain <- range(vc_raw, na.rm = TRUE)
    # Fall back to Status colouring if no valid values
    if (any(!is.finite(vc_domain))) color_col <- "Status"
  }

  if (color_col == "Value_Cost") {
    med_vc <- median(vc_raw, na.rm = TRUE)
    vc <- ifelse(is.na(vc_raw), med_vc, vc_raw)
    vc_pal <- colorNumeric(
      palette = c("#4CAF50", "#FF9800", "#F44336"),  # green → amber → red
      domain  = vc_domain,
      na.color = "#9E9E9E"
    )
    marker_col   <- unname(as.character(vc_pal(vc)))
    legend_pal   <- vc_pal
    legend_vals  <- vc_domain
    legend_title <- "Value-Cost"
  } else {
    status_norm <- tolower(trimws(df$Status))
    status_norm[!status_norm %in% names(STATUS_PALETTE)] <- "other"
    status_pal <- colorFactor(unname(STATUS_PALETTE),
                               levels   = names(STATUS_PALETTE),
                               na.color = STATUS_PALETTE[["other"]])
    marker_col  <- unname(as.character(status_pal(status_norm)))
    legend_type <- "factor"
    legend_pal  <- status_pal
    legend_vals <- factor(status_norm, levels = names(STATUS_PALETTE))
    legend_title <- "Status"
  }

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
    c("Apartment", "Address", "Status", "Type", "Rent", "Ttl_Cost", "Value_Cost",
      "Parking_EV", "Laundry", "Gym", "Storage", "Notes"),
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

  m <- leaflet(df) |>
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
      pal      = legend_pal,
      values   = legend_vals,
      title    = legend_title,
      opacity  = 0.9
    ) |>
    addControl(html = type_legend_html, position = "bottomleft")

  # Essentials overlay
  if (!is.null(essentials) && nrow(essentials) > 0) {
    ess <- essentials[!is.na(essentials$lat) & !is.na(essentials$lng), ]
    if (nrow(ess) > 0) {
      svcs    <- sort(unique(trimws(ess$Service)))
      # Map each service to a colour, falling back to "other" for unknowns
      svc_norm <- tolower(trimws(ess$Service))
      pal_colours <- vapply(svcs, function(s) {
        key <- tolower(s)
        if (key %in% names(ESSENTIALS_PALETTE)) ESSENTIALS_PALETTE[[key]]
        else ESSENTIALS_PALETTE[["other"]]
      }, character(1))
      ess_pal <- colorFactor(unname(pal_colours), levels = svcs, na.color = "#AAAAAA")
      ess_col <- unname(as.character(ess_pal(trimws(ess$Service))))
      ess_popup <- paste0(
        "<b>", ess$Name, "</b><br>",
        "<i>", ess$Service, "</i><br>",
        "<small>", ess$Address, "</small>"
      )
      m <- m |>
        addCircleMarkers(
          data        = ess,
          lng         = ~lng,
          lat         = ~lat,
          radius      = 3,
          color       = ess_col,
          fillColor   = ess_col,
          fillOpacity = 0.8,
          opacity     = 1,
          weight      = 1,
          popup       = ess_popup
        ) |>
        addLegend(
          position = "topright",
          pal      = ess_pal,
          values   = factor(trimws(ess$Service), levels = svcs),
          title    = "Essentials",
          opacity  = 0.9
        )
    }
  }

  # Zones overlay
  if (!is.null(zones) && nrow(zones) > 0) {
    req_cols <- c("lat_ne", "lng_ne", "lat_se", "lng_se",
                  "lat_sw", "lng_sw", "lat_nw", "lng_nw")
    zones_valid <- zones[rowSums(is.na(zones[, intersect(req_cols, names(zones))])) == 0, ]
    if (nrow(zones_valid) > 0) {
      for (i in seq_len(nrow(zones_valid))) {
        z <- zones_valid[i, ]
        lats <- c(z$lat_ne, z$lat_se, z$lat_sw, z$lat_nw, z$lat_ne)
        lngs <- c(z$lng_ne, z$lng_se, z$lng_sw, z$lng_nw, z$lng_ne)
        m <- m |> addPolylines(
          lng    = lngs,
          lat    = lats,
          color  = "#555555",
          weight = 2,
          opacity = 0.8,
          label  = if ("zone_name" %in% names(z)) z$zone_name else NULL
        )
      }
    }
  }

  m
}
