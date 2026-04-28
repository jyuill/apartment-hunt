library(tidygeocoder)
library(googlesheets4)
library(dplyr)

#' Geocode any rows with missing lat/lng and write results back to the sheet.
#'
#' Rows where `lat` is already populated are skipped.
#' After geocoding new rows, the entire `lat` and `lng` columns are
#' written back to the sheet so coordinates persist across sessions.
#'
#' @param df        Data frame returned by fetch_sheet().
#' @param sheet_id  Google Sheet ID string.
#' @return df with `lat` and `lng` columns fully populated for geocodeable rows.
geocode_writeback <- function(df, sheet_id) {

  if (!all(c("lat", "lng") %in% names(df))) {
    stop("Sheet must contain 'lat' and 'lng' columns.")
  }

  # Only attempt geocoding on rows that have an address
  has_address    <- !is.na(df$Address) & nchar(trimws(df$Address)) > 0
  needs_geocode  <- has_address & (is.na(df$lat) | df$lat == "")

  if (!any(needs_geocode)) {
    message("No new addresses to geocode.")
    return(df)
  }

  n_new <- sum(needs_geocode)
  message(sprintf("Geocoding %d new address(es)...", n_new))

  # Build full address strings for geocoding.
  # Uses a City column if present, otherwise appends Vancouver, BC, Canada.
  addr_suffix <- if ("City" %in% names(df)) {
    paste0(", ", df$City[needs_geocode])
  } else {
    ", Vancouver, BC, Canada"
  }

  to_geocode <- data.frame(
    address = paste0(df$Address[needs_geocode], addr_suffix),
    stringsAsFactors = FALSE
  ) |>
    geocode(address = address, method = "osm", lat = lat_new, long = lng_new)

  # Write results back into df
  df$lat[needs_geocode] <- to_geocode$lat_new
  df$lng[needs_geocode] <- to_geocode$lng_new

  # Ensure numeric type (mixing NA and numeric via indexing can produce character)
  df$lat <- as.numeric(df$lat)
  df$lng <- as.numeric(df$lng)

  # Write the full lat/lng columns back to the sheet
  # Determine column letters for lat and lng
  col_names  <- names(df)
  lat_col    <- which(col_names == "lat")
  lng_col    <- which(col_names == "lng")

  lat_range  <- gs4_range_col(lat_col, nrow(df))
  lng_range  <- gs4_range_col(lng_col, nrow(df))

  sheet_name <- Sys.getenv("SHEET_NAME", unset = NA)
  sheet_arg  <- if (!is.na(sheet_name) && nchar(sheet_name) > 0) sheet_name else NULL

  range_write(sheet_id,
              data      = data.frame(lat = df$lat),
              range     = lat_range,
              sheet     = sheet_arg,
              col_names = FALSE,
              reformat  = FALSE)

  range_write(sheet_id,
              data      = data.frame(lng = df$lng),
              range     = lng_range,
              sheet     = sheet_arg,
              col_names = FALSE,
              reformat  = FALSE)

  message("lat/lng written back to sheet.")
  df
}

#' Build an A1-notation range string for a single column (data rows only).
#' Headers are at row 8, so data starts at row 9.
#'
#' @param col_index  1-based column index.
#' @param n_rows     Number of data rows (excluding header).
#' @param header_row Row number of the header (default 8).
#' @return A1 range string, e.g. "E9:E58"
gs4_range_col <- function(col_index, n_rows, header_row = 8) {
  col_letter <- function(n) {
    s <- ""
    while (n > 0) {
      r <- (n - 1) %% 26
      s <- paste0(LETTERS[r + 1], s)
      n <- (n - 1) %/% 26
    }
    s
  }
  letter     <- col_letter(col_index)
  data_start <- header_row + 1
  data_end   <- header_row + n_rows
  sprintf("%s%d:%s%d", letter, data_start, letter, data_end)
}
