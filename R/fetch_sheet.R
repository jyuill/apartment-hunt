library(googlesheets4)
library(dplyr)

#' Authenticate with Google Sheets using a service account JSON.
#' Locally: set GCP_SA_JSON to the file path of the JSON.
#' On Connect Cloud: set GCP_SA_JSON to the raw JSON string contents.
gs4_auth_sa <- function() {
  sa <- Sys.getenv("GCP_SA_JSON")
  if (nchar(sa) == 0) stop("GCP_SA_JSON environment variable is not set.")

  # Accept either a file path or a raw JSON string
  if (file.exists(sa)) {
    gs4_auth(path = sa)
  } else {
    gs4_auth(path = jsonlite::fromJSON(sa, simplifyVector = FALSE))
  }
}

#' Read the apartment listings sheet.
#'
#' @param sheet_id Google Sheet ID string.
#' @return A data frame with all columns from the sheet.
fetch_sheet <- function(sheet_id) {
  gs4_auth_sa()

  # Headers are at row 8; read from row 8 onward
  df <- read_sheet(sheet_id, range = "8:10000", col_types = "c")

  # Coerce known numeric columns, stripping currency formatting if present
  for (col in c("Rent", "Ttl_Cost")) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(gsub("[$,]", "", df[[col]]))
    }
  }

  # Coerce known boolean columns
  bool_cols <- c("Parking_EV", "Laundry", "Gym", "Amenities")
  for (col in bool_cols) {
    if (col %in% names(df)) {
      df[[col]] <- df[[col]] %in% c("TRUE", "true", "True", "1", "yes", "Yes")
    }
  }

  # Coerce lat/lng to numeric
  for (col in c("lat", "lng")) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }

  df
}
