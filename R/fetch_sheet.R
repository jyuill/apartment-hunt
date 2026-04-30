library(googlesheets4)
library(dplyr)

#' Authenticate with Google Sheets using a service account JSON.
#' Locally: set GCP_SA_JSON to the file path of the JSON.
#' On Connect Cloud: set GCP_SA_JSON to the raw JSON string contents.
gs4_auth_sa <- function() {
  sa <- Sys.getenv("GCP_SA_JSON")
  if (nchar(sa) == 0) stop("GCP_SA_JSON environment variable is not set.")

  if (file.exists(sa)) {
    # Local dev: sa is a file path
    gs4_auth(path = sa)
  } else {
    # Connect Cloud: sa is the raw JSON string
    # Strip any surrounding whitespace or accidental outer quotes
    sa <- trimws(sa)
    sa <- gsub("^'|'$", "", sa)   # remove surrounding single quotes if any

    # Sanity check — log the type field to help diagnose auth issues
    type_match <- regmatches(sa, regexpr('"type"\\s*:\\s*"[^"]+"', sa))
    message("GCP_SA_JSON type field: ", if (length(type_match)) type_match else "NOT FOUND")

    tmp <- tempfile(fileext = ".json")
    cat(sa, file = tmp)
    on.exit(unlink(tmp), add = TRUE)
    gs4_auth(path = tmp)
  }
}

#' Read the apartment listings sheet.
#'
#' @param sheet_id Google Sheet ID string.
#' @return A data frame with all columns from the sheet.
fetch_sheet <- function(sheet_id) {
  gs4_auth_sa()

  # Headers are at row 8; read from row 8 onward
  sheet_name <- Sys.getenv("SHEET_NAME", unset = NA)
  range_spec  <- if (!is.na(sheet_name) && nchar(sheet_name) > 0) {
    googlesheets4::cell_limits(
      ul = c(8, 1), lr = c(NA, NA), sheet = sheet_name
    )
  } else {
    "8:10000"
  }
  df <- read_sheet(sheet_id, range = range_spec, col_types = "c")

  # Drop rows where Address is blank — these are empty trailing rows
  if ("Address" %in% names(df)) {
    df <- df[!is.na(df$Address) & nchar(trimws(df$Address)) > 0, ]
  }

  # Coerce known numeric columns, stripping currency formatting if present
  for (col in c("Rent", "Ttl_Cost", "Value_Cost")) {
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
