source("R/fetch_sheet.R")
source("R/geocode_writeback.R")
source("R/build_map.R")

df <- fetch_sheet(Sys.getenv("SHEET_ID"))
df <- geocode_writeback(df, Sys.getenv("SHEET_ID"))

# Simulate exactly what server.R does
status_filter <- c("tour", "open", "msg", "ref")
type_filter   <- c("studio", "1 bdrm")
size_col      <- "Ttl_Cost"

cat("=== Before filter ===\n")
cat("Rows:", nrow(df), "  NA lat:", sum(is.na(df$lat)), "\n")

# Status
df2 <- df[tolower(df$Status) %in% tolower(status_filter), ]
cat("After status filter:", nrow(df2), "\n")

# Type
df2 <- df2[tolower(trimws(df2$Type)) %in% tolower(type_filter), ]
cat("After type filter:", nrow(df2), "\n")

# Simulate what build_map does step by step
df3 <- df2[!is.na(df2$lat) & !is.na(df2$lng), ]
cat("After dropping NA coords:", nrow(df3), "\n")

cat("\nsize_col value:", size_col, "\n")
cat("size_col is NULL:", is.null(size_col), "\n")
size_vals <- as.numeric(df3[[size_col]])
cat("size_vals:", size_vals, "\n")

cat("\nType values in filtered data:", unique(df3$Type), "\n")
type_norm <- tolower(trimws(df3$Type))
cat("type_norm:", type_norm, "\n")

fill_opacities <- sapply(type_norm, function(t) {
  TYPE_STROKE <- list(
    "studio" = list(fill_opacity = 0.85, weight = 1),
    "1 bdrm" = list(fill_opacity = 0.15, weight = 3),
    "other"  = list(fill_opacity = 0.85, weight = 1)
  )
  tv <- if (t %in% names(TYPE_STROKE)) t else "other"
  TYPE_STROKE[[tv]]$fill_opacity
})
cat("fill_opacities:", fill_opacities, "\n")
cat("fill_opacities named?", !is.null(names(fill_opacities)), "\n")
