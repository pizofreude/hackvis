# In scripts/utils_data.R
# ------------------------------------------------------------------
# Utility helpers used by the HackVis app for robust data handling.
# Keep this file free of side-effects: only function definitions.

is_categorical <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

numeric_cols <- function(df) {
  names(df)[vapply(df, is.numeric, logical(1))]
}

categorical_cols <- function(df) {
  names(df)[vapply(df, is_categorical, logical(1))]
}

datetime_cols <- function(df) {
  names(df)[vapply(df, lubridate::is.POSIXct, logical(1))]
}

require_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("The CSV is missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

coerce_types <- function(df) {
  out <- df
  numeric_targets <- intersect(c("score", "votes", "lines_of_code"), names(out))
  for (nm in numeric_targets) {
    if (!is.numeric(out[[nm]])) {
      old <- out[[nm]]
      out[[nm]] <- suppressWarnings(as.numeric(old))
    }
  }
  if ("submitted_at" %in% names(out) && !lubridate::is.POSIXct(out$submitted_at)) {
    out$submitted_at <- suppressWarnings(readr::parse_datetime(out$submitted_at))
  }
  out
}

impute_numeric <- function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)

describe_df <- function(df) {
  tibble::tibble(
    column   = names(df),
    class    = vapply(df, \(x) paste(class(x), collapse = "/"), character(1)),
    n_na     = vapply(df, \(x) sum(is.na(x)), integer(1)),
    n_unique = vapply(df, \(x) dplyr::n_distinct(x), integer(1))
  ) |>
    mutate(na_rate = round(100 * n_na / nrow(df), 1))
}