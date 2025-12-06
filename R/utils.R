#' Utility Functions
#'
#' @name utils
#' @description Helper utilities for oxidizr package
NULL

#' Check Required Columns
#'
#' Internal helper to validate that required columns exist in a data frame.
#'
#' @param df Data frame to check
#' @param required Character vector of required column names
#' @param context Context string for error message
#'
#' @return TRUE if all columns present, otherwise throws error
#' @keywords internal
check_required_cols <- function(df, required, context = "data") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(
      "Missing required columns in {context}: {paste(missing, collapse = ', ')}"
    )
  }
  TRUE
}

#' Safe Column Selection
#'
#' Select columns that exist, ignoring missing ones.
#'
#' @param df Data frame
#' @param cols Column names to select
#'
#' @return Data frame with available columns
#' @keywords internal
safe_select <- function(df, cols) {
  available <- intersect(cols, names(df))
  df[, available, drop = FALSE]
}

#' Convert Time to Numeric
#'
#' Safely convert a time column to numeric.
#'
#' @param x Time vector (can be factor, character, or numeric)
#'
#' @return Numeric vector
#' @export
as_numeric_time <- function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))
  } else if (is.character(x)) {
    as.numeric(x)
  } else {
    as.numeric(x)
  }
}

#' Format Mean +/- SD
#'
#' Format a mean and standard deviation as "mean +/- sd".
#'
#' @param mean Mean value
#' @param sd Standard deviation
#' @param decimals Number of decimal places
#' @param sep Separator (default: " +/- ")
#'
#' @return Character string
#' @export
format_mean_sd <- function(mean, sd, decimals = 2, sep = " \u00b1 ") {
  paste0(
    format(round(mean, decimals), nsmall = decimals),
    sep,
    format(round(sd, decimals), nsmall = decimals)
  )
}

#' Validate Oxidation Results
#'
#' Check that oxidation values are within reasonable ranges.
#'
#' @param ox Data frame with oxidation rates
#' @param warn_only If TRUE, only warn instead of error
#'
#' @return Logical indicating if valid
#' @keywords internal
validate_oxidation_values <- function(ox, warn_only = TRUE) {

  issues <- character()

  # Check for negative values
  rate_cols <- c("cho_total", "fat_total", "protein_ox", "cho_exo", "cho_endo")
  for (col in intersect(rate_cols, names(ox))) {
    if (any(ox[[col]] < 0, na.rm = TRUE)) {
      issues <- c(issues, glue::glue("Negative values in {col}"))
    }
  }

  # Check for unrealistic RER values
  if ("rer" %in% names(ox)) {
    if (any(ox$rer < 0.7 | ox$rer > 1.1, na.rm = TRUE)) {
      issues <- c(issues, "RER values outside typical range (0.7-1.1)")
    }
  }

  # Check for unrealistic oxidation rates
  if ("cho_total" %in% names(ox)) {
    if (any(ox$cho_total > 5, na.rm = TRUE)) {
      issues <- c(issues, "CHO oxidation > 5 g/min (unusually high)")
    }
  }

  if (length(issues) > 0) {
    if (warn_only) {
      cli::cli_warn("Potential issues detected: {paste(issues, collapse = '; ')}")
      return(TRUE)
    } else {
      cli::cli_abort("Validation failed: {paste(issues, collapse = '; ')}")
    }
  }

  TRUE
}

#' List Available Columns
#'
#' Print available columns in oxidation results for reference.
#'
#' @param results An OxidationResults object
#'
#' @return Invisible NULL (prints to console)
#' @export
list_columns <- function(results) {

  if (S7_inherits(results, OxidationResults)) {
    cli::cli_h2("Oxidation Rates")
    cli::cli_ul(names(results@oxidation_rates))

    if (!is.null(results@energy_contributions)) {
      cli::cli_h2("Energy Contributions")
      cli::cli_ul(names(results@energy_contributions))
    }

    if (!is.null(results@partition)) {
      cli::cli_h2("CHO Partition")
      cli::cli_ul(names(results@partition))
    }
  } else {
    cli::cli_ul(names(results))
  }

  invisible(NULL)
}

#' Quick Summary of Oxidation Results
#'
#' Print a quick summary of key oxidation metrics.
#'
#' @param results An OxidationResults object or data frame
#' @param by Optional grouping variable
#'
#' @return Invisible NULL (prints summary to console)
#' @export
quick_summary <- function(results, by = NULL) {

  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
    energy <- results@energy_contributions
  } else {
    df <- results
    energy <- NULL
  }

  cli::cli_h1("Oxidation Summary")

  # Basic stats
  n_obs <- nrow(df)
  cli::cli_alert_info("Total observations: {n_obs}")

  # Rate summaries
  if ("cho_total" %in% names(df)) {
    cho_mean <- mean(df$cho_total, na.rm = TRUE)
    cho_sd <- sd(df$cho_total, na.rm = TRUE)
    cli::cli_alert_info("CHO oxidation: {format_mean_sd(cho_mean, cho_sd)} g/min")
  }

  if ("fat_total" %in% names(df)) {
    fat_mean <- mean(df$fat_total, na.rm = TRUE)
    fat_sd <- sd(df$fat_total, na.rm = TRUE)
    cli::cli_alert_info("Fat oxidation: {format_mean_sd(fat_mean, fat_sd)} g/min")
  }

  if (!is.null(energy) && "pct_cho_total" %in% names(energy)) {
    pct_cho <- mean(energy$pct_cho_total, na.rm = TRUE)
    pct_fat <- mean(energy$pct_fat, na.rm = TRUE)
    cli::cli_alert_info("Energy from CHO: {round(pct_cho, 1)}%")
    cli::cli_alert_info("Energy from fat: {round(pct_fat, 1)}%")
  }

  if ("cho_exo" %in% names(df)) {
    exo_mean <- mean(df$cho_exo, na.rm = TRUE)
    exo_sd <- sd(df$cho_exo, na.rm = TRUE)
    cli::cli_alert_info("Exogenous CHO: {format_mean_sd(exo_mean, exo_sd)} g/min")
  }

  invisible(NULL)
}
