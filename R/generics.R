#' S7 Generic Functions for Oxidation Analysis
#'
#' @name oxidizr-generics
#' @description S7 generic function definitions and method implementations
NULL

# -----------------------------------------------------------------------------
# Print Methods
# -----------------------------------------------------------------------------

#' @export
method(print, CalorimetryData) <- function(x, ...) {
  n_subjects <- length(unique(x@data[[x@id_col]]))
  n_timepoints <- length(unique(x@data[[x@time_col]]))
  n_obs <- nrow(x@data)

  cli::cli_h1("CalorimetryData")
  cli::cli_bullets(c(
    "*" = "Subjects: {n_subjects}",
    "*" = "Time points: {n_timepoints}",
    "*" = "Observations: {n_obs}",
    "*" = "VO2 unit: {x@vo2_unit}"
  ))
  if (!is.null(x@protocol_col)) {
    protocols <- unique(x@data[[x@protocol_col]])
    cli::cli_bullets(c(
      "*" = "Protocols: {paste(protocols, collapse = ', ')}"
    ))
  }
  invisible(x)
}

#' @export
method(print, IsotopeData) <- function(x, ...) {
  n_rexp <- nrow(x@rexp)
  n_rexo <- nrow(x@rexo)
  has_rpla <- !is.null(x@rpla)

  cli::cli_h1("IsotopeData")
  cli::cli_bullets(c(
    "*" = "Rexp observations: {n_rexp}",
    "*" = "Rexo observations: {n_rexo}",
    "*" = "Plasma enrichment (Rpla): {if(has_rpla) 'Yes' else 'No'}"
  ))
  invisible(x)
}

#' @export
method(print, OxidationStudy) <- function(x, ...) {
  cli::cli_h1("OxidationStudy")
  cli::cli_h2("Components")

  # Calorimetry
  n_calo <- nrow(x@calorimetry@data)
  cli::cli_bullets(c("v" = "Calorimetry: {n_calo} observations"))

  # Isotopes
  if (!is.null(x@isotopes)) {
    cli::cli_bullets(c("v" = "Isotope data: present"))
  } else {
    cli::cli_bullets(c("x" = "Isotope data: not provided"))
  }

  # Urea
  if (!is.null(x@urea)) {
    cli::cli_bullets(c("v" = "Urea data: present"))
  } else {
    cli::cli_bullets(c("x" = "Urea data: not provided"))
  }

  # Environment
  if (!is.null(x@environment)) {
    cli::cli_bullets(c("v" = "Environment data: present"))
  } else {
    cli::cli_bullets(c("x" = "Environment data: not provided"))
  }

  # Subjects
  if (!is.null(x@subjects)) {
    n_subj <- nrow(x@subjects@data)
    cli::cli_bullets(c("v" = "Subject data: {n_subj} records"))
  } else {
    cli::cli_bullets(c("x" = "Subject data: not provided"))
  }

  # Protocols
  if (!is.null(x@protocols)) {
    cli::cli_bullets(c(
      "*" = "Protocols: {paste(levels(x@protocols), collapse = ', ')}"
    ))
  }

  invisible(x)
}

#' @export
method(print, OxidationResults) <- function(x, ...) {
  n_obs <- nrow(x@oxidation_rates)

  cli::cli_h1("OxidationResults")
  cli::cli_bullets(c(
    "*" = "Oxidation rates: {n_obs} observations"
  ))

  if (!is.null(x@energy_contributions)) {
    cli::cli_bullets(c("v" = "Energy contributions: calculated"))
  }

  if (!is.null(x@partition)) {
    cli::cli_bullets(c("v" = "CHO partitioning: calculated"))
  }

  # Show available substrates
  rate_cols <- names(x@oxidation_rates)
  substrates <- rate_cols[rate_cols %in% c("cho_total", "fat_total", "protein",
                                            "cho_exo", "cho_endo", "cho_mus", "cho_liv")]
  if (length(substrates) > 0) {
    cli::cli_bullets(c(
      "*" = "Substrates: {paste(substrates, collapse = ', ')}"
    ))
  }

  invisible(x)
}

#' @export
method(print, ValidationResult) <- function(x, ...) {
  cli::cli_h1("ValidationResult")

  # Status indicator
  if (x@passed) {
    cli::cli_alert_success("Validation PASSED (no errors)")
  } else {
    cli::cli_alert_danger("Validation FAILED ({x@severity_summary$error} error(s))")
  }

  # Severity summary
  cli::cli_h2("Issues Summary")
  cli::cli_bullets(c(
    "!" = "Errors: {x@severity_summary$error %||% 0}",
    "!" = "Warnings: {x@severity_summary$warning %||% 0}",
    "i" = "Info: {x@severity_summary$info %||% 0}"
  ))

  # Show issues by category if any
  if (nrow(x@issues) > 0) {
    cli::cli_h2("Issues by Category")
    categories <- unique(x@issues$category)
    for (cat in categories) {
      cat_issues <- x@issues[x@issues$category == cat, ]
      n_errors <- sum(cat_issues$severity == "error")
      n_warnings <- sum(cat_issues$severity == "warning")
      cli::cli_bullets(c(
        "*" = "{cat}: {n_errors} error(s), {n_warnings} warning(s)"
      ))
    }
  }

  # Recommendations
  if (length(x@recommendations) > 0) {
    cli::cli_h2("Recommendations")
    for (rec in x@recommendations) {
      cli::cli_bullets(c(">" = rec))
    }
  }

  # Timestamp
  if (!is.null(x@timestamp)) {
    cli::cli_text(cli::col_grey("Validated: {format(x@timestamp, '%Y-%m-%d %H:%M:%S')}"))
  }

  invisible(x)
}

# -----------------------------------------------------------------------------
# Summary Methods
# -----------------------------------------------------------------------------

#' @export
method(summary, OxidationResults) <- function(object, by = NULL, ...) {
  df <- object@oxidation_rates

  # Identify numeric columns for summarizing
  rate_cols <- c("cho_total", "fat_total", "protein",
                 "cho_exo", "cho_endo", "cho_mus", "cho_liv")
  available_cols <- intersect(rate_cols, names(df))

  if (is.null(by)) {
    # Overall summary
    summary_df <- df |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(available_cols),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        n = dplyr::n()
      )
  } else {
    # Summary by group
    if (!by %in% names(df)) {
      cli::cli_abort("Column '{by}' not found in oxidation_rates")
    }
    summary_df <- df |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(available_cols),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        n = dplyr::n()
      ) |>
      dplyr::ungroup()
  }

  summary_df
}

#' @export
method(summary, ValidationResult) <- function(object, ...) {
  issues <- object@issues

  if (nrow(issues) == 0) {
    cli::cli_alert_success("No validation issues found")
    return(invisible(tibble::tibble()))
  }

  # Summary by category and severity
  summary_df <- issues |>
    dplyr::group_by(.data$category, .data$severity) |>
    dplyr::summarise(
      n_issues = dplyr::n(),
      n_affected = sum(.data$n_affected),
      variables = paste(unique(.data$variable), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$severity == "error"),
      dplyr::desc(.data$severity == "warning"),
      .data$category
    )

  summary_df
}

# -----------------------------------------------------------------------------
# Data Extraction Methods
# -----------------------------------------------------------------------------

#' Extract data from oxidizr objects
#'
#' Generic function to extract the underlying data frame from oxidizr S7 objects.
#'
#' @param x An oxidizr S7 object
#' @param ... Additional arguments
#'
#' @return A tibble/data frame
#' @export
get_data <- new_generic("get_data", "x")

#' @export
method(get_data, CalorimetryData) <- function(x, ...) {
  tibble::as_tibble(x@data)
}

#' @export
method(get_data, UreaData) <- function(x, ...) {

  tibble::as_tibble(x@data)
}

#' @export
method(get_data, EnvironmentData) <- function(x, ...) {
  tibble::as_tibble(x@data)
}

#' @export
method(get_data, SubjectData) <- function(x, ...) {
  tibble::as_tibble(x@data)
}

#' @export
method(get_data, OxidationResults) <- function(x, what = c("rates", "energy", "partition"), ...) {
  what <- match.arg(what)
  result <- switch(what,
    rates = x@oxidation_rates,
    energy = x@energy_contributions,
    partition = x@partition
  )
  if (is.null(result)) {
    cli::cli_warn("No {what} data available in results")
    return(NULL)
  }
  tibble::as_tibble(result)
}

#' @export
method(get_data, ValidationResult) <- function(x, what = c("issues", "summary", "recommendations"), ...) {
  what <- match.arg(what)
  result <- switch(what,
    issues = x@issues,
    summary = x@data_summary,
    recommendations = tibble::tibble(recommendation = x@recommendations)
  )
  if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
    cli::cli_warn("No {what} data available in validation results")
    return(NULL)
  }
  tibble::as_tibble(result)
}

# -----------------------------------------------------------------------------
# Conversion to tibble
# -----------------------------------------------------------------------------

#' @export
method(as.data.frame, CalorimetryData) <- function(x, ...) {
  x@data
}

#' @export
method(as.data.frame, OxidationResults) <- function(x, ...) {
  x@oxidation_rates
}
