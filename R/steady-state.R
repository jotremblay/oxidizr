#' Steady-State Detection Functions
#'
#' @name steady-state
#' @description Functions for detecting steady-state periods in metabolic data
#'   using coefficient of variation (CV) and rolling variance methods.
#'
#' @references
#' Robergs, R. A., Dwyer, D., & Astorino, T. (2010). Recommendations for
#' improved data processing from expired gas analysis indirect calorimetry.
#' Sports Medicine, 40(2), 95-111.
NULL

# -----------------------------------------------------------------------------
# CV Threshold Method
# -----------------------------------------------------------------------------

#' Detect Steady-State Using Coefficient of Variation
#'
#' Identifies steady-state periods where the coefficient of variation (CV)
#' of a variable falls below a specified threshold within a rolling window.
#'
#' @param data A data frame or CalorimetryData object containing the measurements.
#' @param variable Character string specifying the variable to analyze (default: "vo2").
#' @param cv_threshold Numeric CV threshold (default: 0.10 = 10%). Values at or below
#'   this threshold are considered steady-state.
#' @param window_size Integer specifying the number of consecutive observations
#'   in the rolling window (default: 3).
#' @param min_duration Minimum number of consecutive steady-state windows required
#'   to qualify as a steady-state period (default: 2).
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#'
#' @return A tibble with columns:
#'   - Subject ID column
#'   - Time column
#'   - `cv`: Rolling CV for each time point

#'   - `is_steady`: Logical indicating if CV is below threshold
#'   - `steady_period`: Integer identifying consecutive steady-state periods
#'   - `meets_duration`: Logical indicating if period meets minimum duration
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Detect steady-state in VO2 data
#' ss_result <- detect_steady_state_cv(calo_data, variable = "vo2", cv_threshold = 0.10)
#'
#' # Use stricter threshold
#' ss_strict <- detect_steady_state_cv(calo_data, variable = "vo2", cv_threshold = 0.05)
#' }
detect_steady_state_cv <- function(data,
                                   variable = "vo2",
                                   cv_threshold = 0.10,
                                   window_size = 3,
                                   min_duration = 2,
                                   id_col = "id",
                                   time_col = "time") {
  # Extract data frame if S7 object

  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }


  # Validate inputs

  if (!variable %in% names(df)) {
    cli::cli_abort("Variable '{variable}' not found in data")
  }
  if (!id_col %in% names(df)) {
    cli::cli_abort("ID column '{id_col}' not found in data")
  }
  if (!time_col %in% names(df)) {
    cli::cli_abort("Time column '{time_col}' not found in data")
  }
  if (cv_threshold <= 0 || cv_threshold > 1) {
    cli::cli_abort("cv_threshold must be between 0 and 1 (e.g., 0.10 for 10%)")
  }
  if (window_size < 2) {
    cli::cli_abort("window_size must be at least 2")
  }

  # Calculate rolling CV for each subject

  result <- df |>
    dplyr::arrange(.data[[id_col]], .data[[time_col]]) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      # Rolling mean
      roll_mean = slider::slide_dbl(
        .data[[variable]],
        mean,
        na.rm = TRUE,
        .before = window_size - 1,
        .complete = TRUE
      ),
      # Rolling SD
      roll_sd = slider::slide_dbl(
        .data[[variable]],
        sd,
        na.rm = TRUE,
        .before = window_size - 1,
        .complete = TRUE
      ),
      # CV = SD / mean
      cv = dplyr::if_else(
        .data$roll_mean > 0,
        .data$roll_sd / .data$roll_mean,
        NA_real_
      ),
      # Is this point in steady-state?
      is_steady = !is.na(.data$cv) & .data$cv <= cv_threshold
    ) |>
    dplyr::ungroup()

  # Identify consecutive steady-state periods
  result <- result |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      # Create run-length encoding for steady periods
      steady_change = .data$is_steady != dplyr::lag(.data$is_steady, default = FALSE),
      steady_period = cumsum(.data$steady_change) * .data$is_steady
    ) |>
    dplyr::ungroup()

  # Calculate period durations and flag those meeting minimum

  period_durations <- result |>
    dplyr::filter(.data$steady_period > 0) |>
    dplyr::group_by(.data[[id_col]], .data$steady_period) |>
    dplyr::summarise(
      duration = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      meets_duration = .data$duration >= min_duration
    )

  # Join back to main result
  result <- result |>
    dplyr::left_join(
      period_durations |> dplyr::select(dplyr::all_of(id_col), "steady_period", "meets_duration"),
      by = c(id_col, "steady_period")
    ) |>
    dplyr::mutate(
      meets_duration = dplyr::coalesce(.data$meets_duration, FALSE)
    ) |>
    dplyr::select(
      dplyr::all_of(c(id_col, time_col)),
      dplyr::all_of(variable),
      "cv",
      "is_steady",
      "steady_period",
      "meets_duration"
    )

  # Add attributes for later reference
  attr(result, "method") <- "cv"
  attr(result, "variable") <- variable
  attr(result, "cv_threshold") <- cv_threshold
  attr(result, "window_size") <- window_size
  attr(result, "min_duration") <- min_duration

  result
}

# -----------------------------------------------------------------------------
# Rolling Variance Method
# -----------------------------------------------------------------------------

#' Detect Steady-State Using Rolling Variance
#'
#' Identifies steady-state periods where the rolling variance of a variable
#' falls below a specified threshold or automatically determined threshold.
#'
#' @param data A data frame or CalorimetryData object containing the measurements.
#' @param variable Character string specifying the variable to analyze (default: "vo2").
#' @param variance_threshold Numeric variance threshold. If NULL (default),
#'   automatically determined as the 25th percentile of observed variances.
#' @param window_size Integer specifying the number of consecutive observations
#'   in the rolling window (default: 5).
#' @param min_duration Minimum number of consecutive low-variance windows required
#'   to qualify as a steady-state period (default: 2).
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#'
#' @return A tibble with columns:
#'   - Subject ID column
#'   - Time column
#'   - `variance`: Rolling variance for each time point
#'   - `variance_threshold`: The threshold used
#'   - `is_steady`: Logical indicating if variance is below threshold
#'   - `steady_period`: Integer identifying consecutive steady-state periods
#'   - `meets_duration`: Logical indicating if period meets minimum duration
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Detect steady-state with auto threshold
#' ss_result <- detect_steady_state_variance(calo_data, variable = "vo2")
#'
#' # Use specific variance threshold
#' ss_specific <- detect_steady_state_variance(calo_data, variable = "vo2",
#'                                              variance_threshold = 0.01)
#' }
detect_steady_state_variance <- function(data,
                                         variable = "vo2",
                                         variance_threshold = NULL,
                                         window_size = 5,
                                         min_duration = 2,
                                         id_col = "id",
                                         time_col = "time") {
  # Extract data frame if S7 object
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }

  # Validate inputs
  if (!variable %in% names(df)) {
    cli::cli_abort("Variable '{variable}' not found in data")
  }
  if (!id_col %in% names(df)) {
    cli::cli_abort("ID column '{id_col}' not found in data")
  }
  if (!time_col %in% names(df)) {
    cli::cli_abort("Time column '{time_col}' not found in data")
  }
  if (window_size < 2) {
    cli::cli_abort("window_size must be at least 2")
  }

  # Calculate rolling variance for each subject
  result <- df |>
    dplyr::arrange(.data[[id_col]], .data[[time_col]]) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      variance = slider::slide_dbl(
        .data[[variable]],
        var,
        na.rm = TRUE,
        .before = window_size - 1,
        .complete = TRUE
      )
    ) |>
    dplyr::ungroup()

  # Auto-determine threshold if not provided
  if (is.null(variance_threshold)) {
    variance_threshold <- stats::quantile(
      result$variance,
      probs = 0.25,
      na.rm = TRUE
    )
    cli::cli_inform(
      "Auto-determined variance threshold: {round(variance_threshold, 6)}"
    )
  } else if (variance_threshold <= 0) {
    cli::cli_abort("variance_threshold must be positive")
  }

  # Identify steady-state points
  result <- result |>
    dplyr::mutate(
      variance_threshold = variance_threshold,
      is_steady = !is.na(.data$variance) & .data$variance <= variance_threshold
    )

  # Identify consecutive steady-state periods
  result <- result |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      steady_change = .data$is_steady != dplyr::lag(.data$is_steady, default = FALSE),
      steady_period = cumsum(.data$steady_change) * .data$is_steady
    ) |>
    dplyr::ungroup()

  # Calculate period durations
  period_durations <- result |>
    dplyr::filter(.data$steady_period > 0) |>
    dplyr::group_by(.data[[id_col]], .data$steady_period) |>
    dplyr::summarise(
      duration = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      meets_duration = .data$duration >= min_duration
    )

  # Join back
  result <- result |>
    dplyr::left_join(
      period_durations |> dplyr::select(dplyr::all_of(id_col), "steady_period", "meets_duration"),
      by = c(id_col, "steady_period")
    ) |>
    dplyr::mutate(
      meets_duration = dplyr::coalesce(.data$meets_duration, FALSE)
    ) |>
    dplyr::select(
      dplyr::all_of(c(id_col, time_col)),
      dplyr::all_of(variable),
      "variance",
      "variance_threshold",
      "is_steady",
      "steady_period",
      "meets_duration"
    )

  # Add attributes
  attr(result, "method") <- "variance"
  attr(result, "variable") <- variable
  attr(result, "variance_threshold") <- variance_threshold
  attr(result, "window_size") <- window_size
  attr(result, "min_duration") <- min_duration

  result
}

# -----------------------------------------------------------------------------
# Combined Method
# -----------------------------------------------------------------------------

#' Detect Steady-State Using Combined Methods
#'
#' Combines CV threshold and rolling variance methods for robust steady-state
#' detection. Can use either method alone or both with configurable combination logic.
#'
#' @param data A data frame or CalorimetryData object containing the measurements.
#' @param variables Character vector of variables to analyze (default: c("vo2", "vco2")).
#'   When multiple variables are specified, steady-state is determined based on
#'   all variables meeting the criteria.
#' @param cv_threshold Numeric CV threshold (default: 0.10 = 10%).
#' @param variance_threshold Numeric variance threshold. If NULL (default),
#'   automatically determined per variable.
#' @param method Character specifying which method(s) to use: "both" (default),
#'   "cv", or "variance".
#' @param combine_method How to combine results when method = "both":
#'   "and" (default) requires both methods agree, "or" accepts either.
#' @param window_size_cv Window size for CV method (default: 3).
#' @param window_size_var Window size for variance method (default: 5).
#' @param min_duration Minimum consecutive steady-state points (default: 2).
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#' @param verbose Logical, whether to print progress messages (default: TRUE).
#'
#' @return A tibble with columns:
#'   - Subject ID column
#'   - Time column
#'   - Variable columns
#'   - For each variable: `{var}_cv`, `{var}_variance`, `{var}_steady`
#'   - `is_steady_all`: Combined steady-state indicator across all variables
#'   - `steady_period`: Integer identifying consecutive steady-state periods
#'   - `meets_duration`: Logical indicating if period meets minimum duration
#'   - `method`: The detection method used
#'
#' @section Small Sample Considerations:
#' CV and variance-based methods require sufficient data points within each
#' window. With fewer than 3 subjects or limited time points:
#' \itemize{
#'   \item Consider reducing `window_size_cv` or `window_size_var` if data
#'     is sparse
#'   \item CV calculations may be unstable with small samples
#'   \item Visual inspection of individual subject traces is recommended
#'   \item Default thresholds were validated on samples with n >= 5 subjects
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Combined detection on VO2 and VCO2
#' ss <- detect_steady_state(calo_data, variables = c("vo2", "vco2"))
#'
#' # Use only CV method
#' ss_cv <- detect_steady_state(calo_data, method = "cv", cv_threshold = 0.08)
#'
#' # Accept steady-state if EITHER method indicates it
#' ss_or <- detect_steady_state(calo_data, method = "both", combine_method = "or")
#' }
detect_steady_state <- function(data,
                                variables = c("vo2", "vco2"),
                                cv_threshold = 0.10,
                                variance_threshold = NULL,
                                method = c("both", "cv", "variance"),
                                combine_method = c("and", "or"),
                                window_size_cv = 3,
                                window_size_var = 5,
                                min_duration = 2,
                                id_col = "id",
                                time_col = "time",
                                verbose = TRUE) {
  method <- match.arg(method)
  combine_method <- match.arg(combine_method)

  # Extract data frame if S7 object
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }

  # Validate variables exist
  missing_vars <- setdiff(variables, names(df))
  if (length(missing_vars) > 0) {
    cli::cli_abort("Variable(s) not found in data: {paste(missing_vars, collapse = ', ')}")
  }

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Detecting steady-state using {method} method",
      "*" = "Variables: {paste(variables, collapse = ', ')}",
      "*" = "CV threshold: {cv_threshold * 100}%",
      "*" = "Combine method: {combine_method}"
    ))
  }

  # Start with base data
  result <- df |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, variables))) |>
    dplyr::arrange(.data[[id_col]], .data[[time_col]])

  # Process each variable
  for (var in variables) {
    if (method %in% c("cv", "both")) {
      cv_result <- detect_steady_state_cv(
        data = df,
        variable = var,
        cv_threshold = cv_threshold,
        window_size = window_size_cv,
        min_duration = 1,  # We'll handle duration at the combined level
        id_col = id_col,
        time_col = time_col
      )

      result <- result |>
        dplyr::left_join(
          cv_result |>
            dplyr::select(
              dplyr::all_of(c(id_col, time_col)),
              "{var}_cv" := "cv",
              "{var}_steady_cv" := "is_steady"
            ),
          by = c(id_col, time_col)
        )
    }

    if (method %in% c("variance", "both")) {
      # Handle per-variable variance threshold
      var_threshold <- if (!is.null(variance_threshold) && is.list(variance_threshold)) {
        variance_threshold[[var]]
      } else if (!is.null(variance_threshold) && is.numeric(variance_threshold)) {
        variance_threshold
      } else {
        NULL
      }

      var_result <- detect_steady_state_variance(
        data = df,
        variable = var,
        variance_threshold = var_threshold,
        window_size = window_size_var,
        min_duration = 1,
        id_col = id_col,
        time_col = time_col
      )

      result <- result |>
        dplyr::left_join(
          var_result |>
            dplyr::select(
              dplyr::all_of(c(id_col, time_col)),
              "{var}_variance" := "variance",
              "{var}_steady_var" := "is_steady"
            ),
          by = c(id_col, time_col)
        )
    }

    # Combine methods for this variable
    steady_col <- paste0(var, "_steady")
    if (method == "cv") {
      result <- result |>
        dplyr::mutate(!!steady_col := .data[[paste0(var, "_steady_cv")]])
    } else if (method == "variance") {
      result <- result |>
        dplyr::mutate(!!steady_col := .data[[paste0(var, "_steady_var")]])
    } else {
      # method == "both"
      cv_col <- paste0(var, "_steady_cv")
      var_col <- paste0(var, "_steady_var")
      if (combine_method == "and") {
        result <- result |>
          dplyr::mutate(
            !!steady_col := .data[[cv_col]] & .data[[var_col]]
          )
      } else {
        result <- result |>
          dplyr::mutate(
            !!steady_col := .data[[cv_col]] | .data[[var_col]]
          )
      }
    }
  }

  # Combine across all variables (all must be steady)
  steady_cols <- paste0(variables, "_steady")
  result <- result |>
    dplyr::rowwise() |>
    dplyr::mutate(
      is_steady_all = all(dplyr::c_across(dplyr::all_of(steady_cols)))
    ) |>
    dplyr::ungroup()

  # Identify consecutive steady-state periods across all variables
  result <- result |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      steady_change = .data$is_steady_all != dplyr::lag(.data$is_steady_all, default = FALSE),
      steady_period = cumsum(.data$steady_change) * .data$is_steady_all
    ) |>
    dplyr::ungroup()

  # Calculate period durations
  period_durations <- result |>
    dplyr::filter(.data$steady_period > 0) |>
    dplyr::group_by(.data[[id_col]], .data$steady_period) |>
    dplyr::summarise(
      duration = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      meets_duration = .data$duration >= min_duration
    )

  # Join back and finalize

  result <- result |>
    dplyr::left_join(
      period_durations |> dplyr::select(dplyr::all_of(id_col), "steady_period", "meets_duration"),
      by = c(id_col, "steady_period")
    ) |>
    dplyr::mutate(
      meets_duration = dplyr::coalesce(.data$meets_duration, FALSE),
      method = method
    ) |>
    dplyr::select(
      dplyr::all_of(c(id_col, time_col, variables)),
      dplyr::starts_with(paste0(variables, "_")),
      "is_steady_all",
      "steady_period",
      "meets_duration",
      "method"
    )

  # Add attributes
  attr(result, "variables") <- variables
  attr(result, "method") <- method
  attr(result, "combine_method") <- combine_method
  attr(result, "cv_threshold") <- cv_threshold
  attr(result, "variance_threshold") <- variance_threshold
  attr(result, "min_duration") <- min_duration

  if (verbose) {
    n_steady <- sum(result$is_steady_all & result$meets_duration, na.rm = TRUE)
    n_total <- nrow(result)
    pct_steady <- round(n_steady / n_total * 100, 1)
    cli::cli_inform(c(
      "v" = "Steady-state detection complete",
      "*" = "{n_steady}/{n_total} observations ({pct_steady}%) in qualifying steady-state periods"
    ))
  }

  result
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Summarize Steady-State Periods
#'
#' Generates a summary of steady-state periods identified in the data.
#'
#' @param steady_state_result Result from any of the detect_steady_state functions.
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#'
#' @return A tibble with columns:
#'   - Subject ID column
#'   - `period`: Period number
#'   - `start_time`: Start time of period
#'   - `end_time`: End time of period
#'   - `duration_points`: Number of data points in period
#'   - `meets_duration`: Whether period meets minimum duration
#'   - `mean_cv`: Mean CV during period (if available)
#'   - `mean_variance`: Mean variance during period (if available)
#'
#' @export
summarize_steady_state <- function(steady_state_result,
                                   id_col = "id",
                                   time_col = "time") {
  # Filter to steady-state periods only
  periods_df <- steady_state_result |>
    dplyr::filter(.data$steady_period > 0)

  if (nrow(periods_df) == 0) {
    cli::cli_warn("No steady-state periods found")
    return(tibble::tibble())
  }

  # Summarize each period
  summary_df <- periods_df |>
    dplyr::group_by(.data[[id_col]], .data$steady_period) |>
    dplyr::summarise(
      start_time = min(.data[[time_col]], na.rm = TRUE),
      end_time = max(.data[[time_col]], na.rm = TRUE),
      duration_points = dplyr::n(),
      meets_duration = dplyr::first(.data$meets_duration),
      .groups = "drop"
    ) |>
    dplyr::rename(period = "steady_period")

  # Add CV/variance summaries if available
  cv_cols <- names(steady_state_result)[grepl("_cv$", names(steady_state_result))]
  var_cols <- names(steady_state_result)[grepl("_variance$", names(steady_state_result))]

  if (length(cv_cols) > 0) {
    cv_summary <- periods_df |>
      dplyr::group_by(.data[[id_col]], .data$steady_period) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(cv_cols),
          ~ mean(.x, na.rm = TRUE),
          .names = "mean_{.col}"
        ),
        .groups = "drop"
      ) |>
      dplyr::rename(period = "steady_period")

    summary_df <- summary_df |>
      dplyr::left_join(cv_summary, by = c(id_col, "period"))
  }

  if (length(var_cols) > 0) {
    var_summary <- periods_df |>
      dplyr::group_by(.data[[id_col]], .data$steady_period) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(var_cols),
          ~ mean(.x, na.rm = TRUE),
          .names = "mean_{.col}"
        ),
        .groups = "drop"
      ) |>
      dplyr::rename(period = "steady_period")

    summary_df <- summary_df |>
      dplyr::left_join(var_summary, by = c(id_col, "period"))
  }

  summary_df
}

#' Filter Data to Steady-State Periods
#'
#' Filters a data frame to include only observations within qualifying
#' steady-state periods.
#'
#' @param data Original data frame to filter.
#' @param steady_state_result Result from any of the detect_steady_state functions.
#' @param require_meets_duration Logical, whether to require periods meet minimum
#'   duration (default: TRUE).
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#'
#' @return Filtered data frame containing only steady-state observations.
#'
#' @export
filter_steady_state <- function(data,
                                steady_state_result,
                                require_meets_duration = TRUE,
                                id_col = "id",
                                time_col = "time") {
  # Extract data frame if S7 object
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }

  # Get steady-state indicators
  if (require_meets_duration) {
    steady_points <- steady_state_result |>
      dplyr::filter(.data$meets_duration) |>
      dplyr::select(dplyr::all_of(c(id_col, time_col)))
  } else {
    # Use is_steady_all if available (combined method), otherwise is_steady
    steady_col <- if ("is_steady_all" %in% names(steady_state_result)) {
      "is_steady_all"
    } else {
      "is_steady"
    }
    steady_points <- steady_state_result |>
      dplyr::filter(.data[[steady_col]]) |>
      dplyr::select(dplyr::all_of(c(id_col, time_col)))
  }

  # Filter original data
  df |>
    dplyr::inner_join(steady_points, by = c(id_col, time_col))
}

#' Calculate Steady-State Statistics
#'
#' Calculates summary statistics for variables during steady-state periods.
#'
#' @param data Original data frame.
#' @param steady_state_result Result from any of the detect_steady_state functions.
#' @param variables Character vector of variables to summarize.
#' @param id_col Character string specifying the subject ID column (default: "id").
#' @param time_col Character string specifying the time column (default: "time").
#'
#' @return A tibble with summary statistics (mean, sd, cv, min, max, n) for each
#'   variable during steady-state periods, grouped by subject.
#'
#' @export
calc_steady_state_stats <- function(data,
                                    steady_state_result,
                                    variables = c("vo2", "vco2"),
                                    id_col = "id",
                                    time_col = "time") {
  # Filter to steady-state
  ss_data <- filter_steady_state(
    data = data,
    steady_state_result = steady_state_result,
    require_meets_duration = TRUE,
    id_col = id_col,
    time_col = time_col
  )

  if (nrow(ss_data) == 0) {
    cli::cli_warn("No steady-state data to summarize")
    return(tibble::tibble())
  }

  # Calculate statistics per subject
  ss_data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(variables),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          cv = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          n = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),
      n_total = dplyr::n(),
      .groups = "drop"
    )
}
