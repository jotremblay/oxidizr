#' Table Generation Functions
#'
#' @name tables
#' @description Functions for creating publication-ready gt tables
NULL

#' Subject Characteristics Table
#'
#' Create a formatted table of subject characteristics.
#'
#' @param study An OxidationStudy object
#' @param by Grouping variable (e.g., "protocol")
#' @param vars Variables to include (NULL = auto-detect)
#' @param decimals Number of decimal places
#'
#' @return A gt table object
#' @export
tbl_subjects <- function(study,
                          by = NULL,
                          vars = NULL,
                          decimals = 1) {

  if (S7_inherits(study, OxidationStudy)) {
    if (is.null(study@subjects)) {
      cli::cli_abort("No subject data in study")
    }
    df <- study@subjects@data
  } else if (is.data.frame(study)) {
    df <- study
  } else {
    cli::cli_abort("study must be OxidationStudy or data frame")
  }

  # Auto-detect numeric variables if not specified
  if (is.null(vars)) {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    # Common subject characteristics
    common_vars <- c("age", "mass", "height", "body_mass", "body_fat",
                     "vo2max", "VO2max", "pam", "PAM", "initial_body_mass")
    vars <- intersect(numeric_cols, common_vars)
    if (length(vars) == 0) vars <- numeric_cols[1:min(5, length(numeric_cols))]
  }

  available_vars <- intersect(vars, names(df))
  if (length(available_vars) == 0) {
    cli::cli_abort("No matching variables found")
  }

  # Calculate summary statistics
  if (!is.null(by) && by %in% names(df)) {
    summary_df <- df |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  } else {
    summary_df <- df |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        )
      )
  }

  # Create gt table
  tbl <- gt::gt(summary_df)

  # Merge mean +/- sd columns
  for (var in available_vars) {
    mean_col <- paste0(var, "_mean")
    sd_col <- paste0(var, "_sd")

    if (mean_col %in% names(summary_df) && sd_col %in% names(summary_df)) {
      tbl <- tbl |>
        gt::fmt_number(columns = dplyr::all_of(c(mean_col, sd_col)),
                       decimals = decimals) |>
        gt::cols_merge_uncert(
          col_val = dplyr::all_of(mean_col),
          col_uncert = dplyr::all_of(sd_col)
        ) |>
        gt::cols_label(!!mean_col := var)
    }
  }

  # Format n column
  tbl <- tbl |>
    gt::cols_label(n = gt::md("**n**"))

  # Add title if grouped
  if (!is.null(by)) {
    tbl <- tbl |>
      gt::cols_label(!!by := gt::md(paste0("**", by, "**")))
  }

  tbl
}

#' Oxidation Summary Table
#'
#' Create a summary table of oxidation rates.
#'
#' @param results An OxidationResults object
#' @param by Grouping variable (e.g., "protocol")
#' @param vars Variables to include (NULL = auto)
#' @param decimals Number of decimal places
#'
#' @return A gt table object
#' @export
tbl_oxidation_summary <- function(results,
                                   by = NULL,
                                   vars = NULL,
                                   decimals = 2) {

  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
  } else {
    df <- results
  }

  # Default oxidation variables
  if (is.null(vars)) {
    vars <- c("cho_total", "fat_total", "protein_ox",
              "cho_exo", "cho_endo", "cho_mus", "cho_liv")
  }

  available_vars <- intersect(vars, names(df))
  if (length(available_vars) == 0) {
    cli::cli_abort("No matching oxidation columns found")
  }

  # Calculate summary
  if (!is.null(by) && by %in% names(df)) {
    summary_df <- df |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  } else {
    summary_df <- df |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        )
      )
  }

  # Create gt table
  tbl <- gt::gt(summary_df)

  # Labels for oxidation variables
  var_labels <- c(
    cho_total = "Total CHO (g/min)",
    fat_total = "Fat (g/min)",
    protein_ox = "Protein (g/min)",
    cho_exo = "Exogenous CHO (g/min)",
    cho_endo = "Endogenous CHO (g/min)",
    cho_mus = "Muscle CHO (g/min)",
    cho_liv = "Liver CHO (g/min)"
  )

  # Format and merge columns
  for (var in available_vars) {
    mean_col <- paste0(var, "_mean")
    sd_col <- paste0(var, "_sd")

    if (mean_col %in% names(summary_df)) {
      label <- if (var %in% names(var_labels)) var_labels[var] else var

      tbl <- tbl |>
        gt::fmt_number(columns = dplyr::all_of(c(mean_col, sd_col)),
                       decimals = decimals) |>
        gt::cols_merge_uncert(
          col_val = dplyr::all_of(mean_col),
          col_uncert = dplyr::all_of(sd_col)
        ) |>
        gt::cols_label(!!mean_col := label)
    }
  }

  tbl <- tbl |>
    gt::cols_label(n = gt::md("**n**"))

  if (!is.null(by)) {
    tbl <- tbl |>
      gt::cols_label(!!by := gt::md(paste0("**", by, "**")))
  }

  tbl
}

#' Energy Contribution Table
#'
#' Create a table of energy contributions by substrate.
#'
#' @param results An OxidationResults object
#' @param by Grouping variable
#' @param show_kcal Show absolute energy (kcal/min)
#' @param show_percent Show percentage contributions
#' @param decimals Number of decimal places
#'
#' @return A gt table object
#' @export
tbl_energy_contribution <- function(results,
                                     by = NULL,
                                     show_kcal = TRUE,
                                     show_percent = TRUE,
                                     decimals = 1) {

  if (S7_inherits(results, OxidationResults)) {
    df <- results@energy_contributions
    if (is.null(df)) {
      cli::cli_abort("Energy contributions not available - run analyze_oxidation with calc_energy = TRUE")
    }
  } else {
    df <- results
  }

  # Select columns
  vars <- c()
  if (show_kcal) {
    vars <- c(vars, "e_cho_total", "e_fat", "e_protein", "e_total")
  }
  if (show_percent) {
    vars <- c(vars, "pct_cho_total", "pct_fat", "pct_protein")
  }

  available_vars <- intersect(vars, names(df))

  # Calculate summary
  if (!is.null(by) && by %in% names(df)) {
    summary_df <- df |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  } else {
    summary_df <- df |>
      dplyr::summarise(
        n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(available_vars),
          list(mean = ~ mean(.x, na.rm = TRUE),
               sd = ~ sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        )
      )
  }

  tbl <- gt::gt(summary_df)

  # Labels
  var_labels <- c(
    e_cho_total = "CHO (kcal/min)",
    e_fat = "Fat (kcal/min)",
    e_protein = "Protein (kcal/min)",
    e_total = "Total (kcal/min)",
    pct_cho_total = "CHO (%)",
    pct_fat = "Fat (%)",
    pct_protein = "Protein (%)"
  )

  for (var in available_vars) {
    mean_col <- paste0(var, "_mean")
    sd_col <- paste0(var, "_sd")

    if (mean_col %in% names(summary_df)) {
      label <- if (var %in% names(var_labels)) var_labels[var] else var

      tbl <- tbl |>
        gt::fmt_number(columns = dplyr::all_of(c(mean_col, sd_col)),
                       decimals = decimals) |>
        gt::cols_merge_uncert(
          col_val = dplyr::all_of(mean_col),
          col_uncert = dplyr::all_of(sd_col)
        ) |>
        gt::cols_label(!!mean_col := label)
    }
  }

  tbl <- tbl |>
    gt::cols_label(n = gt::md("**n**"))

  if (!is.null(by)) {
    tbl <- tbl |>
      gt::cols_label(!!by := gt::md(paste0("**", by, "**")))
  }

  tbl
}

#' Statistical Results Table
#'
#' Create a table of statistical test results.
#'
#' @param model A fitted model object (aov, lmer, etc.)
#' @param type Type of table: "anova" or "posthoc"
#' @param decimals Number of decimal places
#'
#' @return A gt table object
#' @export
tbl_stats <- function(model,
                       type = c("anova", "posthoc"),
                       decimals = 3) {

  type <- match.arg(type)

  # This function would integrate with afex/emmeans

  # Placeholder for now
  cli::cli_warn("tbl_stats requires afex/emmeans - implement based on your statistical workflow")

  NULL
}

#' Export Table to Excel
#'
#' Export a gt table or data frame to Excel format.
#'
#' @param tbl A gt table or data frame
#' @param file Output file path (.xlsx)
#' @param sheet Sheet name (default: "Sheet1")
#'
#' @return Invisible NULL
#' @export
export_table_xlsx <- function(tbl, file, sheet = "Sheet1") {

  # Extract data from gt if needed
  if (inherits(tbl, "gt_tbl")) {
    # gt tables don't have a direct xlsx export, use the underlying data
    cli::cli_alert_info("Exporting underlying data (formatting not preserved)")
    df <- tbl$`_data`
  } else {
    df <- tbl
  }

  # Check for writexl or openxlsx
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(list(!!sheet := df), file)
  } else if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, file, sheetName = sheet)
  } else {
    cli::cli_abort("Install 'writexl' or 'openxlsx' package to export to Excel")
  }

  cli::cli_alert_success("Table exported to {file}")
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Data Quality Tables
# -----------------------------------------------------------------------------

#' Validation Summary Table
#'
#' Create a formatted summary table of validation results.
#'
#' @param validation A ValidationResult object from validate_study() or component validators.
#' @param group_by How to group issues: "category" (default), "severity", or "variable".
#' @param show_details Logical, show detailed message column (default: FALSE).
#' @param max_rows Maximum rows to display (default: 20).
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- validate_study(study)
#' tbl_validation_summary(validation)
#' tbl_validation_summary(validation, group_by = "severity")
#' }
tbl_validation_summary <- function(validation,
                                    group_by = c("category", "severity", "variable"),
                                    show_details = FALSE,
                                    max_rows = 20) {
  group_by <- match.arg(group_by)

  if (!S7_inherits(validation, ValidationResult)) {
    cli::cli_abort("validation must be a ValidationResult object")
  }

  issues <- validation@issues

  if (nrow(issues) == 0) {
    cli::cli_inform("No validation issues found")
    return(gt::gt(tibble::tibble(Status = "All checks passed")))
  }

  # Summarize by grouping variable
  summary_df <- issues |>
    dplyr::group_by(.data[[group_by]]) |>
    dplyr::summarise(
      n_issues = dplyr::n(),
      errors = sum(.data$severity == "error"),
      warnings = sum(.data$severity == "warning"),
      info = sum(.data$severity == "info"),
      total_affected = sum(.data$n_affected, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$errors), dplyr::desc(.data$warnings))

  if (nrow(summary_df) > max_rows) {
    summary_df <- summary_df |> dplyr::slice_head(n = max_rows)
    cli::cli_inform("Showing first {max_rows} groups")
  }

  # Create gt table
  tbl <- gt::gt(summary_df) |>
    gt::cols_label(
      !!group_by := stringr::str_to_title(group_by),
      n_issues = "Issues",
      errors = "Errors",
      warnings = "Warnings",
      info = "Info",
      total_affected = "Affected Values"
    ) |>
    gt::fmt_number(columns = c("n_issues", "errors", "warnings", "info", "total_affected"),
                   decimals = 0, use_seps = TRUE)

  # Color-code severity columns
  tbl <- tbl |>
    gt::data_color(
      columns = "errors",
      fn = scales::col_numeric(
        palette = c("white", "#E41A1C"),
        domain = c(0, max(summary_df$errors, 1))
      )
    ) |>
    gt::data_color(
      columns = "warnings",
      fn = scales::col_numeric(
        palette = c("white", "#FF7F00"),
        domain = c(0, max(summary_df$warnings, 1))
      )
    )

  # Add title and subtitle
  n_errors <- validation@severity_summary$error %||% 0
  n_warnings <- validation@severity_summary$warning %||% 0

  status_text <- if (validation@passed) {
    "Validation Passed (no errors)"
  } else {
    paste0("Validation Failed (", n_errors, " error(s), ", n_warnings, " warning(s))")
  }

  tbl <- tbl |>
    gt::tab_header(
      title = "Validation Summary",
      subtitle = status_text
    )

  # Add footnote if truncated
  if (nrow(issues) > max_rows) {
    tbl <- tbl |>
      gt::tab_footnote(
        footnote = paste0("Showing top ", max_rows, " of ", nrow(issues), " issues")
      )
  }

  tbl
}

#' Data Completeness Table
#'
#' Create a table showing data completeness by subject or variable.
#'
#' @param data A data frame, CalorimetryData, or OxidationStudy object.
#' @param by How to organize the table: "variable" (default) or "subject".
#' @param variables Character vector of variables to include (default: auto-detect).
#' @param id_col Subject ID column name (default: "id").
#' @param threshold Threshold percentage for highlighting incomplete data (default: 95).
#' @param decimals Number of decimal places (default: 1).
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' tbl_data_completeness(calo_data)
#' tbl_data_completeness(calo_data, by = "subject")
#' }
tbl_data_completeness <- function(data,
                                   by = c("variable", "subject"),
                                   variables = NULL,
                                   id_col = "id",
                                   threshold = 95,
                                   decimals = 1) {
  by <- match.arg(by)

  # Extract data
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    df <- data@data
  } else if (S7_inherits(data, OxidationStudy)) {
    id_col <- data@calorimetry@id_col
    df <- data@calorimetry@data
  } else {
    df <- data
  }

  # Auto-detect numeric variables if not specified
  if (is.null(variables)) {
    variables <- names(df)[sapply(df, is.numeric)]
    variables <- setdiff(variables, c(id_col, "time"))
  }

  variables <- intersect(variables, names(df))
  if (length(variables) == 0) {
    cli::cli_abort("No numeric variables found to check for completeness")
  }

  if (by == "variable") {
    # Completeness by variable across all subjects
    completeness_df <- tibble::tibble(
      variable = variables,
      n_total = nrow(df),
      n_complete = purrr::map_int(variables, ~ sum(!is.na(df[[.x]]))),
      n_missing = purrr::map_int(variables, ~ sum(is.na(df[[.x]])))
    ) |>
      dplyr::mutate(
        pct_complete = .data$n_complete / .data$n_total * 100,
        pct_missing = .data$n_missing / .data$n_total * 100
      ) |>
      dplyr::arrange(dplyr::desc(.data$pct_missing))

    tbl <- gt::gt(completeness_df) |>
      gt::cols_label(
        variable = "Variable",
        n_total = "Total",
        n_complete = "Complete",
        n_missing = "Missing",
        pct_complete = "Complete (%)",
        pct_missing = "Missing (%)"
      ) |>
      gt::fmt_number(columns = c("n_total", "n_complete", "n_missing"), decimals = 0) |>
      gt::fmt_number(columns = c("pct_complete", "pct_missing"), decimals = decimals)

    # Highlight incomplete variables
    tbl <- tbl |>
      gt::data_color(
        columns = "pct_complete",
        fn = scales::col_numeric(
          palette = c("#E41A1C", "#FFFF33", "#4DAF4A"),
          domain = c(0, 100)
        )
      )

  } else {
    # Completeness by subject
    completeness_df <- df |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(
        n_observations = dplyr::n(),
        dplyr::across(
          dplyr::all_of(variables),
          ~ sum(!is.na(.x)) / dplyr::n() * 100,
          .names = "{.col}_complete"
        ),
        .groups = "drop"
      )

    # Calculate overall completeness per subject
    complete_cols <- paste0(variables, "_complete")
    completeness_df <- completeness_df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        overall_complete = mean(dplyr::c_across(dplyr::all_of(complete_cols)), na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data$overall_complete)

    tbl <- gt::gt(completeness_df) |>
      gt::fmt_number(columns = dplyr::all_of(c(complete_cols, "overall_complete")),
                     decimals = decimals)

    # Rename columns
    for (var in variables) {
      col_name <- paste0(var, "_complete")
      tbl <- tbl |>
        gt::cols_label(!!col_name := paste0(var, " (%)"))
    }

    tbl <- tbl |>
      gt::cols_label(
        !!id_col := "Subject",
        n_observations = "Obs",
        overall_complete = "Overall (%)"
      )

    # Color code overall completeness
    tbl <- tbl |>
      gt::data_color(
        columns = "overall_complete",
        fn = scales::col_numeric(
          palette = c("#E41A1C", "#FFFF33", "#4DAF4A"),
          domain = c(0, 100)
        )
      )
  }

  # Add header
  tbl <- tbl |>
    gt::tab_header(
      title = "Data Completeness Report",
      subtitle = paste0("Threshold: ", threshold, "% complete")
    )

  tbl
}

#' Outlier Summary Table
#'
#' Create a detailed table of detected outliers from validation results.
#'
#' @param validation A ValidationResult object containing outlier information.
#' @param max_rows Maximum rows to display (default: 50).
#' @param show_values Logical, show actual outlier values (default: TRUE).
#' @param show_thresholds Logical, show threshold information (default: TRUE).
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- validate_calorimetry(calo_data)
#' tbl_outliers(validation)
#' }
tbl_outliers <- function(validation,
                          max_rows = 50,
                          show_values = TRUE,
                          show_thresholds = TRUE) {
  if (!S7_inherits(validation, ValidationResult)) {
    cli::cli_abort("validation must be a ValidationResult object")
  }

  issues <- validation@issues

  # Filter to outlier-related issues
  outlier_issues <- issues |>
    dplyr::filter(grepl("outlier|range|extreme", .data$check_id, ignore.case = TRUE))

  if (nrow(outlier_issues) == 0) {
    cli::cli_inform("No outliers detected")
    return(gt::gt(tibble::tibble(Status = "No outliers found")))
  }

  # Build summary table
  outlier_df <- outlier_issues |>
    dplyr::select(
      "variable",
      "severity",
      "message",
      "n_affected",
      "pct_affected",
      "threshold",
      "values"
    )

  # Handle values column (may be a list column)
  if ("values" %in% names(outlier_df) && is.list(outlier_df$values)) {
    outlier_df <- outlier_df |>
      dplyr::mutate(
        values_preview = purrr::map_chr(.data$values, function(v) {
          if (is.null(v) || length(v) == 0) return(NA_character_)
          if (length(v) > 5) {
            paste0(paste(round(v[1:5], 2), collapse = ", "), ", ...")
          } else {
            paste(round(v, 2), collapse = ", ")
          }
        })
      )
  }

  if (nrow(outlier_df) > max_rows) {
    outlier_df <- outlier_df |> dplyr::slice_head(n = max_rows)
    cli::cli_inform("Showing first {max_rows} outlier issues")
  }

  # Select columns based on options
  cols_to_show <- c("variable", "severity", "n_affected", "pct_affected")
  if (show_thresholds && "threshold" %in% names(outlier_df)) {
    cols_to_show <- c(cols_to_show, "threshold")
  }
  if (show_values && "values_preview" %in% names(outlier_df)) {
    cols_to_show <- c(cols_to_show, "values_preview")
  }
  cols_to_show <- c(cols_to_show, "message")

  outlier_df <- outlier_df |>
    dplyr::select(dplyr::any_of(cols_to_show))

  # Create gt table
  tbl <- gt::gt(outlier_df)

  # Column labels
  label_map <- list(
    variable = "Variable",
    severity = "Severity",
    n_affected = "Count",
    pct_affected = "% Affected",
    threshold = "Threshold",
    values_preview = "Sample Values",
    message = "Description"
  )

  for (col in names(outlier_df)) {
    if (col %in% names(label_map)) {
      tbl <- tbl |> gt::cols_label(!!col := label_map[[col]])
    }
  }

  # Format numeric columns
  if ("n_affected" %in% names(outlier_df)) {
    tbl <- tbl |> gt::fmt_number(columns = "n_affected", decimals = 0)
  }
  if ("pct_affected" %in% names(outlier_df)) {
    tbl <- tbl |> gt::fmt_number(columns = "pct_affected", decimals = 1, pattern = "{x}%")
  }

  # Color-code severity
  tbl <- tbl |>
    gt::data_color(
      columns = "severity",
      fn = function(x) {
        dplyr::case_when(
          x == "error" ~ "#E41A1C",
          x == "warning" ~ "#FF7F00",
          x == "info" ~ "#377EB8",
          TRUE ~ "grey"
        )
      }
    )

  # Header
  tbl <- tbl |>
    gt::tab_header(
      title = "Outlier Detection Summary",
      subtitle = paste0(nrow(outlier_issues), " outlier issue(s) found")
    )

  tbl
}

#' Steady-State Summary Table
#'
#' Create a summary table of steady-state detection results.
#'
#' @param steady_state_result Result from detect_steady_state() functions.
#' @param id_col Subject ID column name (default: "id").
#' @param decimals Number of decimal places (default: 2).
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' ss_result <- detect_steady_state(calo_data)
#' tbl_steady_state(ss_result)
#' }
tbl_steady_state <- function(steady_state_result,
                              id_col = "id",
                              decimals = 2) {
  df <- steady_state_result

  # Get method and variables from attributes

  method <- attr(df, "method") %||% "unknown"
  variables <- attr(df, "variables") %||% attr(df, "variable") %||% "vo2"
  cv_threshold <- attr(df, "cv_threshold")
  min_duration <- attr(df, "min_duration")

  # Summarize by subject
  summary_df <- df |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      n_steady = sum(.data$meets_duration, na.rm = TRUE),
      pct_steady = sum(.data$meets_duration, na.rm = TRUE) / dplyr::n() * 100,
      n_periods = length(unique(.data$steady_period[.data$steady_period > 0 & .data$meets_duration])),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$pct_steady))

  # Create gt table
  tbl <- gt::gt(summary_df) |>
    gt::cols_label(
      !!id_col := "Subject",
      n_observations = "Observations",
      n_steady = "Steady Points",
      pct_steady = "Steady (%)",
      n_periods = "Periods"
    ) |>
    gt::fmt_number(columns = c("n_observations", "n_steady", "n_periods"), decimals = 0) |>
    gt::fmt_number(columns = "pct_steady", decimals = decimals)

  # Color code steady percentage
  tbl <- tbl |>
    gt::data_color(
      columns = "pct_steady",
      fn = scales::col_numeric(
        palette = c("#E41A1C", "#FFFF33", "#4DAF4A"),
        domain = c(0, 100)
      )
    )

  # Build subtitle
  subtitle_parts <- c(paste0("Method: ", method))
  if (!is.null(cv_threshold)) {
    subtitle_parts <- c(subtitle_parts, paste0("CV threshold: ", cv_threshold * 100, "%"))
  }
  if (!is.null(min_duration)) {
    subtitle_parts <- c(subtitle_parts, paste0("Min duration: ", min_duration, " points"))
  }

  tbl <- tbl |>
    gt::tab_header(
      title = "Steady-State Detection Summary",
      subtitle = paste(subtitle_parts, collapse = " | ")
    )

  # Add overall summary as source note
  overall_steady <- mean(summary_df$pct_steady)
  tbl <- tbl |>
    gt::tab_source_note(
      source_note = paste0("Overall: ", round(overall_steady, 1), "% of data in steady-state")
    )

  tbl
}
