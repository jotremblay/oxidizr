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
