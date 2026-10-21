#' Energy Calculation Functions
#'
#' @name energy
#' @description Functions for calculating energy yield from substrate oxidation
NULL

#' Calculate Energy Yield
#'
#' Convert substrate oxidation rates (g/min) to energy yield (kcal/min).
#'
#' @param oxidation A data frame with oxidation rates (from calc_substrate_oxidation)
#' @param cho_factor Energy per gram CHO (default: `energy_factors$cho`, 3.72 kcal/g)
#' @param fat_factor Energy per gram fat (default: `energy_factors$fat`, 9.44 kcal/g)
#' @param protein_factor Energy per gram protein (default:
#'   `energy_factors$protein`, 4.70 kcal/g)
#'
#' @return A tibble with energy yield columns added
#' @export
#'
#' @details
#' Default energy factors ([energy_factors]; IOM, 2005, Table 5-1, which
#' repeats Livesey & Elia, 1988):
#' - Carbohydrate: 3.72 kcal/g (glucose, 15.56 kJ/g)
#' - Fat: 9.44 kcal/g (general triacylglycerol, 39.5 kJ/g)
#' - Protein: 4.70 kcal/g (protein oxidized in the body, 19.68 kJ/g)
#'
#' These values can be customized for specific substrates.
calc_energy_yield <- function(oxidation,
                              cho_factor = energy_factors$cho,
                              fat_factor = energy_factors$fat,
                              protein_factor = energy_factors$protein) {

  if (!is.data.frame(oxidation)) {
    cli::cli_abort("oxidation must be a data frame")
  }

  result <- oxidation


  # Define substrate-to-energy column mappings with their factors

  energy_mappings <- list(
    list(from = "cho_total", to = "e_cho_total", factor = cho_factor),
    list(from = "fat_total", to = "e_fat", factor = fat_factor),
    list(from = "protein_ox", to = "e_protein", factor = protein_factor),
    list(from = "cho_exo", to = "e_cho_exo", factor = cho_factor),
    list(from = "cho_endo", to = "e_cho_endo", factor = cho_factor),
    list(from = "cho_pla", to = "e_cho_pla", factor = cho_factor),
    list(from = "cho_mus", to = "e_cho_mus", factor = cho_factor),
    list(from = "cho_liv", to = "e_cho_liv", factor = cho_factor)
  )

  # Apply energy calculations for available columns
  for (mapping in energy_mappings) {
    if (mapping$from %in% names(result)) {
      result[[mapping$to]] <- result[[mapping$from]] * mapping$factor
    }
  }

  # Calculate total energy
  energy_cols <- c("e_cho_total", "e_fat", "e_protein")
  available_energy <- intersect(energy_cols, names(result))

  if (length(available_energy) > 0) {
    result <- result |>
      dplyr::mutate(
        e_total = rowSums(dplyr::across(dplyr::all_of(available_energy)), na.rm = TRUE)
      )
  }

  tibble::as_tibble(result)
}

#' Calculate Energy Percentages
#'
#' Calculate the percentage contribution of each substrate to total energy.
#'
#' @param energy A data frame with energy yield columns (from calc_energy_yield)
#'
#' @return A tibble with percentage contribution columns added
#' @export
#'
#' @examples
#' \dontrun{
#' ox <- calc_substrate_oxidation(calo)
#' energy <- calc_energy_yield(ox)
#' energy_pct <- calc_energy_percent(energy)
#' }
calc_energy_percent <- function(energy) {

  if (!is.data.frame(energy)) {
    cli::cli_abort("energy must be a data frame")
  }

  if (!"e_total" %in% names(energy)) {
    cli::cli_abort("e_total column required - run calc_energy_yield first")
  }

  result <- energy

  # Calculate percentages for each energy component
  energy_cols <- c(
    "e_cho_total", "e_fat", "e_protein",
    "e_cho_exo", "e_cho_endo", "e_cho_pla", "e_cho_mus", "e_cho_liv"
  )

  for (col in energy_cols) {
    if (col %in% names(result)) {
      pct_col <- gsub("^e_", "pct_", col)
      result <- result |>
        dplyr::mutate(
          !!pct_col := .data[[col]] / .data$e_total * 100
        )
    }
  }

  tibble::as_tibble(result)
}

#' Calculate Total Energy Expenditure
#'
#' Calculate total energy expenditure from VO2 using the Weir equation.
#'
#' @param vo2 Oxygen consumption (L/min)
#' @param vco2 Carbon dioxide production (L/min)
#' @param method Calculation method: "weir" or "brockway" (default: "weir")
#'
#' @return Energy expenditure (kcal/min)
#' @export
#'
#' @details
#' Weir equation (1949):
#' EE (kcal/min) = 3.941 * VO2 + 1.106 * VCO2
#'
#' Brockway equation (1987):
#' EE (kcal/min) = 3.869 * VO2 + 1.195 * VCO2
calc_total_energy <- function(vo2, vco2, method = c("weir", "brockway")) {

  method <- match.arg(method)

  if (method == "weir") {
    # Weir (1949) equation
    ee <- 3.941 * vo2 + 1.106 * vco2
  } else {
    # Brockway (1987) equation
    ee <- 3.869 * vo2 + 1.195 * vco2
  }

  ee
}

#' Summarize Oxidation Results
#'
#' Summarise oxidation, gas exchange and energy columns with the subject as
#' the unit of analysis. Each subject's rows are first averaged within the
#' `by` groups; the subject means are then summarised across subjects. This
#' avoids pseudo-replication: `n` counts subjects, and the SD describes
#' between-subject variation only.
#'
#' @param data A data frame, e.g. `results@oxidation_rates` or
#'   `results@energy_contributions`.
#' @param vars Numeric columns to summarise. `NULL` selects every column
#'   present among `cho_total`, `fat_total`, `protein_ox`, `cho_exo`,
#'   `cho_endo`, `cho_pla`, `cho_mus`, `cho_liv`, `rer`, `vo2_lmin`,
#'   `vco2_lmin`, and every column starting with `e_` or `pct_`. Columns
#'   ending in `_u` or `_u_sys` (uncertainties) are never summarised as
#'   variables.
#' @param by Grouping column(s), e.g. `"protocol"` or `c("time", "protocol")`.
#' @param level Confidence level of the t-based interval (default 0.95).
#' @param unit Column identifying the unit of analysis (default `"id"`, the
#'   subject). If `NULL` or absent from `data`, each row is treated as a unit
#'   and a message says so.
#'
#' @return A tibble with the `by` columns, `n` (number of units), and for each
#'   variable `<var>_mean`, `<var>_sd`, `<var>_se` (SD / sqrt(k), with k the
#'   number of non-missing units), `<var>_ci_low` and `<var>_ci_high`
#'   (mean -/+ qt(1 - (1 - level) / 2, k - 1) * SE; `NA` when k < 2). When
#'   `data` has a `<var>_u_sys` column (systematic standard uncertainty), the
#'   result also has `<var>_u_sys`, its mean over units. Systematic error is
#'   shared by all subjects, so it is not divided by sqrt(n).
#' @export
#'
#' @examples
#' ox <- data.frame(
#'   id = c(1, 1, 2, 3),
#'   cho_total = c(1.0, 1.2, 1.5, 1.8)
#' )
#' summarize_oxidation(ox)
summarize_oxidation <- function(data,
                                vars = NULL,
                                by = NULL,
                                level = 0.95,
                                unit = "id") {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number between 0 and 1")
  }
  missing_by <- setdiff(by, names(data))
  if (length(missing_by) > 0) {
    cli::cli_abort("Grouping columns not found: {missing_by}")
  }

  if (is.null(vars)) {
    vars <- c(
      "cho_total", "fat_total", "protein_ox", "cho_exo", "cho_endo",
      "cho_pla", "cho_mus", "cho_liv", "rer", "vo2_lmin", "vco2_lmin",
      grep("^(e|pct)_", names(data), value = TRUE)
    )
  }
  vars <- intersect(vars, names(data))
  vars <- vars[!grepl("_u(_sys)?$", vars)]
  if (length(vars) == 0) {
    cli::cli_abort("No columns to summarise")
  }
  u_sys <- intersect(paste0(vars, "_u_sys"), names(data))

  if (!is.null(unit) && unit %in% names(data)) {
    data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(by, unit)))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(c(vars, u_sys)), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    cli::cli_inform("No subject column found: each row is a unit of the summary.")
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ stats::sd(.x, na.rm = TRUE),
          se = ~ stats::sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
          ci_low = ~ ci_bounds(.x, level)[1],
          ci_high = ~ ci_bounds(.x, level)[2]
        ),
        .names = "{.col}_{.fn}"
      ),
      dplyr::across(dplyr::all_of(u_sys), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' t-based confidence interval of a mean
#'
#' @param x Numeric vector (missing values dropped).
#' @param level Confidence level.
#' @return `c(low, high)`, or two `NA` when fewer than two values remain.
#' @noRd
ci_bounds <- function(x, level = 0.95) {
  x <- x[!is.na(x)]
  k <- length(x)
  if (k < 2) {
    return(c(NA_real_, NA_real_))
  }
  mean(x) + c(-1, 1) * stats::qt(1 - (1 - level) / 2, df = k - 1) *
    stats::sd(x) / sqrt(k)
}

#' Error-bar half-width from a summarize_oxidation() result
#'
#' @param summary Output of [summarize_oxidation()].
#' @param var Variable name.
#' @param error One of `"sd"`, `"se"` or `"ci"`.
#' @return Numeric vector of half-widths, one per summary row.
#' @noRd
error_halfwidth <- function(summary, var, error = c("sd", "se", "ci")) {
  error <- match.arg(error)
  if (error == "ci") {
    (summary[[paste0(var, "_ci_high")]] - summary[[paste0(var, "_ci_low")]]) / 2
  } else {
    summary[[paste0(var, "_", error)]]
  }
}

#' Name of the statistic shown, e.g. "Mean (95% CI)"
#' @noRd
error_label <- function(error, level = 0.95) {
  switch(error,
    sd = "Mean \u00b1 SD",
    se = "Mean \u00b1 SE",
    ci = paste0("Mean (", level * 100, "% CI)")
  )
}

#' Subject column to use as the summary unit
#'
#' @param results An OxidationResults object or a data frame.
#' @param df The data frame that will be summarised.
#' @return The study's calorimetry `id_col` when it is in `df`, otherwise
#'   `"id"` when it is in `df`, otherwise `NULL` (rows are the unit).
#' @noRd
summary_unit <- function(results, df) {
  cols <- "id"
  if (S7_inherits(results, OxidationResults) && !is.null(results@study)) {
    cols <- c(results@study@calorimetry@id_col, cols)
  }
  cols <- intersect(cols, names(df))
  if (length(cols) > 0) cols[1] else NULL
}

#' Summarize Energy Contributions
#'
#' Summarise the percentage (`pct_*`) and energy (`e_*`) columns with the
#' subject as the unit. A wrapper of [summarize_oxidation()].
#'
#' @param energy A data frame with energy and percentage columns
#' @param by Grouping variable(s) (character vector)
#' @inheritParams summarize_oxidation
#'
#' @return A tibble with `n` and, for each column, `<col>_mean`, `<col>_sd`,
#'   `<col>_se`, `<col>_ci_low` and `<col>_ci_high`
#' @export
summarize_energy <- function(energy, by = NULL, level = 0.95, unit = "id") {

  if (!is.data.frame(energy)) {
    cli::cli_abort("energy must be a data frame")
  }

  vars <- c(
    grep("^pct_", names(energy), value = TRUE),
    grep("^e_", names(energy), value = TRUE)
  )
  summarize_oxidation(energy, vars = vars, by = by, level = level, unit = unit)
}

#' Calculate Cumulative Energy
#'
#' Calculate cumulative energy expenditure over time.
#'
#' @param energy A data frame with energy columns
#' @param time_col Name of the time column
#' @param time_interval Time interval between measurements (minutes)
#' @param by Optional grouping variable(s)
#'
#' @return A tibble with cumulative energy columns added
#' @export
calc_cumulative_energy <- function(energy,
                                   time_col = "time",
                                   time_interval = 30,
                                   by = NULL) {

  if (!time_col %in% names(energy)) {
    cli::cli_abort("Time column '{time_col}' not found")
  }

  # Energy columns to accumulate
  energy_cols <- c("e_cho_total", "e_fat", "e_protein", "e_total",
                   "e_cho_exo", "e_cho_endo")
  available_cols <- intersect(energy_cols, names(energy))

  # Group if specified
  if (!is.null(by)) {
    result <- energy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by)))
  } else {
    result <- energy
  }

  # Calculate cumulative sums (multiply by time_interval for kcal)
  for (col in available_cols) {
    cum_col <- paste0("cum_", col)
    result <- result |>
      dplyr::arrange(.data[[time_col]]) |>
      dplyr::mutate(
        !!cum_col := cumsum(.data[[col]] * time_interval)
      )
  }

  if (!is.null(by)) {
    result <- result |> dplyr::ungroup()
  }

  tibble::as_tibble(result)
}
