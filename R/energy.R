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
#' @param cho_factor Energy per gram CHO (default: 3.72 kcal/g)
#' @param fat_factor Energy per gram fat (default: 9.44 kcal/g)
#' @param protein_factor Energy per gram protein (default: 4.70 kcal/g)
#'
#' @return A tibble with energy yield columns added
#' @export
#'
#' @details
#' Default energy factors:
#' - Carbohydrate: 3.72 kcal/g (average of glucose, glycogen)
#' - Fat: 9.44 kcal/g (average triglyceride)
#' - Protein: 4.70 kcal/g (average amino acid mixture)
#'
#' These values can be customized for specific substrates.
calc_energy_yield <- function(oxidation,
                               cho_factor = 3.72,
                               fat_factor = 9.44,
                               protein_factor = 4.70) {

  if (!is.data.frame(oxidation)) {
    cli::cli_abort("oxidation must be a data frame")
  }

  result <- oxidation

  # Calculate energy from total CHO
  if ("cho_total" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_total = .data$cho_total * cho_factor)
  }

  # Calculate energy from fat
  if ("fat_total" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_fat = .data$fat_total * fat_factor)
  }

  # Calculate energy from protein
  if ("protein_ox" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_protein = .data$protein_ox * protein_factor)
  }

  # Calculate energy from partitioned CHO sources
  if ("cho_exo" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_exo = .data$cho_exo * cho_factor)
  }
  if ("cho_endo" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_endo = .data$cho_endo * cho_factor)
  }
  if ("cho_pla" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_pla = .data$cho_pla * cho_factor)
  }
  if ("cho_mus" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_mus = .data$cho_mus * cho_factor)
  }
  if ("cho_liv" %in% names(result)) {
    result <- result |>
      dplyr::mutate(e_cho_liv = .data$cho_liv * cho_factor)
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

#' Summarize Energy Contributions
#'
#' Calculate summary statistics for energy contributions by group.
#'
#' @param energy A data frame with energy and percentage columns
#' @param by Grouping variable(s) (character vector)
#'
#' @return A tibble with summary statistics
#' @export
summarize_energy <- function(energy, by = NULL) {

  if (!is.data.frame(energy)) {
    cli::cli_abort("energy must be a data frame")
  }

  # Identify percentage columns
  pct_cols <- names(energy)[grepl("^pct_", names(energy))]
  energy_cols <- names(energy)[grepl("^e_", names(energy))]

  summary_cols <- c(pct_cols, energy_cols)
  available_cols <- intersect(summary_cols, names(energy))

  if (is.null(by)) {
    # Overall summary
    result <- energy |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(available_cols),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
          ),
          .names = "{.col}_{.fn}"
        ),
        n = dplyr::n()
      )
  } else {
    # Summary by group
    missing_by <- setdiff(by, names(energy))
    if (length(missing_by) > 0) {
      cli::cli_abort("Grouping columns not found: {paste(missing_by, collapse = ', ')}")
    }

    result <- energy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(available_cols),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
          ),
          .names = "{.col}_{.fn}"
        ),
        n = dplyr::n(),
        .groups = "drop"
      )
  }

  tibble::as_tibble(result)
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
