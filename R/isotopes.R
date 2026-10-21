#' Isotope Tracer Functions
#'
#' @name isotopes
#' @description Functions for 13C stable isotope tracer analysis
NULL

#' Calculate Reference Enrichment (Rref)
#'
#' Calculate baseline 13C enrichment from control/placebo condition.
#' This represents the natural 13C abundance in expired CO2.
#'
#' @param isotopes An IsotopeData object
#' @param control_protocol Name of the control/placebo protocol
#' @param by_time Logical, calculate Rref at each time point (default: TRUE)
#' @param by_subject Logical, calculate one Rref for each subject from their
#'   own control trial (default: FALSE). Use it in crossover studies: it
#'   removes the between-subject spread of the background enrichment from the
#'   error of each subject. With FALSE, every subject gets the control-group
#'   mean.
#'
#' @return A tibble with Rref values: `rref`, `rref_sd` (SD over the control
#'   observations of each group) and `n` (number of non-missing values), with the time column and, if
#'   `by_subject`, the ID column.
#' @export
calc_rref <- function(isotopes, control_protocol, by_time = TRUE, by_subject = FALSE) {

  if (!S7_inherits(isotopes, IsotopeData)) {
    cli::cli_abort("isotopes must be an IsotopeData object")
  }

  rexp_df <- isotopes@rexp
  protocol_col <- isotopes@protocol_col
  time_col <- isotopes@time_col
  rexp_col <- isotopes@rexp_col

  if (is.null(protocol_col)) {
    cli::cli_abort("Protocol column must be specified in IsotopeData")
  }

  # Filter to control condition
  control_data <- rexp_df |>
    dplyr::filter(.data[[protocol_col]] == control_protocol)

  if (nrow(control_data) == 0) {
    cli::cli_abort("No data found for control protocol '{control_protocol}'")
  }

  # Calculate Rref for each group (subject and/or time, or all)
  groups <- c(if (by_subject) isotopes@id_col, if (by_time) time_col)
  rref <- control_data |>
    dplyr::summarise(
      rref = mean(.data[[rexp_col]], na.rm = TRUE),
      rref_sd = sd(.data[[rexp_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[rexp_col]])),
      .by = dplyr::all_of(groups)
    )

  tibble::as_tibble(rref)
}

#' Calculate Exogenous CHO Oxidation
#'
#' Calculate the rate of exogenous (ingested) carbohydrate oxidation
#' using 13C isotope enrichment data.
#'
#' @param calo A CalorimetryData object
#' @param isotopes An IsotopeData object
#' @param rref Reference enrichment (from calc_rref or data frame with rref column)
#' @param co2_cho_ratio L CO2 produced per g glucose oxidized (default:
#'   `stoich_coefficients$co2_cho_ratio`, 0.7465 L/g)
#'
#' @return A tibble with exogenous CHO oxidation rates (g/min)
#' @export
#'
#' @details
#' The calculation uses the formula:
#' CHOexo = VCO2 * ((Rexp - Rref) / (Rexo - Rref)) / k
#'
#' Where:
#' - Rexp = 13C enrichment of expired CO2
#' - Rref = background 13C enrichment of expired CO2, from the control trial
#' - Rexo = 13C enrichment of the ingested substrate
#' - k = L CO2 per g glucose oxidized, from the glucose reaction
#'   C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O: 6 x 22.414 / 180.16 = 0.7465
#'
#' This is the Mosora equation, from a 13C mass balance on expired CO2
#' (Telmosse, 2022, pp. 66-68). Telmosse uses k = 0.747 (6 x 22.4 / 180).
#' Taking Rref from an exercise trial without exogenous glucose limits the
#' overestimation caused by the rise of background enrichment during exercise
#' (Telmosse, 2022, pp. 70-71). See `vignette("stoichiometry")`.
calc_exogenous_cho <- function(calo,
                               isotopes,
                               rref,
                               co2_cho_ratio = stoich_coefficients$co2_cho_ratio) {

  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }
  if (!S7_inherits(isotopes, IsotopeData)) {
    cli::cli_abort("isotopes must be an IsotopeData object")
  }

  # Extract data
  calo_df <- calo@data
  rexp_df <- isotopes@rexp
  rexo_df <- isotopes@rexo

  id_col <- calo@id_col
  time_col <- calo@time_col
  vco2_col <- calo@vco2_col
  vo2_unit <- calo@vo2_unit
  protocol_col <- calo@protocol_col

  iso_id_col <- isotopes@id_col
  iso_time_col <- isotopes@time_col
  iso_protocol_col <- isotopes@protocol_col
  rexp_col <- isotopes@rexp_col
  rexo_col <- isotopes@rexo_col

  # Unit conversion
  unit_factor <- if (vo2_unit == "mL/min") 1 / 1000 else 1

  # Start with calorimetry data
  result <- calo_df |>
    dplyr::mutate(vco2_lmin = .data[[vco2_col]] * unit_factor)

  # Join Rexp on ID and time, plus protocol when both tables have it
  keys <- iso_keys(calo, isotopes, rexp_df)
  result <- result |>
    dplyr::left_join(
      rexp_df |> dplyr::select(dplyr::all_of(c(keys, rexp_col))),
      by = keys,
      relationship = "many-to-one"
    )

  # Join Rexo (by protocol)
  if (!is.null(protocol_col) && !is.null(iso_protocol_col)) {
    result <- result |>
      dplyr::left_join(
        rexo_df |> dplyr::select(dplyr::all_of(c(iso_protocol_col, rexo_col))),
        by = stats::setNames(iso_protocol_col, protocol_col)
      )
  }

  # Join or assign Rref
  result <- join_rref(result, rref, iso_id_col, iso_time_col, id_col, time_col)

  # Calculate exogenous CHO oxidation
  result <- result |>
    dplyr::mutate(
      cho_exo = .data$vco2_lmin *
        ((.data[[rexp_col]] - .data$rref) / (.data[[rexo_col]] - .data$rref)) /
        co2_cho_ratio
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, protocol_col)),
                  cho_exo, rexp = dplyr::all_of(rexp_col), rexo = dplyr::all_of(rexo_col), rref)

  tibble::as_tibble(result)
}

#' Calculate Fraction Exogenous (Fexo)
#'
#' Calculate the fraction of plasma glucose derived from exogenous sources.
#'
#' @param isotopes An IsotopeData object (must include rpla)
#' @param rref Reference enrichment value or data frame
#'
#' @return A tibble with Fexo values (%)
#' @export
#'
#' @details
#' Fexo = ((Rpla - Rref) / (Rexo - Rref)) * 100
#'
#' Where Rpla is the 13C enrichment of plasma glucose.
calc_fexo <- function(isotopes, rref) {

  if (!S7_inherits(isotopes, IsotopeData)) {
    cli::cli_abort("isotopes must be an IsotopeData object")
  }

  if (is.null(isotopes@rpla)) {
    cli::cli_abort("Plasma glucose enrichment (rpla) required for Fexo calculation")
  }

  rpla_df <- isotopes@rpla
  rexo_df <- isotopes@rexo

  time_col <- isotopes@time_col
  protocol_col <- isotopes@protocol_col
  rpla_col <- isotopes@rpla_col
  rexo_col <- isotopes@rexo_col

  result <- rpla_df

  # Join Rexo
  if (!is.null(protocol_col)) {
    result <- result |>
      dplyr::left_join(
        rexo_df |> dplyr::select(dplyr::all_of(c(protocol_col, rexo_col))),
        by = protocol_col
      )
  }

  # Add Rref
  result <- join_rref(result, rref, isotopes@id_col, time_col)

  # Calculate Fexo
  result <- result |>
    dplyr::mutate(
      fexo = ((.data[[rpla_col]] - .data$rref) / (.data[[rexo_col]] - .data$rref)) * 100
    )

  tibble::as_tibble(result)
}

#' Calculate Plasma CHO Oxidation
#'
#' Calculate plasma-derived carbohydrate oxidation using Rpla enrichment.
#'
#' @param calo A CalorimetryData object
#' @param isotopes An IsotopeData object (must include rpla)
#' @param rref Reference enrichment
#' @param co2_cho_ratio L CO2 produced per g glucose oxidized (default:
#'   `stoich_coefficients$co2_cho_ratio`, 0.7465 L/g)
#'
#' @return A tibble with plasma CHO oxidation rates
#' @export
#'
#' @details
#' CHOpla = VCO2 * ((Rexp - Rref) / (Rpla - Rref)) / k
#'
#' This represents total CHO oxidation from plasma glucose pool.
#' k is the L CO2 produced per g glucose (see [calc_exogenous_cho()]).
calc_plasma_cho <- function(calo,
                            isotopes,
                            rref,
                            co2_cho_ratio = stoich_coefficients$co2_cho_ratio) {

  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }
  if (!S7_inherits(isotopes, IsotopeData)) {
    cli::cli_abort("isotopes must be an IsotopeData object")
  }
  if (is.null(isotopes@rpla)) {
    cli::cli_abort("Plasma enrichment (rpla) required for CHOpla calculation")
  }

  calo_df <- calo@data
  rexp_df <- isotopes@rexp
  rpla_df <- isotopes@rpla

  id_col <- calo@id_col
  time_col <- calo@time_col
  vco2_col <- calo@vco2_col
  vo2_unit <- calo@vo2_unit
  protocol_col <- calo@protocol_col

  iso_id_col <- isotopes@id_col
  iso_time_col <- isotopes@time_col
  rexp_col <- isotopes@rexp_col
  rpla_col <- isotopes@rpla_col

  unit_factor <- if (vo2_unit == "mL/min") 1 / 1000 else 1

  result <- calo_df |>
    dplyr::mutate(vco2_lmin = .data[[vco2_col]] * unit_factor)

  # Join Rexp on ID and time, plus protocol when both tables have it
  keys <- iso_keys(calo, isotopes, rexp_df)
  result <- result |>
    dplyr::left_join(
      rexp_df |> dplyr::select(dplyr::all_of(c(keys, rexp_col))),
      by = keys,
      relationship = "many-to-one"
    )

  # Join Rpla on ID and time, plus protocol when both tables have it
  keys <- iso_keys(calo, isotopes, rpla_df)
  result <- result |>
    dplyr::left_join(
      rpla_df |> dplyr::select(dplyr::all_of(c(keys, rpla_col))),
      by = keys,
      relationship = "many-to-one"
    )

  # Add Rref
  result <- join_rref(result, rref, iso_id_col, iso_time_col, id_col, time_col)

  # Calculate CHOpla
  result <- result |>
    dplyr::mutate(
      cho_pla = .data$vco2_lmin *
        ((.data[[rexp_col]] - .data$rref) / (.data[[rpla_col]] - .data$rref)) /
        co2_cho_ratio
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, protocol_col)), cho_pla)

  tibble::as_tibble(result)
}

# Add an rref column to `data`. `rref` is a number, or a data frame with an
# `rref` column and, as join keys, the isotope ID and/or time columns (from
# calc_rref()). A data frame with neither key gives its first value to all rows.
join_rref <- function(data, rref, iso_id_col, iso_time_col,
                      id_col = iso_id_col, time_col = iso_time_col) {
  if (is.numeric(rref)) {
    return(dplyr::mutate(data, rref = rref))
  }
  if (!is.data.frame(rref) || !"rref" %in% names(rref)) {
    cli::cli_abort("rref must be a number or a data frame with an {.field rref} column.")
  }
  keys <- intersect(c(iso_id_col, iso_time_col), names(rref))
  if (length(keys) == 0) {
    return(dplyr::mutate(data, rref = rref$rref[1]))
  }
  data_keys <- c(id_col, time_col)[match(keys, c(iso_id_col, iso_time_col))]
  dplyr::left_join(data, rref[c(keys, "rref")],
                   by = stats::setNames(keys, data_keys),
                   relationship = "many-to-one")
}

# Join keys from calorimetry to isotope data (calo name = isotope name): ID
# and time, plus protocol when both the calorimetry and `iso_df` have one.
iso_keys <- function(calo, isotopes, iso_df) {
  keys <- stats::setNames(c(isotopes@id_col, isotopes@time_col), c(calo@id_col, calo@time_col))
  if (!is.null(calo@protocol_col) && !is.null(isotopes@protocol_col) &&
      isotopes@protocol_col %in% names(iso_df)) {
    keys <- c(keys, stats::setNames(isotopes@protocol_col, calo@protocol_col))
  }
  keys
}
