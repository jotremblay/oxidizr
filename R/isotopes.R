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
#'
#' @return A tibble with Rref values
#' @export
calc_rref <- function(isotopes, control_protocol, by_time = TRUE) {

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

  # Calculate Rref
  if (by_time) {
    rref <- control_data |>
      dplyr::group_by(.data[[time_col]]) |>
      dplyr::summarise(
        rref = mean(.data[[rexp_col]], na.rm = TRUE),
        rref_sd = sd(.data[[rexp_col]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  } else {
    rref <- control_data |>
      dplyr::summarise(
        rref = mean(.data[[rexp_col]], na.rm = TRUE),
        rref_sd = sd(.data[[rexp_col]], na.rm = TRUE),
        n = dplyr::n()
      )
  }

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
#' @param co2_cho_ratio CO2/CHO conversion ratio (default: 0.747)
#'
#' @return A tibble with exogenous CHO oxidation rates (g/min)
#' @export
#'
#' @details
#' The calculation uses the formula:
#' CHOexo = VCO2 * ((Rexp - Rref) / (Rexo - Rref)) / 0.747
#'
#' Where:
#' - Rexp = 13C enrichment of expired CO2
#' - Rref = baseline 13C enrichment (from placebo)
#' - Rexo = 13C enrichment of ingested substrate
#' - 0.747 = L CO2 per gram CHO oxidized (Péronnet & Massicotte, 1991; Telmosse, 2022)
calc_exogenous_cho <- function(calo,
                               isotopes,
                               rref,
                               co2_cho_ratio = 0.747) {

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

  # Handle rref - can be a data frame or single value
  if (is.data.frame(rref)) {
    if ("rref" %in% names(rref)) {
      if (iso_time_col %in% names(rref)) {
        # Time-varying rref
        rref_join <- rref
      } else {
        # Single rref value
        rref_val <- rref$rref[1]
        rref_join <- NULL
      }
    } else {
      cli::cli_abort("rref data frame must contain 'rref' column")
    }
  } else if (is.numeric(rref)) {
    rref_val <- rref
    rref_join <- NULL
  } else {
    cli::cli_abort("rref must be a data frame or numeric value")
  }

  # Start with calorimetry data
  result <- calo_df |>
    dplyr::mutate(vco2_lmin = .data[[vco2_col]] * unit_factor)

  # Join Rexp
  result <- result |>
    dplyr::left_join(
      rexp_df |> dplyr::select(dplyr::all_of(c(iso_id_col, iso_time_col, rexp_col))),
      by = stats::setNames(c(iso_id_col, iso_time_col), c(id_col, time_col))
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
  if (!is.null(rref_join)) {
    result <- result |>
      dplyr::left_join(
        rref_join |> dplyr::select(dplyr::all_of(c(iso_time_col, "rref"))),
        by = stats::setNames(iso_time_col, time_col)
      )
  } else {
    result <- result |>
      dplyr::mutate(rref = rref_val)
  }

  # Calculate exogenous CHO oxidation
  result <- result |>
    dplyr::mutate(
      cho_exo = .data$vco2_lmin *
        ((.data[[rexp_col]] - .data$rref) / (.data[[rexo_col]] - .data$rref)) /
        co2_cho_ratio
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, protocol_col)),
                  cho_exo, rexp = dplyr::all_of(rexp_col), rexo = dplyr::all_of(rexo_col), rref)

  # Set negative values to 0 (can happen at early time points)
  result <- result |>
    dplyr::mutate(cho_exo = pmax(.data$cho_exo, 0, na.rm = TRUE))

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

  # Handle rref
  if (is.data.frame(rref)) {
    rref_val <- if (time_col %in% names(rref)) NULL else rref$rref[1]
  } else {
    rref_val <- rref
  }

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
  if (is.null(rref_val) && is.data.frame(rref)) {
    result <- result |>
      dplyr::left_join(
        rref |> dplyr::select(dplyr::all_of(c(time_col, "rref"))),
        by = time_col
      )
  } else {
    result <- result |>
      dplyr::mutate(rref = rref_val)
  }

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
#' @param co2_cho_ratio CO2/CHO ratio (default: 0.747)
#'
#' @return A tibble with plasma CHO oxidation rates
#' @export
#'
#' @details
#' CHOpla = VCO2 * ((Rexp - Rref) / (Rpla - Rref)) / 0.747
#'
#' This represents total CHO oxidation from plasma glucose pool.
#' The 0.747 L CO2 per gram CHO oxidized factor is based on Péronnet & Massicotte (1991) and presented in Telmosse (2022).
calc_plasma_cho <- function(calo,
                            isotopes,
                            rref,
                            co2_cho_ratio = 0.747) {

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

  # Handle rref
  if (is.data.frame(rref)) {
    rref_val <- if (iso_time_col %in% names(rref)) NULL else rref$rref[1]
  } else {
    rref_val <- rref
  }

  result <- calo_df |>
    dplyr::mutate(vco2_lmin = .data[[vco2_col]] * unit_factor)

  # Join Rexp
  result <- result |>
    dplyr::left_join(
      rexp_df |> dplyr::select(dplyr::all_of(c(iso_id_col, iso_time_col, rexp_col))),
      by = stats::setNames(c(iso_id_col, iso_time_col), c(id_col, time_col))
    )

  # Join Rpla
  result <- result |>
    dplyr::left_join(
      rpla_df |> dplyr::select(dplyr::all_of(c(iso_id_col, iso_time_col, rpla_col))),
      by = stats::setNames(c(iso_id_col, iso_time_col), c(id_col, time_col))
    )

  # Add Rref
  if (is.null(rref_val) && is.data.frame(rref)) {
    result <- result |>
      dplyr::left_join(
        rref |> dplyr::select(dplyr::all_of(c(iso_time_col, "rref"))),
        by = stats::setNames(iso_time_col, time_col)
      )
  } else {
    result <- result |>
      dplyr::mutate(rref = rref_val)
  }

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
