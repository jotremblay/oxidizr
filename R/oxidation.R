#' Substrate Oxidation Functions
#'
#' @name oxidation
#' @description Main functions for calculating substrate oxidation rates
NULL

#' Calculate Substrate Oxidation
#'
#' Calculate carbohydrate and fat oxidation rates from indirect calorimetry
#' using stoichiometric equations.
#'
#' @param calo A CalorimetryData object
#' @param protein_ox Optional protein oxidation data (from calc_protein_oxidation)
#' @param coefficients Stoichiometric coefficients (default: stoich_coefficients)
#' @param na_protein_action How to handle NA values in protein oxidation data.
#'   One of:
#'   - `"mean"` (default): Replace NA with group mean, then 0 if all NA
#'   - `"zero"`: Replace NA with 0 (assume negligible protein contribution)
#'   - `"error"`: Raise an error if any NA values are present
#'
#' @return A tibble with CHO and fat oxidation rates (g/min)
#' @export
#'
#' @details
#' The coefficients come from [stoich_coefficients], which solves the
#' balanced oxidation reactions of glucose and a triacylglycerol (Telmosse,
#' 2022; see `vignette("stoichiometry")`). With the default coefficients:
#'
#' Non-protein gas exchange:
#' - VO2np = VO2 - protein_ox * 1.0103
#' - VCO2np = VCO2 - protein_ox * 0.8436
#'
#' Substrate oxidation:
#' - CHO (g/min) = 4.6211 * VCO2np - 3.2815 * VO2np
#' - Fat (g/min) = 1.7137 * VO2np - 1.7137 * VCO2np
#'
#' If `protein_ox` is a data frame, it is joined by the ID column, and also by
#' the protocol column when both tables have it. Each calorimetry row must
#' match at most one protein row.
#'
#' ## NA Handling Strategies
#'
#' When protein oxidation data contains NA values, the `na_protein_action`
#' parameter controls behavior:
#'
#' - **"mean"**: Imputes missing values with the mean of available protein
#'   oxidation values. If all values are NA, uses 0. This preserves the
#'   group average contribution but may not be appropriate for studies
#'   with large inter-subject variability.
#'
#' - **"zero"**: Treats missing protein oxidation as zero contribution.
#'   Appropriate when protein contribution is expected to be negligible
#'   (< 5% of total energy) or when conservative estimates are preferred.
#'
#' - **"error"**: Raises an error if any NA values are present. Recommended
#'   for automated pipelines where missing data should halt processing.
#'
#' @examples
#' \dontrun{
#' # Basic calculation (assuming negligible protein contribution)
#' ox <- calc_substrate_oxidation(calo_data)
#'
#' # With protein correction
#' protein <- calc_protein_oxidation(urea_loss)
#' ox <- calc_substrate_oxidation(calo_data, protein_ox = protein)
#'
#' # Strict mode for automated pipelines
#' ox <- calc_substrate_oxidation(calo_data, protein_ox = protein,
#'                                na_protein_action = "error")
#' }
calc_substrate_oxidation <- function(calo,
                                     protein_ox = NULL,
                                     coefficients = stoich_coefficients,
                                     na_protein_action = c("mean", "zero", "error")) {

  na_protein_action <- match.arg(na_protein_action)

  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }

  calo_df <- calo@data
  id_col <- calo@id_col
  time_col <- calo@time_col
  vo2_col <- calo@vo2_col
  vco2_col <- calo@vco2_col
  vo2_unit <- calo@vo2_unit
  protocol_col <- calo@protocol_col

  # Convert to L/min if needed
  unit_factor <- if (vo2_unit == "mL/min") 1 / 1000 else 1

  result <- calo_df |>
    dplyr::mutate(
      vo2_lmin = .data[[vo2_col]] * unit_factor,
      vco2_lmin = .data[[vco2_col]] * unit_factor
    )

  # Add protein oxidation if provided
  if (!is.null(protein_ox)) {
    if (is.data.frame(protein_ox)) {
      if (!"protein_ox" %in% names(protein_ox)) {
        cli::cli_abort("protein_ox must contain 'protein_ox' column")
      }
      # Join protein data on ID, plus protocol when both tables have it
      by <- c(id_col, intersect(protocol_col, names(protein_ox)))
      result <- result |>
        dplyr::left_join(
          protein_ox |> dplyr::select(dplyr::all_of(c(by, "protein_ox"))),
          by = by,
          relationship = "many-to-one"
        )
      # Keep the calorimetry protocol column (a join can change its type)
      if (length(by) > 1) result[[protocol_col]] <- calo_df[[protocol_col]]
    } else if (is.numeric(protein_ox)) {
      result <- result |>
        dplyr::mutate(protein_ox = protein_ox)
    }

    # Handle NA values in protein oxidation based on na_protein_action
    n_na <- sum(is.na(result$protein_ox))
    if (n_na > 0) {
      if (na_protein_action == "error") {
        cli::cli_abort(c(
          "Found {n_na} NA value{?s} in protein oxidation data.",
          "i" = "Use {.arg na_protein_action = 'mean'} or {.code 'zero'} to handle NAs.",
          "i" = "Or ensure protein_ox data has no missing values."
        ))
      } else if (na_protein_action == "zero") {
        cli::cli_alert_info("Replacing {n_na} NA protein oxidation value{?s} with 0.")
        result <- result |>
          dplyr::mutate(protein_ox = dplyr::if_else(is.na(.data$protein_ox), 0, .data$protein_ox))
      } else {
        # na_protein_action == "mean"
        mean_val <- mean(result$protein_ox, na.rm = TRUE)
        if (is.na(mean_val)) {
          cli::cli_alert_warning("All protein oxidation values are NA; using 0.")
          mean_val <- 0
        } else {
          cli::cli_alert_info("Replacing {n_na} NA protein oxidation value{?s} with mean ({round(mean_val, 4)} g/min).")
        }
        result <- result |>
          dplyr::mutate(protein_ox = dplyr::if_else(is.na(.data$protein_ox), mean_val, .data$protein_ox))
      }
    }

    # Calculate non-protein gas exchange
    result <- result |>
      dplyr::mutate(
        vo2_protein = .data$protein_ox * coefficients$protein_vo2,
        vco2_protein = .data$protein_ox * coefficients$protein_vco2,
        vo2_np = .data$vo2_lmin - .data$vo2_protein,
        vco2_np = .data$vco2_lmin - .data$vco2_protein
      )
  } else {
    # No protein correction - use total values
    result <- result |>
      dplyr::mutate(
        protein_ox = 0,
        vo2_np = .data$vo2_lmin,
        vco2_np = .data$vco2_lmin
      )
  }

  # Calculate substrate oxidation
  result <- result |>
    dplyr::mutate(
      cho_total = coefficients$cho_vco2 * .data$vco2_np +
        coefficients$cho_vo2 * .data$vo2_np,
      fat_total = coefficients$fat_vco2 * .data$vco2_np +
        coefficients$fat_vo2 * .data$vo2_np,
      rer = .data$vco2_lmin / .data$vo2_lmin,
      rer_np = .data$vco2_np / .data$vo2_np
    )

  # Select output columns
  out_cols <- c(id_col, time_col)
  if (!is.null(protocol_col)) out_cols <- c(out_cols, protocol_col)
  out_cols <- c(out_cols, "cho_total", "fat_total", "protein_ox", "rer", "rer_np",
                "vo2_lmin", "vco2_lmin")

  result <- result |>
    dplyr::select(dplyr::all_of(out_cols)) |>
    dplyr::mutate(
      cho_total = pmax(.data$cho_total, 0),
      fat_total = pmax(.data$fat_total, 0)
    )

  tibble::as_tibble(result)
}

#' Calculate CHO Source Partitioning
#'
#' Partition total CHO oxidation into exogenous, endogenous, muscle, and liver sources.
#'
#' @param substrate_ox Substrate oxidation from calc_substrate_oxidation
#' @param cho_exo Exogenous CHO oxidation from calc_exogenous_cho
#' @param cho_pla Plasma CHO oxidation from calc_plasma_cho (optional)
#' @param id_col Name of the ID column (auto-detected if NULL)
#' @param time_col Name of the time column (auto-detected if NULL)
#' @param protocol_col Name of the protocol column (default: "protocol").
#'   When all inputs have it, it is part of the join keys, so that each
#'   protocol of a crossover study keeps its own values.
#'
#' @return A tibble with partitioned CHO oxidation rates
#' @export
#'
#' @details
#' Partitioning calculations:
#' - CHOendo = CHOtot - CHOexo (endogenous = total - exogenous)
#' - CHOmus = CHOtot - CHOpla (muscle = total - plasma-derived)
#' - CHOliv = CHOpla - CHOexo (liver output = plasma - exogenous)
#'
#' Column auto-detection looks for common columns between inputs matching
#' patterns "id", "ID", "subject" for ID and "time", "Time", "timepoint" for time.
#' If no match is found, an error is raised. Use explicit column parameters
#' when using non-standard column names.
calc_cho_partition <- function(substrate_ox,
                               cho_exo,
                               cho_pla = NULL,
                               id_col = NULL,
                               time_col = NULL,
                               protocol_col = "protocol") {

  # Identify common columns between inputs
  common_cols <- intersect(names(substrate_ox), names(cho_exo))

  # Auto-detect or validate ID column

  if (is.null(id_col)) {
    id_candidates <- common_cols[common_cols %in% c("id", "ID", "subject")]
    if (length(id_candidates) == 0) {
      cli::cli_abort(c(
        "Cannot auto-detect ID column.",
        "i" = "No common column matching 'id', 'ID', or 'subject' found.",
        "i" = "Use {.arg id_col} to specify the ID column explicitly."
      ))
    }
    id_col <- id_candidates[1]
  } else if (!id_col %in% common_cols) {
    cli::cli_abort(c(
      "ID column {.val {id_col}} not found in both inputs.",
      "i" = "Common columns: {.val {common_cols}}"
    ))
  }

  # Auto-detect or validate time column
  if (is.null(time_col)) {
    time_candidates <- common_cols[common_cols %in% c("time", "Time", "timepoint")]
    if (length(time_candidates) == 0) {
      cli::cli_abort(c(
        "Cannot auto-detect time column.",
        "i" = "No common column matching 'time', 'Time', or 'timepoint' found.",
        "i" = "Use {.arg time_col} to specify the time column explicitly."
      ))
    }
    time_col <- time_candidates[1]
  } else if (!time_col %in% common_cols) {
    cli::cli_abort(c(
      "Time column {.val {time_col}} not found in both inputs.",
      "i" = "Common columns: {.val {common_cols}}"
    ))
  }

  # Join substrate and exogenous data
  keys <- c(id_col, time_col, intersect(protocol_col, common_cols))
  result <- substrate_ox |>
    dplyr::left_join(
      cho_exo |> dplyr::select(dplyr::all_of(c(keys, "cho_exo"))),
      by = keys,
      relationship = "many-to-one"
    )

  # Calculate endogenous CHO
  result <- result |>
    dplyr::mutate(
      cho_endo = .data$cho_total - .data$cho_exo,
      cho_endo = pmax(.data$cho_endo, 0)  # Ensure non-negative
    )

  # If plasma CHO available, calculate muscle and liver
  if (!is.null(cho_pla)) {
    result <- result |>
      dplyr::left_join(
        cho_pla |> dplyr::select(dplyr::all_of(c(intersect(keys, names(cho_pla)), "cho_pla"))),
        by = intersect(keys, names(cho_pla)),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        cho_mus = .data$cho_total - .data$cho_pla,
        cho_liv = .data$cho_pla - .data$cho_exo,
        cho_mus = pmax(.data$cho_mus, 0),
        cho_liv = pmax(.data$cho_liv, 0)
      )
  }

  tibble::as_tibble(result)
}

#' Create Oxidation Study Object
#'
#' Convenience function to create an OxidationStudy object from component data.
#'
#' @param calorimetry A CalorimetryData object or data frame
#' @param isotopes An IsotopeData object (optional)
#' @param urea A UreaData object (optional)
#' @param environment An EnvironmentData object (optional)
#' @param subjects A SubjectData object or data frame (optional)
#' @param protocols Character vector or factor of protocol names (optional)
#' @param ... Additional arguments for CalorimetryData if data frame provided
#'
#' @return An OxidationStudy S7 object
#' @export
oxidation_study <- function(calorimetry,
                            isotopes = NULL,
                            urea = NULL,
                            environment = NULL,
                            subjects = NULL,
                            protocols = NULL,
                            ...) {

  # Convert calorimetry to S7 if data frame
  if (is.data.frame(calorimetry) && !S7_inherits(calorimetry, CalorimetryData)) {
    calorimetry <- CalorimetryData(data = calorimetry, ...)
  }

  # Convert subjects to S7 if data frame
  if (!is.null(subjects) && is.data.frame(subjects) && !S7_inherits(subjects, SubjectData)) {
    subjects <- SubjectData(data = subjects)
  }

  # Convert protocols to factor
  if (!is.null(protocols) && !is.factor(protocols)) {
    protocols <- as.factor(protocols)
  }

  OxidationStudy(
    calorimetry = calorimetry,
    isotopes = isotopes,
    urea = urea,
    environment = environment,
    subjects = subjects,
    protocols = protocols
  )
}

#' Analyze Oxidation (Main Workflow)
#'
#' Main analysis function that performs complete oxidation analysis.
#'
#' @param study An OxidationStudy object
#' @param time_range Optional time range to filter (vector of 2)
#' @param aggregate Logical, aggregate by time point (default: FALSE). The
#'   aggregated data keep the SD and number of replicates, which give the
#'   data-based uncertainty of VO2 and VCO2.
#' @param calc_energy Logical, calculate energy contributions (default: TRUE)
#' @param control_protocol Name of control protocol for Rref (required if isotopes present)
#' @param validate Logical, whether to validate data before analysis (default: TRUE)
#' @param strict Logical, whether to stop on validation errors (default: FALSE).
#'   If FALSE (default), warnings are issued but analysis continues.
#'   If TRUE, analysis stops if validation errors are found.
#' @param validation_thresholds Optional custom thresholds list (see calorimetry_thresholds)
#' @param exercise_duration Exercise duration in minutes, used for the sweat
#'   loss and the protein oxidation rate (default: 120)
#' @param constants The constants of the calculations, from
#'   [oxidation_constants()] (default: the oxidizr defaults).
#' @param uncertainty The input uncertainties to propagate, from
#'   [oxidation_uncertainty()]. The default propagates only what the data
#'   show (replicate SDs, control-group SD of Rref). Use NULL for no
#'   propagation.
#' @param rref_by_subject Logical, take Rref from each subject's own control
#'   trial (crossover studies; see [calc_rref()]). Default: FALSE.
#'
#' @return An OxidationResults S7 object. When an uncertainty was propagated,
#'   `oxidation_rates` and `energy_contributions` have, for each variable,
#'   `<var>_u` (standard uncertainty) and `<var>_u_sys` (its systematic part),
#'   and the `uncertainty` slot holds every contribution (see
#'   [uncertainty_budget()]).
#' @export
#'
#' @examples
#' \dontrun{
#' study <- oxidation_study(
#'   calorimetry = calo_data,
#'   isotopes = iso_data,
#'   subjects = subject_data
#' )
#' results <- analyze_oxidation(study, time_range = c(30, 120))
#'
#' # With strict validation
#' results <- analyze_oxidation(study, validate = TRUE, strict = TRUE)
#'
#' # Other constants, and stated measurement uncertainties
#' results <- analyze_oxidation(
#'   study,
#'   constants = oxidation_constants(molar_volume = "real_gas"),
#'   uncertainty = oxidation_uncertainty(vo2 = 0.01, vco2 = 0.01, rexp = 0.1)
#' )
#' }
analyze_oxidation <- function(study,
                              time_range = NULL,
                              aggregate = FALSE,
                              calc_energy = TRUE,
                              control_protocol = NULL,
                              validate = TRUE,
                              strict = FALSE,
                              validation_thresholds = NULL,
                              exercise_duration = 120,
                              constants = oxidation_constants(),
                              uncertainty = oxidation_uncertainty(),
                              rref_by_subject = FALSE) {

  if (!S7_inherits(study, OxidationStudy)) {
    cli::cli_abort("study must be an OxidationStudy object")
  }
  if (!is.null(uncertainty) && !inherits(uncertainty, "oxidation_uncertainty")) {
    cli::cli_abort("{.arg uncertainty} must come from {.fn oxidation_uncertainty} or be NULL.")
  }

  cli::cli_h1("Analyzing Oxidation")

  # Validate data before analysis
  validation_result <- NULL
  if (validate) {
    cli::cli_alert_info("Validating study data...")
    validation_result <- validate_study(
      study,
      strict = FALSE,  # Always get full results first
      thresholds = validation_thresholds,
      verbose = FALSE
    )

    if (!validation_result@passed) {
      n_errors <- validation_result@severity_summary$error %||% 0
      n_warnings <- validation_result@severity_summary$warning %||% 0

      if (strict && n_errors > 0) {
        cli::cli_alert_danger("Validation failed with {n_errors} error(s)")
        cli::cli_inform("Run validate_study(study) for details")
        cli::cli_abort("Analysis stopped due to validation errors (strict = TRUE)")
      } else if (n_errors > 0) {
        cli::cli_alert_warning(
          "Validation found {n_errors} error(s), {n_warnings} warning(s) - proceeding with caution"
        )
        cli::cli_inform("Run validate_study(study) for details")
      } else if (n_warnings > 0) {
        cli::cli_alert_info("Validation found {n_warnings} warning(s)")
      }
    } else {
      cli::cli_alert_success("Validation passed")
    }
  }

  # Filter and aggregate the calorimetry data used by every step
  calo <- study@calorimetry
  if (!is.null(time_range)) {
    cli::cli_alert_info("Filtering to time range: {time_range[1]} - {time_range[2]}")
    calo <- filter_time_range(calo, time_range)
  }
  if (aggregate) {
    cli::cli_alert_info("Aggregating calorimetry data")
    calo <- aggregate_calorimetry(calo)
  }
  used <- study
  used@calorimetry <- calo

  if (has_protein_data(used)) cli::cli_alert_info("Calculating protein oxidation from urea")
  cli::cli_alert_info("Calculating substrate oxidation")
  if (!is.null(used@isotopes)) {
    if (is.null(control_protocol)) {
      cli::cli_warn("control_protocol not specified - skipping isotope calculations")
    } else {
      cli::cli_alert_info("Calculating exogenous CHO oxidation")
    }
  }
  if (calc_energy) cli::cli_alert_info("Calculating energy contributions")

  run <- function(study, rref_fn = identity) {
    run_pipeline(study, constants, control_protocol, exercise_duration,
                 rref_by_subject, calc_energy, rref_fn)
  }
  out <- run(used)
  if (rref_by_subject && !is.null(out$rref) && !is.null(calo@protocol_col)) {
    no_rref <- out$oxidation[[calo@id_col]][is.na(out$oxidation$cho_exo) &
                                              out$oxidation[[calo@protocol_col]] != control_protocol]
    if (length(no_rref) > 0) {
      cli::cli_warn(c(
        "No own control value for subject{?s} {.val {unique(no_rref)}} at some time points.",
        "i" = "Their exogenous CHO is NA. Use {.code rref_by_subject = FALSE} for the group Rref."
      ))
    }
  }

  # Propagate the input uncertainties
  unc <- NULL
  if (!is.null(uncertainty)) {
    unc <- propagate_uncertainty(used, uncertainty, out, run, control_protocol, rref_by_subject)
    if (!is.null(unc)) out <- add_uncertainty_columns(out, unc)
  }

  # Store settings
  settings <- list(
    time_range = time_range,
    aggregate = aggregate,
    control_protocol = control_protocol,
    calc_energy = calc_energy,
    validate = validate,
    strict = strict,
    exercise_duration = exercise_duration,
    rref_by_subject = rref_by_subject,
    uncertainty = uncertainty
  )

  # Add validation result to settings if available
  if (!is.null(validation_result)) {
    settings$validation <- validation_result
  }

  cli::cli_alert_success("Analysis complete")

  OxidationResults(
    oxidation_rates = as.data.frame(out$oxidation),
    energy_contributions = if (!is.null(out$energy)) as.data.frame(out$energy) else NULL,
    partition = if (!is.null(out$partition)) as.data.frame(out$partition) else NULL,
    study = study,
    settings = settings,
    constants = constants,
    uncertainty = unc
  )
}

# TRUE when the study has what the protein (urea) path needs
has_protein_data <- function(study) {
  !is.null(study@urea) && !is.null(study@environment) && !is.null(study@subjects)
}

# The calculation steps of analyze_oxidation(), with no messages. The
# uncertainty code runs it again on changed inputs. `rref_fn` changes the
# Rref table (from calc_rref()) before it is used.
run_pipeline <- function(study, constants, control_protocol, exercise_duration,
                         rref_by_subject = FALSE, calc_energy = TRUE,
                         rref_fn = identity) {
  calo <- study@calorimetry
  # A plain coefficient list (as stoich_coefficients) has no energy part
  energy_k <- constants$energy %||% energy_factors
  co2_k <- constants$co2_cho_ratio %||% stoich_coefficients$co2_cho_ratio

  # Protein oxidation from urea
  protein_ox <- NULL
  if (has_protein_data(study)) {
    # When the urine volume is only in the urea data, use it in the sweat
    # mass balance too, so that the urine is not counted as sweat
    subjects <- study@subjects
    urine_col <- subjects@fluid_cols$urine
    urea <- study@urea
    if (!is.null(urine_col) && !urine_col %in% names(subjects@data) &&
        urea@urine_vol_col %in% names(urea@data)) {
      proto <- intersect(subjects@protocol_col, intersect(names(subjects@data), names(urea@data)))
      urine <- urea@data[, c(urea@id_col, proto, urea@urine_vol_col), drop = FALSE]
      names(urine) <- c(subjects@id_col, proto, urine_col)
      subjects@data <- dplyr::left_join(subjects@data, urine,
                                        by = c(subjects@id_col, proto),
                                        relationship = "many-to-one")
    }

    sweat_loss <- calc_sweat_loss(
      subjects = subjects,
      calo = calo,
      environment = study@environment,
      exercise_duration = exercise_duration
    )
    urea_loss <- calc_urea_loss(urea = urea, sweat_loss = sweat_loss, subjects = subjects)
    protein_ox <- calc_protein_oxidation(
      urea_loss,
      exercise_duration = exercise_duration,
      conversion_factor = constants$protein_urea %||% stoich_coefficients$protein_urea,
      id_col = urea@id_col,
      protocol_col = study@subjects@protocol_col
    )
  }

  substrate_ox <- calc_substrate_oxidation(calo, protein_ox = protein_ox, coefficients = constants)

  # CHO partitioning from the 13C data
  partition <- NULL
  rref <- NULL
  if (!is.null(study@isotopes) && !is.null(control_protocol)) {
    keys <- intersect(c(calo@id_col, calo@time_col, calo@protocol_col), names(calo@data))
    if (anyDuplicated(calo@data[keys]) > 0) {
      cli::cli_abort(c(
        "The calorimetry data have several rows for one subject, protocol and time.",
        "i" = "The 13C calculations need one row each: use {.code aggregate = TRUE}."
      ))
    }
    rref <- rref_fn(calc_rref(study@isotopes, control_protocol = control_protocol,
                              by_subject = rref_by_subject))
    cho_exo <- calc_exogenous_cho(calo, study@isotopes, rref, co2_cho_ratio = co2_k)
    # calc_exogenous_cho() turns NA into 0; with no Rref the value is unknown
    cho_exo$cho_exo[is.na(cho_exo$rref)] <- NA_real_
    cho_pla <- NULL
    if (!is.null(study@isotopes@rpla) && nrow(study@isotopes@rpla) > 0) {
      cho_pla <- calc_plasma_cho(calo, study@isotopes, rref, co2_cho_ratio = co2_k)
    }
    partition <- calc_cho_partition(substrate_ox, cho_exo, cho_pla,
                                    id_col = calo@id_col, time_col = calo@time_col,
                                    protocol_col = calo@protocol_col)
    substrate_ox <- partition
  }

  energy <- NULL
  if (calc_energy) {
    energy <- calc_energy_yield(substrate_ox,
                                cho_factor = energy_k$cho,
                                fat_factor = energy_k$fat,
                                protein_factor = energy_k$protein) |>
      calc_energy_percent()
  }

  list(oxidation = substrate_ox, partition = partition, energy = energy, rref = rref)
}
