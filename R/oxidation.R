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
#' Uses equations commonly attributed to Jequier et al. and Frayn,
#' with specific coefficients parameterized as presented in Telmosse (2022):
#'
#' Non-protein calculations:
#' - VO2np = VO2 - (protein_ox * 1.0075)
#' - VCO2np = VCO2 - (protein_ox * 0.8443)
#'
#' Substrate oxidation:
#' - CHO (g/min) = 4.618 * VCO2np - 3.279 * VO2np
#' - Fat (g/min) = -1.712 * VCO2np + 1.712 * VO2np
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
      # Join protein data
      result <- result |>
        dplyr::left_join(
          protein_ox |> dplyr::select(dplyr::all_of(c(id_col, "protein_ox"))),
          by = id_col
        )
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
    dplyr::select(dplyr::all_of(out_cols))

  # Ensure non-negative oxidation rates
  result <- result |>
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
                               time_col = NULL) {

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
  result <- substrate_ox |>
    dplyr::left_join(
      cho_exo |> dplyr::select(dplyr::all_of(c(id_col, time_col, "cho_exo"))),
      by = c(id_col, time_col)
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
        cho_pla |> dplyr::select(dplyr::all_of(c(id_col, time_col, "cho_pla"))),
        by = c(id_col, time_col)
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
#' @param aggregate Logical, aggregate by time point (default: FALSE)
#' @param calc_energy Logical, calculate energy contributions (default: TRUE)
#' @param control_protocol Name of control protocol for Rref (required if isotopes present)
#' @param validate Logical, whether to validate data before analysis (default: TRUE)
#' @param strict Logical, whether to stop on validation errors (default: FALSE).
#'   If FALSE (default), warnings are issued but analysis continues.
#'   If TRUE, analysis stops if validation errors are found.
#' @param validation_thresholds Optional custom thresholds list (see calorimetry_thresholds)
#'
#' @return An OxidationResults S7 object
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
#' # Skip validation
#' results <- analyze_oxidation(study, validate = FALSE)
#' }
analyze_oxidation <- function(study,
                              time_range = NULL,
                              aggregate = FALSE,
                              calc_energy = TRUE,
                              control_protocol = NULL,
                              validate = TRUE,
                              strict = FALSE,
                              validation_thresholds = NULL) {

  if (!S7_inherits(study, OxidationStudy)) {
    cli::cli_abort("study must be an OxidationStudy object")
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

  # Extract calorimetry data
  calo <- study@calorimetry

  # Filter time range if specified
  if (!is.null(time_range)) {
    cli::cli_alert_info("Filtering to time range: {time_range[1]} - {time_range[2]}")
    calo <- filter_time_range(calo, time_range)
  }

  # Aggregate if requested
  if (aggregate) {
    cli::cli_alert_info("Aggregating calorimetry data")
    calo <- aggregate_calorimetry(calo)
  }

  # Calculate protein oxidation if urea and environment data available
  protein_ox <- NULL
  if (!is.null(study@urea) && !is.null(study@environment) && !is.null(study@subjects)) {
    cli::cli_alert_info("Calculating protein oxidation from urea")

    sweat_loss <- calc_sweat_loss(
      subjects = study@subjects,
      calo = calo,
      environment = study@environment
    )

    urea_loss <- calc_urea_loss(
      urea = study@urea,
      sweat_loss = sweat_loss,
      subjects = study@subjects
    )

    protein_ox <- calc_protein_oxidation(urea_loss, id_col = study@urea@id_col)
  }

  # Calculate substrate oxidation
  cli::cli_alert_info("Calculating substrate oxidation")
  substrate_ox <- calc_substrate_oxidation(calo, protein_ox = protein_ox)

  # Calculate CHO partitioning if isotope data available
  partition <- NULL
  if (!is.null(study@isotopes)) {
    if (is.null(control_protocol)) {
      cli::cli_warn("control_protocol not specified - skipping isotope calculations")
    } else {
      cli::cli_alert_info("Calculating exogenous CHO oxidation")

      # Calculate Rref
      rref <- calc_rref(study@isotopes, control_protocol = control_protocol)

      # Calculate exogenous CHO
      cho_exo <- calc_exogenous_cho(calo, study@isotopes, rref)

      # Calculate plasma CHO if available
      cho_pla <- NULL
      if (!is.null(study@isotopes@rpla)) {
        cli::cli_alert_info("Calculating plasma CHO oxidation")
        cho_pla <- calc_plasma_cho(calo, study@isotopes, rref)
      }

      # Partition CHO sources
      partition <- calc_cho_partition(substrate_ox, cho_exo, cho_pla)
      substrate_ox <- partition
    }
  }

  # Calculate energy contributions
  energy <- NULL
  if (calc_energy) {
    cli::cli_alert_info("Calculating energy contributions")
    energy <- calc_energy_yield(substrate_ox)
    energy <- calc_energy_percent(energy)
  }

  # Store settings
  settings <- list(
    time_range = time_range,
    aggregate = aggregate,
    control_protocol = control_protocol,
    calc_energy = calc_energy,
    validate = validate,
    strict = strict
  )

  # Add validation result to settings if available
  if (!is.null(validation_result)) {
    settings$validation <- validation_result
  }

  cli::cli_alert_success("Analysis complete")

  OxidationResults(
    oxidation_rates = as.data.frame(substrate_ox),
    energy_contributions = if (!is.null(energy)) as.data.frame(energy) else NULL,
    partition = if (!is.null(partition)) as.data.frame(partition) else NULL,
    study = study,
    settings = settings
  )
}
