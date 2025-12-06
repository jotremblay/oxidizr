#' Protein Oxidation Functions
#'
#' @name protein
#' @description Functions for calculating protein oxidation from urea losses
NULL

#' Calculate Sweat Loss
#'
#' Estimate sweat loss from mass balance considering fluid intake and
#' respiratory gas/water losses.
#'
#' @param subjects A SubjectData object or data frame with mass/fluid data
#' @param calo A CalorimetryData object for respiratory calculations
#' @param environment An EnvironmentData object for vapor pressure calculations
#' @param exercise_duration Exercise duration in minutes (default: 120)
#'
#' @return A tibble with sweat loss estimates (kg)
#' @export
calc_sweat_loss <- function(subjects,
                             calo,
                             environment,
                             exercise_duration = 120) {

  # Extract data from S7 objects or use directly
  if (S7_inherits(subjects, SubjectData)) {
    subj_df <- subjects@data
    id_col <- subjects@id_col
    initial_mass_col <- subjects@mass_cols$initial
    final_mass_col <- subjects@mass_cols$final
    water_col <- subjects@fluid_cols$water
    saline_col <- subjects@fluid_cols$saline
  } else {
    cli::cli_abort("subjects must be a SubjectData object")
  }

  if (S7_inherits(environment, EnvironmentData)) {
    env_df <- environment@data
    temp_col <- environment@temp_col
    humidity_col <- environment@humidity_col
    pressure_col <- environment@pressure_col
    env_id_col <- environment@id_col
  } else {
    cli::cli_abort("environment must be an EnvironmentData object")
  }

  if (S7_inherits(calo, CalorimetryData)) {
    calo_df <- calo@data
    vo2_col <- calo@vo2_col
    vco2_col <- calo@vco2_col
    calo_id_col <- calo@id_col
    vo2_unit <- calo@vo2_unit
  } else {
    cli::cli_abort("calo must be a CalorimetryData object")
  }

  # Convert VO2/VCO2 to L/min if needed
  unit_factor <- if (vo2_unit == "mL/min") 1/1000 else 1

  # Calculate mean VE per subject (assuming ventilation data or estimate from VO2)
  # VE is typically ~20-25 * VO2 during moderate exercise
  calo_summary <- calo_df |>
    dplyr::group_by(.data[[calo_id_col]]) |>
    dplyr::summarise(
      VO2_mean = mean(.data[[vo2_col]], na.rm = TRUE) * unit_factor,
      VCO2_mean = mean(.data[[vco2_col]], na.rm = TRUE) * unit_factor,
      # Estimate VE if not available (typical VE/VO2 ratio ~25)
      VE_mean = VO2_mean * 25,
      .groups = "drop"
    )

  # Merge all data
  result <- subj_df |>
    dplyr::left_join(env_df, by = stats::setNames(env_id_col, id_col)) |>
    dplyr::left_join(calo_summary, by = stats::setNames(calo_id_col, id_col)) |>
    dplyr::mutate(
      # Calculate expired gas mass loss
      expired_O2_mass = .data$VO2_mean * exercise_duration / 22.4 * 32,
      expired_CO2_mass = .data$VCO2_mean * exercise_duration / 22.4 * 44,
      expired_gas_lost = .data$expired_CO2_mass - .data$expired_O2_mass,

      # Calculate water vapor loss
      # Saturated vapor pressure (Antoine equation approximation)
      pH2O_saturated = 5.53275 * 10^(0.02585 * .data[[temp_col]]),
      pH2O_ambient = .data$pH2O_saturated * (.data[[humidity_col]] / 100),

      # Water vapor in/out (assuming body temp 37C -> 47 mmHg)
      H2O_vapor_in = (.data$VE_mean * exercise_duration / .data[[pressure_col]]) * .data$pH2O_ambient,
      H2O_vapor_out = (.data$VE_mean * exercise_duration / .data[[pressure_col]]) * 47,

      # Convert to mass (corrected for temperature)
      H2O_mass_lost = (((273 * (((.data$H2O_vapor_out - .data$H2O_vapor_in) *
                                  .data[[pressure_col]]) / 760)) / (273 + 37)) / 22.4) * 18,

      # Mass balance
      # Note: 2000 mL is baseline water intake in original protocol
      body_mass_gain = .data[[initial_mass_col]] +
        (.data[[water_col]] + 2000 + .data[[saline_col]]) / 1000,
      mass_lost = (.data$expired_gas_lost + .data$H2O_mass_lost) / 1000,
      virtual_mass = .data$body_mass_gain - .data$mass_lost,
      sweat_loss = .data$virtual_mass - .data[[final_mass_col]]
    ) |>
    dplyr::select(dplyr::all_of(id_col), sweat_loss)

  tibble::as_tibble(result)
}

#' Calculate Urea Loss
#'
#' Calculate total urea nitrogen loss from sweat and urine.
#'
#' @param urea A UreaData object
#' @param sweat_loss A data frame with sweat loss (from calc_sweat_loss)
#' @param subjects A SubjectData object (for urine volume)
#'
#' @return A tibble with urea loss components
#' @export
calc_urea_loss <- function(urea, sweat_loss, subjects = NULL) {

  if (S7_inherits(urea, UreaData)) {
    urea_df <- urea@data
    id_col <- urea@id_col
    sweat_urea_col <- urea@sweat_urea_col
    urine_urea_col <- urea@urine_urea_col
    urine_vol_col <- urea@urine_vol_col
  } else {
    cli::cli_abort("urea must be a UreaData object")
  }

  # Get urine volume if available
  if (!is.null(subjects) && S7_inherits(subjects, SubjectData)) {
    if ("urine" %in% names(subjects@fluid_cols)) {
      urine_vol <- subjects@data |>
        dplyr::select(dplyr::all_of(c(subjects@id_col, "urine"))) |>
        dplyr::rename(urine_vol = "urine")

      urea_df <- urea_df |>
        dplyr::left_join(urine_vol, by = stats::setNames(subjects@id_col, id_col))
      urine_vol_col <- "urine_vol"
    }
  }

  # Calculate urea losses
  result <- urea_df |>
    dplyr::left_join(sweat_loss, by = id_col) |>
    dplyr::mutate(
      # Urea loss from urine (mmol)
      urine_urea_loss = .data[[urine_vol_col]] * .data[[urine_urea_col]] / 1000,
      # Urea loss from sweat (mmol)
      sweat_urea_loss = .data$sweat_loss * .data[[sweat_urea_col]],
      # Total urea loss (mmol)
      total_urea_loss = .data$sweat_urea_loss + .data$urine_urea_loss,
      # Convert to mass (g) - urea MW = 60 g/mol
      urea_mass_loss = .data$total_urea_loss / 1000 * 60
    ) |>
    dplyr::select(dplyr::all_of(id_col),
                  urine_urea_loss, sweat_urea_loss, total_urea_loss, urea_mass_loss)

  tibble::as_tibble(result)
}

#' Calculate Protein Oxidation from Urea
#'
#' Estimate protein oxidation rate from total urea nitrogen loss.
#' Uses the conversion factor of 2.915 (grams protein per gram urea).
#'
#' @param urea_loss A data frame from calc_urea_loss (or with urea_mass_loss column)
#' @param exercise_duration Exercise duration in minutes (default: 120)
#' @param conversion_factor Protein/urea conversion factor (default: 2.915)
#' @param id_col Name of the ID column
#'
#' @return A tibble with protein oxidation rates (g/min)
#' @export
#'
#' @details
#' The conversion factor of 2.915 is derived from the work of Telmosse (2022),
#' which states that 1 gram of urea excreted is equivalent to 0.343 grams of protein oxidized.
#' This value is based on the protein formula (`C89H148.5O32N23.5`) described in
#' Institute of Medicine (2005).
#'
#' - Nitrogen content of proteins (~16%)
#' - Nitrogen content of urea (46.6%)
#' - Complete oxidation of amino acids
#'
#' protein_ox = urea_mass * 2.915 / exercise_duration
calc_protein_oxidation <- function(urea_loss,
                                    exercise_duration = 120,
                                    conversion_factor = 2.915,
                                    id_col = "id") {

  if (!"urea_mass_loss" %in% names(urea_loss)) {
    cli::cli_abort("urea_loss must contain 'urea_mass_loss' column")
  }

  result <- urea_loss |>
    dplyr::mutate(
      # Total protein oxidation during exercise (g)
      total_protein_ox = .data$urea_mass_loss * conversion_factor,
      # Protein oxidation rate (g/min)
      protein_ox = .data$total_protein_ox / exercise_duration
    ) |>
    dplyr::select(dplyr::all_of(id_col), total_protein_ox, protein_ox)

  tibble::as_tibble(result)
}

#' Calculate Protein Contribution to Gas Exchange
#'
#' Calculate the VO2 and VCO2 attributable to protein oxidation.
#'
#' @param protein_ox Protein oxidation rate (g/min) - vector or data frame
#' @param vo2_factor VO2 per gram protein (default: 1.0075 L/g, derived from Institute of Medicine, 2005, as presented in Telmosse, 2022)
#' @param vco2_factor VCO2 per gram protein (default: 0.8443 L/g, derived from Institute of Medicine, 2005, as presented in Telmosse, 2022)
#'
#' @return A tibble with VO2p and VCO2p values
#' @export
calc_protein_gas_exchange <- function(protein_ox,
                                       vo2_factor = 1.0075,
                                       vco2_factor = 0.8443) {

  if (is.data.frame(protein_ox)) {
    if (!"protein_ox" %in% names(protein_ox)) {
      cli::cli_abort("protein_ox data frame must contain 'protein_ox' column")
    }
    pox <- protein_ox$protein_ox
  } else {
    pox <- protein_ox
  }

  tibble::tibble(
    vo2_protein = pox * vo2_factor,
    vco2_protein = pox * vco2_factor
  )
}

#' Estimate Protein Oxidation (Simple Method)
#'
#' Simple estimation of protein oxidation assuming a fixed contribution
#' to total energy expenditure (typically 5-15% during exercise).
#'
#' @param total_energy Total energy expenditure (kcal/min or kcal)
#' @param protein_percent Assumed protein contribution (default: 0.05 = 5%)
#' @param protein_factor Energy per gram protein (default: 4.70 kcal/g)
#'
#' @return Estimated protein oxidation (g/min or g)
#' @export
estimate_protein_simple <- function(total_energy,
                                     protein_percent = 0.05,
                                     protein_factor = 4.70) {
  (total_energy * protein_percent) / protein_factor
}
