#' Protein Oxidation Functions
#'
#' @name protein
#' @description Functions for calculating protein oxidation from urea losses
NULL

#' Calculate Sweat Loss
#'
#' Estimate sweat loss from a body-mass balance: body mass change, fluid
#' intake, urine output, and respiratory gas and water losses.
#'
#' @param subjects A SubjectData object with mass and fluid data
#' @param calo A CalorimetryData object for respiratory calculations
#' @param environment An EnvironmentData object for vapor pressure calculations
#' @param exercise_duration Exercise duration in minutes (default: 120)
#' @param baseline_water_ml Water intake in mL that is not in the water column
#'   of SubjectData, added to the fluid balance (default: 0).
#' @param ve_vo2_ratio Ventilation to VO2 ratio used to estimate minute
#'   ventilation when VE data is not available (default: 25). Typical range
#'   during moderate exercise is 20-30.
#'
#' @details
#' sweat (kg) = initial mass + (water + baseline water + saline) / 1000
#' - (expired CO2 mass - O2 mass taken up + respiratory water) / 1000
#' - urine / 1000 - final mass
#'
#' Urine is the volume (mL) passed between the two weighings, read from the
#' `urine` entry of `subjects@fluid_cols`. If that column is absent, urine is
#' taken as 0 mL. When SubjectData has a protocol column, the balance is
#' computed for each subject and protocol.
#'
#' @return A tibble with the ID column, the protocol column (if any) and
#'   `sweat_loss` (kg)
#' @export
calc_sweat_loss <- function(subjects,
                            calo,
                            environment,
                            exercise_duration = 120,
                            baseline_water_ml = 0,
                            ve_vo2_ratio = 25) {

  # Extract data from S7 objects or use directly
  if (S7_inherits(subjects, SubjectData)) {
    subj_df <- subjects@data
    id_col <- subjects@id_col
    initial_mass_col <- subjects@mass_cols$initial
    final_mass_col <- subjects@mass_cols$final
    water_col <- subjects@fluid_cols$water
    saline_col <- subjects@fluid_cols$saline
    urine_col <- subjects@fluid_cols$urine
    proto <- intersect(subjects@protocol_col, names(subj_df))
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

  has_urine <- !is.null(urine_col) && urine_col %in% names(subj_df)
  if (!has_urine) {
    cli::cli_alert_info("No urine column in subject data; urine output taken as 0 mL.")
  }

  # Convert VO2/VCO2 to L/min if needed
  unit_factor <- if (vo2_unit == "mL/min") 1 / 1000 else 1

  # Mean gas exchange per subject, and per protocol when calorimetry has it.
  # VE is estimated from VO2 (CalorimetryData holds no VE column).
  calo_proto <- intersect(proto, names(calo_df))
  calo_summary <- calo_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(calo_id_col, calo_proto)))) |>
    dplyr::summarise(
      VO2_mean = mean(.data[[vo2_col]], na.rm = TRUE) * unit_factor,
      VCO2_mean = mean(.data[[vco2_col]], na.rm = TRUE) * unit_factor,
      VE_mean = VO2_mean * ve_vo2_ratio,
      .groups = "drop"
    )

  # Merge all data on ID, plus protocol where both tables have it
  result <- subj_df |>
    dplyr::left_join(env_df, by = c(stats::setNames(env_id_col, id_col), intersect(proto, names(env_df))),
                     relationship = "many-to-one") |>
    dplyr::left_join(calo_summary, by = c(stats::setNames(calo_id_col, id_col), calo_proto),
                     relationship = "many-to-one")
  # Keep the subject protocol column (a join can change its type)
  result[proto] <- subj_df[proto]
  result <- result |>
    dplyr::mutate(
      sweat_loss = sweat_balance(
        vo2 = .data$VO2_mean,
        vco2 = .data$VCO2_mean,
        ve = .data$VE_mean,
        temperature = .data[[temp_col]],
        humidity = .data[[humidity_col]],
        pressure = .data[[pressure_col]],
        initial_mass = .data[[initial_mass_col]],
        final_mass = .data[[final_mass_col]],
        water = .data[[water_col]],
        saline = .data[[saline_col]],
        urine = if (has_urine) .data[[urine_col]] else 0,
        exercise_duration = exercise_duration,
        baseline_water_ml = baseline_water_ml
      )
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, proto)), "sweat_loss")

  tibble::as_tibble(result)
}

# Sweat loss (kg) from a body-mass balance. Shared by calc_sweat_loss() and
# tar_oxidation_analysis(). Gas exchange in L/min, fluids in mL, masses in kg,
# temperature in degrees C, relative humidity in %, pressure in mmHg.
sweat_balance <- function(vo2, vco2, ve, temperature, humidity, pressure,
                          initial_mass, final_mass, water, saline, urine,
                          exercise_duration, baseline_water_ml = 0) {
  # Expired CO2 mass minus O2 mass taken up (g), 22.4 L/mol
  gas_lost <- (vco2 * 44 - vo2 * 32) * exercise_duration / 22.4
  # Respiratory water (g): saturated vapor pressure of ambient air (Antoine
  # approximation) against expired air saturated at 37 degrees C (47 mmHg)
  ph2o_ambient <- 5.53275 * 10^(0.02585 * temperature) * humidity / 100
  vapor_l <- ve * exercise_duration / pressure * (47 - ph2o_ambient)
  h2o_lost <- 273 * vapor_l * pressure / 760 / (273 + 37) / 22.4 * 18
  intake <- (water + baseline_water_ml + saline) / 1000
  initial_mass + intake - (gas_lost + h2o_lost + urine) / 1000 - final_mass
}

#' Calculate Urea Loss
#'
#' Calculate total urea nitrogen loss from sweat and urine.
#'
#' @param urea A UreaData object
#' @param sweat_loss A data frame with sweat loss (from calc_sweat_loss)
#' @param subjects A SubjectData object (for urine volume)
#'
#' @return A tibble with the key columns of `sweat_loss` (ID and, if any,
#'   protocol) and the urea loss components
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

  # Get urine volume (per protocol, if any) from the subject data
  if (!is.null(subjects) && S7_inherits(subjects, SubjectData)) {
    urine_col <- subjects@fluid_cols$urine
    if (!is.null(urine_col) && urine_col %in% names(subjects@data)) {
      proto <- intersect(subjects@protocol_col, names(subjects@data))
      urine_vol <- subjects@data |>
        dplyr::select(dplyr::all_of(c(subjects@id_col, proto, urine_col))) |>
        dplyr::rename(urine_vol = dplyr::all_of(urine_col))

      # The subject urine volume replaces any urine_vol column of the urea data
      urea_df <- urea_df |>
        dplyr::select(-dplyr::any_of("urine_vol")) |>
        dplyr::left_join(
          urine_vol,
          by = c(stats::setNames(subjects@id_col, id_col), intersect(proto, names(urea_df)))
        )
      urine_vol_col <- "urine_vol"
    }
  }

  # Join sweat loss on ID, plus protocol where both tables have it
  sweat_id <- if (!is.null(subjects) && S7_inherits(subjects, SubjectData)) subjects@id_col else id_col
  if (!sweat_id %in% names(sweat_loss)) {
    cli::cli_abort("sweat_loss must contain the ID column {.val {sweat_id}}.")
  }
  keys <- setdiff(names(sweat_loss), c(sweat_id, "sweat_loss"))
  result <- urea_df |>
    dplyr::left_join(
      sweat_loss,
      by = c(stats::setNames(sweat_id, id_col), intersect(keys, names(urea_df)))
    ) |>
    dplyr::mutate(
      # Urea loss from urine (mmol)
      urine_urea_loss = .data[[urine_vol_col]] * .data[[urine_urea_col]] / 1000,
      # Urea loss from sweat (mmol)
      sweat_urea_loss = .data$sweat_loss * .data[[sweat_urea_col]],
      # Total urea loss (mmol)
      total_urea_loss = .data$sweat_urea_loss + .data$urine_urea_loss,
      # Convert to mass (g), with the molar mass of urea (60.056 g/mol)
      urea_mass_loss = .data$total_urea_loss / 1000 * urea_molar_mass
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, keys)),
                  urine_urea_loss, sweat_urea_loss, total_urea_loss, urea_mass_loss)

  tibble::as_tibble(result)
}

#' Calculate Protein Oxidation from Urea
#'
#' Estimate protein oxidation rate from total urea loss.
#'
#' @param urea_loss A data frame from calc_urea_loss (or with urea_mass_loss column)
#' @param exercise_duration Exercise duration in minutes (default: 120)
#' @param conversion_factor Grams of protein oxidized per gram of urea excreted
#'   (default: `stoich_coefficients$protein_urea`, 2.9154)
#' @param id_col Name of the ID column
#' @param protocol_col Name of the protocol column to keep in the output
#'   (optional). Keep it when protein oxidation differs between protocols.
#'
#' @return A tibble with protein oxidation rates (g/min)
#' @export
#'
#' @details
#' protein_ox = urea_mass * conversion_factor / exercise_duration
#'
#' The default factor assumes that protein contains 16% N, that is 6.25 g
#' protein per g N (Livesey & Elia, 1988, p. 620), and that all of this N
#' leaves as urea, which is 46.646% N: 6.25 x 0.46646 = 2.9154 g protein per g
#' urea. Telmosse (2022, p. 22) obtains 2.915 from the protein-type
#' C89H148.5O32N23.5: the oxidation of 2057.5 g of protein gives 705 g of urea,
#' that is 0.343 g of urea per g of protein (1 / 0.343 = 2.915).
calc_protein_oxidation <- function(urea_loss,
                                   exercise_duration = 120,
                                   conversion_factor = stoich_coefficients$protein_urea,
                                   id_col = "id",
                                   protocol_col = NULL) {

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
    dplyr::select(dplyr::all_of(c(id_col, intersect(protocol_col, names(urea_loss)))),
                  total_protein_ox, protein_ox)

  tibble::as_tibble(result)
}

#' Calculate Protein Contribution to Gas Exchange
#'
#' Calculate the VO2 and VCO2 attributable to protein oxidation.
#'
#' @param protein_ox Protein oxidation rate (g/min) - vector or data frame
#' @param vo2_factor L O2 used per g protein (default:
#'   `stoich_coefficients$protein_vo2`, 1.0103 L/g: 19.68 kJ/g divided by
#'   19.48 kJ/L O2, Livesey & Elia, 1988, Table 6)
#' @param vco2_factor L CO2 produced per g protein (default:
#'   `stoich_coefficients$protein_vco2`, 0.8436 L/g: vo2_factor times RQ 0.835)
#'
#' @return A tibble with VO2p and VCO2p values
#' @export
calc_protein_gas_exchange <- function(protein_ox,
                                      vo2_factor = stoich_coefficients$protein_vo2,
                                      vco2_factor = stoich_coefficients$protein_vco2) {

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
#' @param protein_factor Energy per gram protein (default:
#'   `energy_factors$protein`, 4.70 kcal/g)
#'
#' @return Estimated protein oxidation (g/min or g)
#' @export
estimate_protein_simple <- function(total_energy,
                                    protein_percent = 0.05,
                                    protein_factor = energy_factors$protein) {
  (total_energy * protein_percent) / protein_factor
}
