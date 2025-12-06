#' Targets Pipeline Helper Functions for Substrate Oxidation Analysis
#'
#' These functions provide targets-compatible wrappers for common oxidation
#' analysis workflows, enabling reproducible pipeline management.
#'
#' @name targets-helpers
#' @keywords internal
NULL

#' Import and Clean Oxidation Study Data
#'
#' Loads raw data files and performs cleaning for oxidation analysis.
#' Returns a list containing S7 objects and cleaned data frames suitable
#' for use as a targets pipeline target.
#'
#' @param data_dir Path to directory containing raw .RData files
#' @param excluded_subjects Character vector of subject IDs to exclude
#' @param excluded_protocols Character vector of protocol names to exclude
#' @param protocol_labels Named character vector mapping protocol codes to labels
#'
#' @return A list containing:
#' \describe{
#'   \item{calo_s7}{CalorimetryData S7 object}
#'   \item{iso_s7}{IsotopeData S7 object}
#'   \item{calo_df}{Cleaned calorimetry data frame}
#'   \item{subjects_clean}{Cleaned subject characteristics}
#'   \item{prot_data}{Data for protein oxidation calculation}
#'   \item{Rexo_clean, Rref_clean, Rpla_clean, Rexp_clean}{Cleaned isotope data}
#'   \item{subject_summary}{Summary statistics by protocol}
#'   \item{config}{Configuration parameters used}
#' }
#'
#' @export
tar_import_data <- function(data_dir = "data",
                            excluded_subjects = NULL,
                            excluded_protocols = NULL,
                            protocol_labels = NULL) {

  # Default protocol labels if not provided
  if (is.null(protocol_labels)) {
    protocol_labels <- c(
      "A" = "Water",
      "B" = "Trace",
      "C" = "Glucose",
      "D" = "Sports Drink",
      "E" = "Maple Water",
      "F" = "Maple Syrup",
      "G" = "Corn Syrup"
    )
  }

  # Load raw data files
  load(file.path(data_dir, "A_calo.RData"))
  load(file.path(data_dir, "A_daily.RData"))
  load(file.path(data_dir, "A_geotop.RData"))
  load(file.path(data_dir, "A_urea.RData"))
  load(file.path(data_dir, "A_Rexo.RData"))
  load(file.path(data_dir, "A_Rref.Rdata"))
  load(file.path(data_dir, "A_Rpla.RData"))
  load(file.path(data_dir, "A_age_height.RData"))
  load(file.path(data_dir, "A_max.RData"))
  load(file.path(data_dir, "A_OGTT.RData"))

  # Clean geotop (13CO2 expired) data
  gt_clean <- gt |>
    dplyr::mutate(
      protocol = as.character(protocol),
      protocol = dplyr::if_else(protocol == "Maple Sap", "Maple Water", protocol),
      protocol = factor(protocol),
      time = as.numeric(as.character(time))
    )

  # Daily groups mapping
  daily_groups <- daily |>
    dplyr::select(ID, name, protocol) |>
    dplyr::mutate(
      protocol = forcats::fct_recode(protocol, !!!protocol_labels),
      ID = as.character(ID),
      name = stringr::str_trim(name)
    )

  # Clean calorimetry data
  calo_df <- calo_data |>
    dplyr::mutate(name = stringr::str_trim(ID)) |>
    dplyr::left_join(daily_groups, by = c("ID" = "name")) |>
    dplyr::select(-ID) |>
    dplyr::rename(id = ID.y) |>
    dplyr::mutate(
      id = as.character(id),
      time = as.numeric(as.character(time)),
      vo2 = VO2 / 1000,
      vco2 = VCO2 / 1000,
      ve = Bf * Vt / 1000
    ) |>
    dplyr::group_by(id, protocol, time) |>
    dplyr::summarise(
      vo2 = mean(vo2, na.rm = TRUE),
      vco2 = mean(vco2, na.rm = TRUE),
      ve = mean(ve, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!id %in% as.character(excluded_subjects)) |>
    dplyr::filter(!protocol %in% excluded_protocols) |>
    dplyr::mutate(rer = vco2 / vo2)

  # Create CalorimetryData S7 object
  calo_s7 <- CalorimetryData(
    data = as.data.frame(calo_df),
    id_col = "id",
    time_col = "time",
    vo2_col = "vo2",
    vco2_col = "vco2",
    vo2_unit = "L/min",
    protocol_col = "protocol"
  )

  # Clean subject characteristics
  subjects_clean <- A_VO2max |>
    dplyr::ungroup() |>
    dplyr::select(ID, mass_kg, VO2max, PAM) |>
    unique() |>
    dplyr::inner_join(
      dplyr::left_join(daily, age_height, by = "name") |>
        dplyr::select(name, ID:date, initial_body_mass:initial_body_fat, mass_kg:b_date) |>
        dplyr::mutate(
          age = lubridate::interval(start = b_date, end = date) /
            lubridate::duration(num = 1, units = "years")
        ) |>
        dplyr::select(name, ID:protocol, initial_body_mass:initial_body_fat, height, age),
      by = c("ID" = "name")
    ) |>
    dplyr::rename(name = ID, id = ID.y) |>
    dplyr::mutate(
      id = as.character(id),
      protocol = forcats::fct_recode(protocol, !!!protocol_labels),
      VO2max_kg = VO2max / mass_kg
    ) |>
    dplyr::left_join(
      ogtt |>
        dplyr::mutate(
          ID = as.character(ID),
          ID = replace(ID, ID == "362", "365")
        ) |>
        dplyr::select(ID, pre, `120_min`),
      by = c("id" = "ID")
    ) |>
    dplyr::filter(!id %in% as.character(excluded_subjects)) |>
    dplyr::filter(!protocol %in% excluded_protocols)

  # Clean urea data
  urea_clean <- urea |>
    dplyr::select(ID, urea, sample) |>
    tidyr::pivot_wider(names_from = sample, values_from = urea) |>
    dplyr::rename(
      id = ID,
      urine_urea = urine,
      sweat_urea = sweat
    ) |>
    dplyr::mutate(id = as.character(id)) |>
    tidyr::drop_na()

  # Calorimetry summary for protein oxidation
  calo_summary <- calo_df |>
    dplyr::filter(time > 0) |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      vo2_mean = mean(vo2, na.rm = TRUE),
      vco2_mean = mean(vco2, na.rm = TRUE),
      ve_mean = mean(ve, na.rm = TRUE),
      .groups = "drop"
    )

  # Data for protein oxidation calculation
  prot_data <- daily |>
    dplyr::select(ID, protocol, initial_body_mass, final_body_mass, water_ingested,
                  saline, urine, p_atm, temperature, humidity) |>
    dplyr::mutate(
      id = as.character(ID),
      protocol = forcats::fct_recode(protocol, !!!protocol_labels)
    ) |>
    dplyr::select(-ID) |>
    dplyr::left_join(urea_clean, by = "id") |>
    dplyr::left_join(calo_summary, by = "id") |>
    tidyr::drop_na() |>
    dplyr::filter(!id %in% as.character(excluded_subjects)) |>
    dplyr::filter(!protocol %in% excluded_protocols)

  # Clean isotope data
  Rexo_clean <- Rexo |>
    dplyr::mutate(
      protocol = as.character(protocol),
      Rexo = dplyr::if_else(protocol == "Glucose", 75.13, Rexo),
      rexo = Rexo
    ) |>
    dplyr::filter(!protocol %in% excluded_protocols)

  Rref_clean <- Rref |>
    dplyr::mutate(
      time = as.numeric(as.character(time)),
      rref = Rref
    ) |>
    dplyr::select(time, rref)

  Rpla_clean <- Rpla |>
    dplyr::mutate(
      id = as.character(ID),
      protocol = as.character(protocol),
      time = as.numeric(as.character(time)),
      rpla = Rpla
    ) |>
    dplyr::filter(!id %in% as.character(excluded_subjects)) |>
    dplyr::filter(!protocol %in% excluded_protocols)

  Rexp_clean <- gt_clean |>
    dplyr::mutate(
      id = as.character(ID),
      protocol = as.character(protocol),
      rexp = Rexp
    ) |>
    dplyr::filter(!id %in% as.character(excluded_subjects)) |>
    dplyr::filter(!protocol %in% excluded_protocols) |>
    dplyr::select(id, protocol, time, rexp)

  # Create IsotopeData S7 object
  iso_s7 <- IsotopeData(
    rexp = as.data.frame(Rexp_clean),
    rexo = as.data.frame(Rexo_clean |> dplyr::select(protocol, rexo)),
    rref = as.data.frame(Rref_clean),
    rpla = as.data.frame(Rpla_clean |> dplyr::select(id, protocol, time, rpla)),
    id_col = "id",
    time_col = "time",
    protocol_col = "protocol",
    rexp_col = "rexp",
    rexo_col = "rexo",
    rref_col = "rref",
    rpla_col = "rpla"
  )

  # Subject summary by protocol
  subject_summary <- subjects_clean |>
    dplyr::group_by(protocol) |>
    dplyr::summarise(
      n = dplyr::n(),
      age_mean = mean(age, na.rm = TRUE),
      age_sd = sd(age, na.rm = TRUE),
      mass_mean = mean(mass_kg, na.rm = TRUE),
      mass_sd = sd(mass_kg, na.rm = TRUE),
      height_mean = mean(height / 100, na.rm = TRUE),
      height_sd = sd(height / 100, na.rm = TRUE),
      VO2max_mean = mean(VO2max / 1000, na.rm = TRUE),
      VO2max_sd = sd(VO2max / 1000, na.rm = TRUE),
      VO2max_kg_mean = mean(VO2max_kg, na.rm = TRUE),
      VO2max_kg_sd = sd(VO2max_kg, na.rm = TRUE),
      PAM_mean = mean(PAM, na.rm = TRUE),
      PAM_sd = sd(PAM, na.rm = TRUE),
      .groups = "drop"
    )

  # Return list of all cleaned data
  list(
    calo_s7 = calo_s7,
    iso_s7 = iso_s7,
    calo_df = calo_df,
    subjects_clean = subjects_clean,
    prot_data = prot_data,
    urea_clean = urea_clean,
    gt_clean = gt_clean,
    Rexo_clean = Rexo_clean,
    Rref_clean = Rref_clean,
    Rpla_clean = Rpla_clean,
    Rexp_clean = Rexp_clean,
    subject_summary = subject_summary,
    config = list(
      excluded_subjects = excluded_subjects,
      excluded_protocols = excluded_protocols,
      protocol_labels = protocol_labels
    )
  )
}


#' Calculate Substrate Oxidation and Energy Contributions
#'
#' Performs oxidation analysis including protein oxidation (mass balance),
#' substrate oxidation (Jequier equations), CHO partitioning (isotope methods),
#' and energy contributions.
#'
#' @param cleaned_data Output from \code{tar_import_data()}
#' @param exercise_duration Duration of exercise in minutes (default: 120)
#'
#' @return A list containing:
#' \describe{
#'   \item{oxidation}{Complete oxidation data with CHO partitioning}
#'   \item{oxidation_last_hour}{Filtered to last hour (time > 30)}
#'   \item{energy}{Energy yield calculations}
#'   \item{protein_oxidation}{Protein oxidation rates}
#'   \item{cho_exo}{Exogenous CHO oxidation data}
#'   \item{cho_pla}{Plasma glucose oxidation data}
#'   \item{oxidation_summary}{Summary by protocol}
#'   \item{timecourse_summary}{Time course summary}
#'   \item{oxidation_timecourse}{Oxidation rates over time}
#'   \item{energy_contribution_summary}{Energy percentages by protocol}
#'   \item{rexp_summary}{Expired 13CO2 summary}
#'   \item{rpla_summary}{Plasma 13C-glucose summary}
#' }
#'
#' @export
tar_oxidation_analysis <- function(cleaned_data, exercise_duration = 120) {

  # Extract data from cleaned_data list
  calo_s7 <- cleaned_data$calo_s7
  calo_df <- cleaned_data$calo_df
  prot_data <- cleaned_data$prot_data
  Rexp_clean <- cleaned_data$Rexp_clean
  Rref_clean <- cleaned_data$Rref_clean
  Rexo_clean <- cleaned_data$Rexo_clean
  Rpla_clean <- cleaned_data$Rpla_clean

  # Calculate protein oxidation from urea (mass balance approach)
  protein_oxidation <- prot_data |>
    dplyr::mutate(
      urine_urea_loss = urine * urine_urea / 1000,
      expired_O2_mass = vo2_mean * exercise_duration / 22.4 * 32,
      expired_CO2_mass = vco2_mean * exercise_duration / 22.4 * 44,
      expired_gas_lost = expired_CO2_mass - expired_O2_mass,
      pH2O_saturated = 5.53275 * 10^(0.02585 * temperature),
      pH2O_ambient = pH2O_saturated * (humidity / 100),
      H2O_vapor_in = (ve_mean * exercise_duration / p_atm) * pH2O_ambient,
      H2O_vapor_out = (ve_mean * exercise_duration / p_atm) * 47,
      H2O_mass_lost = (((273 * (((H2O_vapor_out - H2O_vapor_in) * p_atm) / 760)) /
                          (273 + 37)) / 22.4) * 18,
      body_mass_gain = initial_body_mass + (water_ingested + 2000 + saline) / 1000,
      mass_lost = (expired_gas_lost + H2O_mass_lost) / 1000,
      virtual_mass = body_mass_gain - mass_lost,
      sweat_loss = virtual_mass - final_body_mass,
      sweat_urea_loss = sweat_loss * sweat_urea,
      total_urea_loss = sweat_urea_loss + urine_urea_loss,
      urea_mass_loss = total_urea_loss / 1000 * 60,
      total_protein_ox = urea_mass_loss * 2.9,
      protein_ox = total_protein_ox / exercise_duration
    ) |>
    dplyr::select(id, protein_ox, total_protein_ox)

  # Calculate substrate oxidation using oxidizr
  substrate_ox <- calc_substrate_oxidation(
    calo = calo_s7,
    protein_ox = protein_oxidation
  )

  # Filter to exercise period
  substrate_ox_exercise <- substrate_ox |>
    dplyr::filter(time > 0)

  # Calculate exogenous CHO
  cho_exo <- calo_df |>
    dplyr::filter(time > 0) |>
    dplyr::select(id, time, protocol, vco2) |>
    dplyr::left_join(Rexp_clean, by = c("id", "protocol", "time")) |>
    dplyr::left_join(Rref_clean, by = "time") |>
    dplyr::left_join(Rexo_clean |> dplyr::select(protocol, rexo), by = "protocol") |>
    dplyr::mutate(
      cho_exo = vco2 * ((rexp - rref) / (rexo - rref)) / stoich_coefficients$co2_cho_ratio,
      cho_exo = pmax(cho_exo, 0, na.rm = TRUE)
    ) |>
    dplyr::select(id, time, protocol, cho_exo, rexp, rexo, rref)

  # Calculate plasma CHO
  cho_pla <- calo_df |>
    dplyr::filter(time > 0) |>
    dplyr::select(id, time, protocol, vco2) |>
    dplyr::left_join(Rexp_clean, by = c("id", "protocol", "time")) |>
    dplyr::left_join(Rref_clean, by = "time") |>
    dplyr::left_join(Rpla_clean |> dplyr::select(id, protocol, time, rpla),
                     by = c("id", "protocol", "time")) |>
    dplyr::left_join(Rexo_clean |> dplyr::select(protocol, rexo), by = "protocol") |>
    dplyr::mutate(
      cho_pla = vco2 * ((rexp - rref) / (rpla - rref)) / stoich_coefficients$co2_cho_ratio,
      cho_pla = pmax(cho_pla, 0, na.rm = TRUE),
      fexo = ((rpla - rref) / (rexo - rref)) * 100
    ) |>
    dplyr::select(id, time, protocol, cho_pla, rpla, fexo)

  # Combine and calculate CHO partitioning
  oxidation <- substrate_ox_exercise |>
    dplyr::left_join(cho_exo |> dplyr::select(id, time, protocol, cho_exo, rexp, rexo, rref),
                     by = c("id", "time", "protocol")) |>
    dplyr::left_join(cho_pla |> dplyr::select(id, time, protocol, cho_pla, rpla, fexo),
                     by = c("id", "time", "protocol")) |>
    dplyr::mutate(
      cho_endo = cho_total - cho_exo,
      cho_mus = cho_total - cho_pla,
      cho_liv = cho_pla - cho_exo,
      cho_endo = pmax(cho_endo, 0, na.rm = TRUE),
      cho_mus = pmax(cho_mus, 0, na.rm = TRUE),
      cho_liv = pmax(cho_liv, 0, na.rm = TRUE)
    )

  # Calculate energy contributions
  energy <- calc_energy_yield(oxidation) |>
    calc_energy_percent()

  # Filter for last hour

  oxidation_last_hour <- energy |>
    dplyr::filter(time > 30)

  # Time course summary
  timecourse_summary <- calo_df |>
    dplyr::group_by(protocol, time) |>
    dplyr::summarise(
      vo2_mean = mean(vo2, na.rm = TRUE),
      vo2_sd = sd(vo2, na.rm = TRUE),
      vco2_mean = mean(vco2, na.rm = TRUE),
      vco2_sd = sd(vco2, na.rm = TRUE),
      rer_mean = mean(rer, na.rm = TRUE),
      rer_sd = sd(rer, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Oxidation summary by protocol
  oxidation_summary <- summarize_energy(oxidation_last_hour, by = "protocol")

  # Oxidation time course
  oxidation_timecourse <- energy |>
    dplyr::group_by(protocol, time) |>
    dplyr::summarise(
      dplyr::across(
        c(cho_total, cho_exo, cho_endo, fat_total, protein_ox,
          pct_cho_total, pct_cho_exo, pct_cho_endo, pct_fat, pct_protein),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))
      ),
      n = dplyr::n(),
      .groups = "drop"
    )

  # 13CO2 enrichment summary
  rexp_summary <- Rexp_clean |>
    dplyr::group_by(protocol, time) |>
    dplyr::summarise(
      rexp_mean = mean(rexp, na.rm = TRUE),
      rexp_sd = sd(rexp, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Plasma 13C-glucose summary
  rpla_summary <- Rpla_clean |>
    dplyr::group_by(protocol, time) |>
    dplyr::summarise(
      rpla_mean = mean(rpla, na.rm = TRUE),
      rpla_sd = sd(rpla, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Energy contribution summary
  energy_contribution_summary <- oxidation_last_hour |>
    dplyr::group_by(protocol) |>
    dplyr::summarise(
      pct_cho_total_mean = mean(pct_cho_total, na.rm = TRUE),
      pct_cho_total_sd = sd(pct_cho_total, na.rm = TRUE),
      pct_cho_exo_mean = mean(pct_cho_exo, na.rm = TRUE),
      pct_cho_exo_sd = sd(pct_cho_exo, na.rm = TRUE),
      pct_cho_endo_mean = mean(pct_cho_endo, na.rm = TRUE),
      pct_cho_endo_sd = sd(pct_cho_endo, na.rm = TRUE),
      pct_fat_mean = mean(pct_fat, na.rm = TRUE),
      pct_fat_sd = sd(pct_fat, na.rm = TRUE),
      pct_protein_mean = mean(pct_protein, na.rm = TRUE),
      pct_protein_sd = sd(pct_protein, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  list(
    oxidation = oxidation,
    oxidation_last_hour = oxidation_last_hour,
    energy = energy,
    protein_oxidation = protein_oxidation,
    cho_exo = cho_exo,
    cho_pla = cho_pla,
    oxidation_summary = oxidation_summary,
    timecourse_summary = timecourse_summary,
    oxidation_timecourse = oxidation_timecourse,
    energy_contribution_summary = energy_contribution_summary,
    rexp_summary = rexp_summary,
    rpla_summary = rpla_summary,
    exercise_duration = exercise_duration
  )
}


#' Run Statistical Analysis on Oxidation Data
#'
#' Performs mixed ANOVAs on subject characteristics and oxidation outcomes.
#'
#' @param cleaned_data Output from \code{tar_import_data()}
#' @param oxidation_results Output from \code{tar_oxidation_analysis()}
#'
#' @return A list containing ANOVA results and summary tables
#'
#' @export
tar_statistical_analysis <- function(cleaned_data, oxidation_results) {

  subjects_clean <- cleaned_data$subjects_clean
  calo_df <- cleaned_data$calo_df
  oxidation_last_hour <- oxidation_results$oxidation_last_hour

  # Helper function to extract ANOVA results
  extract_anova_results <- function(aov_obj, name) {
    anova_table <- aov_obj$anova_table
    tibble::tibble(
      outcome = name,
      effect = rownames(anova_table),
      df_num = anova_table$`num Df`,
      df_den = anova_table$`den Df`,
      F_value = anova_table$F,
      p_value = anova_table$`Pr(>F)`,
      ges = anova_table$ges
    )
  }

  # Subject characteristics ANOVAs
  aov_age <- afex::aov_ez("id", "age", data = subjects_clean, between = "protocol")
  aov_mass <- afex::aov_ez("id", "initial_body_mass", data = subjects_clean, between = "protocol")
  aov_vo2max <- afex::aov_ez("id", "VO2max_kg", data = subjects_clean, between = "protocol")

  subject_stats <- list(
    age = list(anova = aov_age, summary = summary(aov_age)),
    mass = list(anova = aov_mass, summary = summary(aov_mass),
                posthoc = pairs(emmeans::emmeans(aov_mass, "protocol"))),
    vo2max = list(anova = aov_vo2max, summary = summary(aov_vo2max))
  )

  # VO2 mixed ANOVA
  vo2_data <- calo_df |>
    dplyr::filter(time > 0) |>
    dplyr::mutate(time = factor(time))

  aov_vo2 <- afex::aov_ez("id", "vo2", data = vo2_data, between = "protocol", within = "time")

  vo2_stats <- list(
    anova = aov_vo2,
    summary = summary(aov_vo2),
    emm_time = emmeans::emmeans(aov_vo2, "time"),
    posthoc_time = pairs(emmeans::emmeans(aov_vo2, "time"))
  )

  # Oxidation ANOVAs
  ox_stats_data <- oxidation_last_hour |>
    dplyr::mutate(time = factor(time))

  # Total CHO
  aov_cho_total <- afex::aov_ez("id", "pct_cho_total", data = ox_stats_data,
                                between = "protocol", within = "time")
  cho_total_stats <- list(
    anova = aov_cho_total,
    summary = summary(aov_cho_total),
    emm_protocol = emmeans::emmeans(aov_cho_total, "protocol"),
    posthoc_protocol = pairs(emmeans::emmeans(aov_cho_total, "protocol")),
    emm_time = emmeans::emmeans(aov_cho_total, "time"),
    posthoc_time = pairs(emmeans::emmeans(aov_cho_total, "time"))
  )

  # Fat
  aov_fat <- afex::aov_ez("id", "pct_fat", data = ox_stats_data,
                          between = "protocol", within = "time")
  fat_stats <- list(
    anova = aov_fat,
    summary = summary(aov_fat),
    emm_protocol = emmeans::emmeans(aov_fat, "protocol"),
    posthoc_protocol = pairs(emmeans::emmeans(aov_fat, "protocol"))
  )

  # Protein
  aov_protein <- afex::aov_ez("id", "pct_protein", data = ox_stats_data,
                              between = "protocol", within = "time")
  protein_stats <- list(
    anova = aov_protein,
    summary = summary(aov_protein)
  )

  # Endogenous CHO
  aov_cho_endo <- afex::aov_ez("id", "pct_cho_endo", data = ox_stats_data,
                               between = "protocol", within = "time")
  cho_endo_stats <- list(
    anova = aov_cho_endo,
    summary = summary(aov_cho_endo)
  )

  # Exogenous CHO
  aov_cho_exo <- afex::aov_ez("id", "pct_cho_exo", data = ox_stats_data,
                              between = "protocol", within = "time")
  cho_exo_stats <- list(
    anova = aov_cho_exo,
    summary = summary(aov_cho_exo),
    emm_protocol = emmeans::emmeans(aov_cho_exo, "protocol"),
    posthoc_protocol = pairs(emmeans::emmeans(aov_cho_exo, "protocol"))
  )

  # Muscle and Liver CHO (with problematic observations filtered)
  ox_stats_mus <- ox_stats_data |>
    dplyr::filter(!(id == "332" & time == "60")) |>
    dplyr::filter(!(id == "233" & time == "90"))

  aov_cho_mus <- afex::aov_ez("id", "pct_cho_mus", data = ox_stats_mus,
                              between = "protocol", within = "time")
  cho_mus_stats <- list(anova = aov_cho_mus, summary = summary(aov_cho_mus))

  aov_cho_liv <- afex::aov_ez("id", "pct_cho_liv", data = ox_stats_mus,
                              between = "protocol", within = "time")
  cho_liv_stats <- list(anova = aov_cho_liv, summary = summary(aov_cho_liv))

  # Summary tables
  anova_summary <- dplyr::bind_rows(
    extract_anova_results(aov_cho_total, "% CHO total"),
    extract_anova_results(aov_fat, "% Fat"),
    extract_anova_results(aov_protein, "% Protein"),
    extract_anova_results(aov_cho_endo, "% CHO endogenous"),
    extract_anova_results(aov_cho_exo, "% CHO exogenous"),
    extract_anova_results(aov_cho_mus, "% CHO muscle"),
    extract_anova_results(aov_cho_liv, "% CHO liver")
  )

  posthoc_cho_protocol <- tibble::as_tibble(cho_total_stats$posthoc_protocol) |>
    dplyr::mutate(outcome = "% CHO total")
  posthoc_fat_protocol <- tibble::as_tibble(fat_stats$posthoc_protocol) |>
    dplyr::mutate(outcome = "% Fat")
  posthoc_cho_exo_protocol <- tibble::as_tibble(cho_exo_stats$posthoc_protocol) |>
    dplyr::mutate(outcome = "% CHO exogenous")

  posthoc_summary <- dplyr::bind_rows(
    posthoc_cho_protocol,
    posthoc_fat_protocol,
    posthoc_cho_exo_protocol
  )

  list(
    subject_stats = subject_stats,
    vo2_stats = vo2_stats,
    cho_total_stats = cho_total_stats,
    fat_stats = fat_stats,
    protein_stats = protein_stats,
    cho_endo_stats = cho_endo_stats,
    cho_exo_stats = cho_exo_stats,
    cho_mus_stats = cho_mus_stats,
    cho_liv_stats = cho_liv_stats,
    anova_summary = anova_summary,
    posthoc_summary = posthoc_summary
  )
}


#' Generate Publication Figures
#'
#' Creates ggplot2 figures for oxidation study results.
#'
#' @param cleaned_data Output from \code{tar_import_data()}
#' @param oxidation_results Output from \code{tar_oxidation_analysis()}
#' @param output_dir Directory to save figure files (default: "plots")
#'
#' @return A list containing ggplot objects and styling parameters
#'
#' @export
tar_generate_figures <- function(cleaned_data, oxidation_results, output_dir = "plots") {

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)

  # Extract data
  timecourse_summary <- oxidation_results$timecourse_summary
  oxidation_timecourse <- oxidation_results$oxidation_timecourse
  energy_contribution_summary <- oxidation_results$energy_contribution_summary
  rexp_summary <- oxidation_results$rexp_summary
  rpla_summary <- oxidation_results$rpla_summary

  # Color palette
  protocol_colors <- c(
    "Water" = "black",
    "Glucose" = "red",
    "Sports Drink" = "darkgreen",
    "Maple Water" = "blue",
    "Maple Syrup" = "orange"
  )

  protocol_order <- c("Water", "Glucose", "Sports Drink", "Maple Water", "Maple Syrup")

  # Figure 1: VO2 Time Course
  fig_vo2 <- timecourse_summary |>
    dplyr::mutate(protocol = factor(protocol, levels = protocol_order)) |>
    ggplot2::ggplot(ggplot2::aes(time, vo2_mean, color = protocol, group = protocol)) +
    ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = vo2_mean - vo2_sd, ymax = vo2_mean + vo2_sd),
      width = 5, linewidth = 0.8, position = ggplot2::position_dodge(width = 5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 1, alpha = 0.5) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::scale_x_continuous(breaks = seq(-30, 120, 30)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
    ggplot2::scale_color_manual(name = "Ingestion", values = protocol_colors) +
    ggplot2::labs(
      x = "Time (min)",
      y = expression(bold(paste(dot(V), O[2], " (L\u00b7min"^{-1}, ")")))
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  # Figure 2: RER Time Course
  fig_rer <- timecourse_summary |>
    dplyr::mutate(protocol = factor(protocol, levels = protocol_order)) |>
    ggplot2::ggplot(ggplot2::aes(time, rer_mean, color = protocol, group = protocol)) +
    ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = rer_mean - rer_sd, ymax = rer_mean + rer_sd),
      width = 5, linewidth = 0.8, position = ggplot2::position_dodge(width = 5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 1, alpha = 0.5) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::scale_x_continuous(breaks = seq(-30, 120, 30)) +
    ggplot2::scale_color_manual(name = "Ingestion", values = protocol_colors) +
    ggplot2::labs(x = "Time (min)", y = expression(bold("RER"))) +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  # Figure 3: 13CO2 Enrichment
  fig_rexp <- rexp_summary |>
    dplyr::mutate(protocol = factor(protocol, levels = protocol_order)) |>
    ggplot2::ggplot(ggplot2::aes(time, rexp_mean, color = protocol, group = protocol)) +
    ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = rexp_mean - rexp_sd, ymax = rexp_mean + rexp_sd),
      width = 5, linewidth = 0.8, position = ggplot2::position_dodge(width = 5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 1, alpha = 0.5) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::scale_x_continuous(breaks = seq(-30, 120, 30)) +
    ggplot2::scale_color_manual(name = "Ingestion", values = protocol_colors) +
    ggplot2::labs(
      x = "Time (min)",
      y = expression(bold(paste(delta^{13}, "CO"[2], " in expired gases (\u2030)")))
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  # Figure 4: Plasma 13C-Glucose
  fig_rpla <- rpla_summary |>
    dplyr::mutate(protocol = factor(protocol, levels = protocol_order)) |>
    ggplot2::ggplot(ggplot2::aes(time, rpla_mean, color = protocol, group = protocol)) +
    ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = rpla_mean - rpla_sd, ymax = rpla_mean + rpla_sd),
      width = 5, linewidth = 0.8, position = ggplot2::position_dodge(width = 5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 1, alpha = 0.5) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 5)) +
    ggplot2::scale_x_continuous(breaks = seq(-30, 120, 30)) +
    ggplot2::scale_y_continuous(breaks = seq(-25, 25, 10)) +
    ggplot2::scale_color_manual(name = "Ingestion", values = protocol_colors) +
    ggplot2::labs(
      x = "Time (min)",
      y = expression(bold(paste("Plasma ", delta^{13}, "C-glucose (\u2030)")))
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  # Figure 5: Total Energy Contribution (Stacked Bar)
  contrib_total_data <- energy_contribution_summary |>
    dplyr::select(protocol, pct_cho_total_mean, pct_cho_total_sd,
                  pct_fat_mean, pct_fat_sd, pct_protein_mean, pct_protein_sd) |>
    tidyr::pivot_longer(
      cols = -protocol,
      names_to = c("source", ".value"),
      names_pattern = "pct_(.+)_(mean|sd)"
    ) |>
    dplyr::mutate(
      protocol = factor(protocol, levels = protocol_order),
      source = factor(source, levels = c("protein", "fat", "cho_total"))
    ) |>
    dplyr::arrange(protocol, dplyr::desc(source)) |>
    dplyr::group_by(protocol) |>
    dplyr::mutate(adj_mean = cumsum(mean))

  fig_contrib_total <- ggplot2::ggplot(contrib_total_data,
                                        ggplot2::aes(protocol, mean, fill = source)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = adj_mean - sd, ymax = adj_mean),
      width = 0.2
    ) +
    ggplot2::geom_errorbar(
      data = dplyr::filter(contrib_total_data, source == "protein"),
      ggplot2::aes(ymin = mean, ymax = mean + sd),
      width = 0.2
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      labels = c("Protein", "Fat", "Total CHO"),
      values = c("protein" = "gray30", "fat" = "gold", "cho_total" = "pink")
    ) +
    ggplot2::labs(x = "", y = "Contribution to Energy Yield (%)") +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )

  # Figure 6: Exo/Endo CHO Contribution
  contrib_exo_endo_data <- energy_contribution_summary |>
    dplyr::select(protocol, dplyr::starts_with("pct_cho_exo"), dplyr::starts_with("pct_cho_endo"),
                  dplyr::starts_with("pct_fat"), dplyr::starts_with("pct_protein")) |>
    tidyr::pivot_longer(
      cols = -protocol,
      names_to = c("source", ".value"),
      names_pattern = "pct_(.+)_(mean|sd)"
    ) |>
    dplyr::mutate(
      protocol = factor(protocol, levels = protocol_order),
      source = dplyr::case_when(
        protocol == "Water" & source %in% c("cho_exo", "cho_endo") ~ "cho_total_water",
        TRUE ~ source
      )
    ) |>
    dplyr::filter(!(protocol == "Water" & source == "cho_endo")) |>
    dplyr::mutate(
      source = factor(source, levels = c("protein", "fat", "cho_total_water", "cho_endo", "cho_exo"))
    ) |>
    dplyr::arrange(protocol, dplyr::desc(source)) |>
    dplyr::group_by(protocol) |>
    dplyr::mutate(adj_mean = cumsum(mean))

  fig_contrib_exo_endo <- ggplot2::ggplot(contrib_exo_endo_data,
                                           ggplot2::aes(protocol, mean, fill = source)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = adj_mean - sd, ymax = adj_mean),
      width = 0.2
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      labels = c("Protein", "Fat", "CHO (Water)", "CHO Endogenous", "CHO Exogenous"),
      values = c("protein" = "gray30", "fat" = "gold",
                 "cho_total_water" = "pink", "cho_endo" = "orange", "cho_exo" = "red")
    ) +
    ggplot2::labs(x = "", y = "Contribution to Energy Yield (%)") +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )

  # Composite figures
  fig_vo2_rer <- cowplot::plot_grid(
    fig_vo2 + ggplot2::theme(legend.position = "none"),
    fig_rer + ggplot2::theme(legend.position = "none"),
    nrow = 2, labels = c("A", "B"), align = "v"
  )

  legend <- cowplot::get_legend(fig_vo2 + ggplot2::theme(legend.position = "bottom"))

  fig_vo2_rer_combined <- cowplot::plot_grid(
    fig_vo2_rer, legend, ncol = 1, rel_heights = c(1, 0.1)
  )

  fig_isotopes <- cowplot::plot_grid(
    fig_rexp + ggplot2::theme(legend.position = "none"),
    fig_rpla + ggplot2::theme(legend.position = "none"),
    nrow = 2, labels = c("A", "B"), align = "v"
  )

  fig_isotopes_combined <- cowplot::plot_grid(
    fig_isotopes, legend, ncol = 1, rel_heights = c(1, 0.1)
  )

  # Save figures
  ggplot2::ggsave(file.path(output_dir, "fig_vo2.png"), fig_vo2, width = 10, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_vo2.pdf"), fig_vo2, width = 10, height = 5)
  ggplot2::ggsave(file.path(output_dir, "fig_rer.png"), fig_rer, width = 10, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_rer.pdf"), fig_rer, width = 10, height = 5)
  ggplot2::ggsave(file.path(output_dir, "fig_rexp.png"), fig_rexp, width = 10, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_rexp.pdf"), fig_rexp, width = 10, height = 5)
  ggplot2::ggsave(file.path(output_dir, "fig_rpla.png"), fig_rpla, width = 10, height = 5, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_rpla.pdf"), fig_rpla, width = 10, height = 5)
  ggplot2::ggsave(file.path(output_dir, "fig_contrib_total.png"), fig_contrib_total, width = 8, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_contrib_total.pdf"), fig_contrib_total, width = 8, height = 6)
  ggplot2::ggsave(file.path(output_dir, "fig_contrib_exo_endo.png"), fig_contrib_exo_endo, width = 8, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_contrib_exo_endo.pdf"), fig_contrib_exo_endo, width = 8, height = 6)
  ggplot2::ggsave(file.path(output_dir, "fig_vo2_rer_combined.png"), fig_vo2_rer_combined, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_vo2_rer_combined.pdf"), fig_vo2_rer_combined, width = 10, height = 8)
  ggplot2::ggsave(file.path(output_dir, "fig_isotopes_combined.png"), fig_isotopes_combined, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "fig_isotopes_combined.pdf"), fig_isotopes_combined, width = 10, height = 8)

  list(
    fig_vo2 = fig_vo2,
    fig_rer = fig_rer,
    fig_rexp = fig_rexp,
    fig_rpla = fig_rpla,
    fig_contrib_total = fig_contrib_total,
    fig_contrib_exo_endo = fig_contrib_exo_endo,
    fig_vo2_rer_combined = fig_vo2_rer_combined,
    fig_isotopes_combined = fig_isotopes_combined,
    protocol_colors = protocol_colors,
    protocol_order = protocol_order
  )
}
