#' oxidizr: Substrate Oxidation Analysis During Exercise
#'
#' @description
#' The oxidizr package provides a complete workflow for analyzing substrate
#' oxidation during exercise using indirect calorimetry and stable isotope tracers.
#'
#' @section Analysis Pipeline:
#' The typical analysis workflow consists of:
#' \enumerate{
#'   \item **Data Import**: Read data files or create S7 data objects
#'   \item **Data Validation**: Check data quality with `validate_study()`
#'   \item **Steady-State Detection**: Identify valid periods with `detect_steady_state()`
#'   \item **Processing**: Filter and aggregate with `filter_time_range()`, `aggregate_calorimetry()`
#'   \item **Computation**: Calculate oxidation with `analyze_oxidation()` or `calc_*()` functions
#'   \item **Reporting**: Generate outputs with `plot_*()`, `tbl_*()`, and `render_*_report()`
#' }
#'
#' @section Key Functions:
#' \describe{
#'   \item{analyze_oxidation()}{Main analysis function with integrated validation}
#'   \item{validate_study()}{Comprehensive data quality validation}
#'   \item{detect_steady_state()}{Identify steady-state measurement periods}
#'   \item{calc_substrate_oxidation()}{Calculate CHO and fat oxidation rates}
#'   \item{render_quality_report()}{Generate data quality HTML report}
#' }
#'
#' @section S7 Classes:
#' \describe{
#'   \item{CalorimetryData}{Indirect calorimetry measurements}
#'   \item{IsotopeData}{13C isotope enrichment data}
#'   \item{OxidationStudy}{Container for all study data}
#'   \item{OxidationResults}{Analysis results}
#'   \item{ValidationResult}{Validation findings}
#' }
#'
#' @section Edge Cases and Data Quality:
#'
#' **Extreme RER Values:**
#' \itemize{
#'   \item RER < 0.70: Physiologically impossible. Indicates measurement error,
#'     gas analyzer calibration issues, or data entry problems. These values
#'     are flagged as errors by `validate_study()`.
#'   \item RER 0.70-0.75: Unusually low for exercise. May indicate very high
#'     fat oxidation, prolonged fasting, or measurement drift. Review data
#'     for potential issues.
#'   \item RER 0.95-1.00: Approaching anaerobic threshold. Consider whether
#'     exercise intensity was appropriate for substrate oxidation assessment.
#'   \item RER > 1.00: Indicates hyperventilation, non-steady state, or
#'     anaerobic metabolism. Data above 1.10 should be excluded for substrate
#'     oxidation calculations.
#' }
#'
#' **Small Sample Sizes (< 3 subjects):**
#' \itemize{
#'   \item Statistical analyses (standard deviation, coefficient of variation)
#'     may be unreliable with fewer than 3 subjects.
#'   \item Steady-state detection CV thresholds were validated on larger samples;
#'     use caution with n < 3.
#'   \item Consider reporting individual subject data rather than group means
#'     for very small samples.
#' }
#'
#' **Missing Data (NA values):**
#' \itemize{
#'   \item `calc_substrate_oxidation()` offers three NA handling strategies
#'     via the `na_protein_action` parameter: "mean" (impute with group mean),
#'     "zero" (assume negligible protein), or "error" (strict validation).
#'   \item Validation functions report missing data patterns to help identify
#'     systematic vs. random missingness.
#'   \item For isotope calculations, missing `rref` values can be provided as
#'     numeric constants or calculated from control protocol data.
#' }
#'
#' @section Vignettes:
#' \itemize{
#'   \item `vignette("getting-started")`: Quick introduction
#'   \item `vignette("analysis-pipeline")`: Complete workflow guide
#'   \item `vignette("data-quality")`: Validation and quality control
#' }
#'
#' @keywords internal
"_PACKAGE"

#' @import S7
#' @import ggplot2
#' @importFrom dplyr filter select mutate group_by ungroup summarise arrange
#' @importFrom dplyr left_join rename across any_of all_of c_across pick
#' @importFrom dplyr row_number first n lag coalesce if_else case_when
#' @importFrom dplyr slice_head slice count rowwise desc
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom purrr map map_chr map_int map_dbl map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||%
#' @importFrom glue glue
#' @importFrom gt gt fmt_number cols_label data_color tab_header
#' @importFrom stats sd var quantile
#' @importFrom graphics pairs
#' @importFrom utils head
#' @importFrom slider slide_dbl
#' @importFrom scales col_numeric
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_to_title
NULL

# Global variables for NSE column names used in dplyr/tidyr pipelines
# This prevents R CMD check NOTEs about "no visible binding for global variable"
utils::globalVariables(c(

  # Column names used in calc_* functions
  "cho_exo", "cho_pla", "cho_total", "cho_endo", "cho_mus", "cho_liv",
  "fat_total", "protein_ox", "total_protein_ox", "rer",
  "vo2", "vco2", "vo2_mean", "vco2_mean", "ve", "ve_mean",
  "rexp", "rexo", "rref", "rpla", "fexo",

  # Column names in sweat/urea calculations
  "VO2_mean", "sweat_loss", "urine_urea_loss", "sweat_urea_loss",
  "total_urea_loss", "urea_mass_loss",

  # Energy percentage columns
  "pct_cho_total", "pct_cho_exo", "pct_cho_endo", "pct_fat", "pct_protein",
  "e_total",

  # Subject/protocol columns
  "id", "time", "protocol", "ID", "ID.y", "name",

  # Targets pipeline columns
  "daily", "calo_data", "VO2", "VCO2", "Bf", "Vt",
  "A_VO2max", "mass_kg", "VO2max", "VO2max_kg", "PAM",
  "age_height", "initial_body_mass", "initial_body_fat", "final_body_mass",
  "b_date", "height", "age", "ogtt", "pre", "120_min",
  "urea", "urine", "urine_urea", "sweat", "sweat_urea",
  "water_ingested", "saline", "p_atm", "temperature", "humidity",
  "Rexo", "Rref", "Rpla", "Rexp",

  # Summary columns
  "vo2_sd", "rer_mean", "rer_sd", "rexp_mean", "rexp_sd",
  "rpla_mean", "rpla_sd", "pct_cho_total_mean", "pct_cho_total_sd",
  "pct_fat_mean", "pct_fat_sd", "pct_protein_mean", "pct_protein_sd",
  "adj_mean",

  # Gas exchange calculations
  "expired_CO2_mass", "expired_O2_mass", "pH2O_saturated", "pH2O_ambient",
  "H2O_vapor_out", "H2O_vapor_in", "H2O_mass_lost",
  "expired_gas_lost", "body_mass_gain", "mass_lost", "virtual_mass",

  # Validation columns
  "check_id", "category", "variable", "severity", "message",
  "n_affected", "pct_affected", "subject_ids", "time_points", "values",
  "threshold", "reference", "n_issues", "errors", "warnings", "info",
  "total_affected", "fill_color",
  "ValidationResult", "validate_study", "calorimetry_thresholds",

  # Steady-state columns
  "cv", "variance", "variance_threshold", "is_steady", "steady_period",
  "meets_duration", "roll_mean", "roll_sd", "steady_change",
  "is_steady_all", "is_steady_cv", "is_steady_var", "duration",
  "start_time", "end_time", "duration_points", "start", "end",

  # Missing data columns
  "pct_missing", "pct_complete", "n_total", "n_complete", "n_missing",
  "overall_complete", "pattern",

  # Outlier columns
  "out_of_range", "is_outlier_subject", "values_preview",

  # Table columns
  "n_observations", "n_steady", "pct_steady", "n_periods",
  "substrate", "rate", "error", "cumsum", "substrate_label",

  # Plot helper columns
  ".row_id", "value", "yintercept", "type",

  # data.table operator
  ":="
))

# -----------------------------------------------------------------------------
# Stoichiometry
# -----------------------------------------------------------------------------
# Every gas-exchange and energy constant below comes from one table of
# substrates. See ?stoich_coefficients and vignette("stoichiometry").

# Atomic weights (g/mol): Péronnet & Massicotte (1991), Table 1; N from IUPAC.
atomic_weight <- c(C = 12.011, H = 1.00794, O = 15.9994, N = 14.0067)

substrates <- list(
  # C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O
  glucose = list(
    formula = c(C = 6, H = 12, O = 6),
    kcal_g = 3.72,
    source = "IOM (2005), Table 5-1; Livesey & Elia (1988), Table 2: 15.56 kJ/g"
  ),
  # TAG-type C53.4H99.2O6 + 75.2 O2 -> 53.4 CO2 + 49.6 H2O (Telmosse, 2022)
  lipid = list(
    formula = c(C = 53.4, H = 99.2, O = 6),
    kcal_g = 9.44,
    source = "IOM (2005), Table 5-1; Livesey & Elia (1988), Table 6: 39.5 kJ/g"
  ),
  # General values for protein oxidized in the body, N excreted as urea,
  # ammonia and creatinine (90:5:5): Livesey & Elia (1988), Table 6
  protein = list(
    kj_g = 19.68,    # energy density, kJ/g
    kj_l_o2 = 19.48, # heat equivalent of O2, kJ/L
    rq = 0.835,
    n_ratio = 6.25,  # g protein per g N: Livesey & Elia (1988), p. 620
    kcal_g = 4.70,
    source = "IOM (2005), Table 5-1; Livesey & Elia (1988), Table 6: 19.68 kJ/g"
  )
)

# Molar mass of urea, CH4N2O (g/mol)
urea_molar_mass <- sum(atomic_weight * c(C = 1, H = 4, O = 1, N = 2))

#' Gas Exchange per Gram of Substrate
#'
#' Volumes of O2 used and CO2 produced by the complete oxidation of 1 g of a
#' substrate CcHhOo, from the balanced reaction
#' CcHhOo + (c + h/4 - o/2) O2 -> c CO2 + h/2 H2O.
#'
#' @param formula Named numeric vector of atom counts, for example
#'   `c(C = 6, H = 12, O = 6)`. Counts can be fractional for an average
#'   formula such as the TAG-type `c(C = 53.4, H = 99.2, O = 6)`.
#' @param molar_volume Molar volume of the gases (L/mol, STPD). Give one value
#'   for both gases, or `c(O2 = , CO2 = )` for separate values. Default:
#'   22.414 L/mol, the ideal-gas value used by Livesey & Elia (1988).
#'
#' @return A named numeric vector: `o2` (L O2/g) and `co2` (L CO2/g).
#' @export
#'
#' @references
#'   Livesey, G., & Elia, M. (1988). Estimation of energy expenditure, net
#'   carbohydrate utilization, and net fat oxidation and synthesis by indirect
#'   calorimetry: evaluation of errors with special reference to the detailed
#'   composition of fuels. *The American Journal of Clinical Nutrition*,
#'   *47*(4), 608-628.
#'
#'   Péronnet, F., & Massicotte, D. (1991). Table of nonprotein respiratory
#'   quotient: an update. *Canadian Journal of Sport Sciences*, *16*(1), 23-29.
#'
#' @examples
#' gas_yield(c(C = 6, H = 12, O = 6))         # glucose
#' gas_yield(c(C = 53.4, H = 99.2, O = 6))    # TAG-type (Telmosse, 2022)
#'
#' # Real-gas molar volumes reproduce Péronnet & Massicotte (1991), Table 1
#' gas_yield(c(C = 6, H = 12, O = 6), molar_volume = c(O2 = 22.3858, CO2 = 22.2966))
gas_yield <- function(formula, molar_volume = 22.414) {
  if (!is.numeric(formula) || is.null(names(formula)) ||
      !all(names(formula) %in% c("C", "H", "O"))) {
    cli::cli_abort("{.arg formula} must be a named numeric vector of C, H and O counts.")
  }
  v <- if (length(molar_volume) == 1) c(O2 = molar_volume, CO2 = molar_volume) else molar_volume
  f <- c(C = 0, H = 0, O = 0)
  f[names(formula)] <- formula
  mw <- sum(atomic_weight[names(f)] * f)
  o2 <- f[["C"]] + f[["H"]] / 4 - f[["O"]] / 2
  c(o2 = o2 * v[["O2"]] / mw, co2 = f[["C"]] * v[["CO2"]] / mw)
}

#' Energy conversion factors
#'
#' Energy released per gram of substrate oxidized (kcal/g). The values are the
#' heats of combustion of IOM (2005), Table 5-1, which repeats Livesey & Elia
#' (1988): glucose 15.56 kJ/g (Table 2), general fat 39.5 kJ/g and protein
#' oxidized in the body 19.68 kJ/g (Table 6; N excreted as urea, ammonia and
#' creatinine). Telmosse (2022, Table 1) uses the same values.
#'
#' @format A named list with three elements:
#' \describe{
#'   \item{cho}{Glucose: 3.72 kcal/g}
#'   \item{fat}{Triacylglycerol: 9.44 kcal/g}
#'   \item{protein}{Protein oxidized in the body: 4.70 kcal/g}
#' }
#' @seealso [stoich_coefficients], `vignette("stoichiometry")`
#' @references
#'   Institute of Medicine (2005). *Dietary Reference Intakes for Energy,
#'   Carbohydrate, Fiber, Fat, Fatty Acids, Cholesterol, Protein, and Amino
#'   Acids*. National Academies Press. \doi{10.17226/10490}
#'
#'   Livesey, G., & Elia, M. (1988). *The American Journal of Clinical
#'   Nutrition*, *47*(4), 608-628.
#' @export
energy_factors <- list(
  cho = substrates$glucose$kcal_g,
  fat = substrates$lipid$kcal_g,
  protein = substrates$protein$kcal_g
)

#' Stoichiometric coefficients
#'
#' Coefficients that convert gas exchange (L/min) into substrate oxidation
#' (g/min). The package computes them when it is built, from balanced
#' reactions, as Telmosse (2022, pp. 17-26) shows step by step.
#'
#' @details
#' The gas volumes per gram come from [gas_yield()] at 22.414 L/mol
#' (Livesey & Elia, 1988) for:
#' \itemize{
#'   \item glucose, C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O (0.7465 L O2 and CO2 per g);
#'   \item the TAG-type of Telmosse (2022), C53.4H99.2O6 + 75.2 O2 ->
#'     53.4 CO2 + 49.6 H2O (2.0129 L O2/g, 1.4294 L CO2/g, RQ 0.710).
#' }
#' Solving VO2 = 0.7465 CHO + 2.0129 FAT and VCO2 = 0.7465 CHO + 1.4294 FAT
#' gives CHO = 4.6211 VCO2 - 3.2815 VO2 and FAT = 1.7137 (VO2 - VCO2).
#' Telmosse (2022) obtained 4.618, 3.279 and 1.712 with 22.4 L/mol, whole-number
#' atomic weights and rounded volumes.
#'
#' Protein values are computed from the general values of Livesey & Elia
#' (1988, Table 6): 19.68 kJ/g / 19.48 kJ/L O2 = 1.0103 L O2/g, times RQ 0.835
#' = 0.8436 L CO2/g. The urea factor is 6.25 g protein per g N (Livesey & Elia,
#' 1988, p. 620) times the N fraction of urea (0.46646), that is 2.9154 g
#' protein per g urea.
#'
#' See `vignette("stoichiometry")` for the reasons behind each choice.
#'
#' @format A named list:
#' \describe{
#'   \item{cho_vo2, cho_vco2}{CHO (g/min) = cho_vo2 * VO2 + cho_vco2 * VCO2}
#'   \item{fat_vo2, fat_vco2}{Fat (g/min) = fat_vo2 * VO2 + fat_vco2 * VCO2}
#'   \item{protein_vo2, protein_vco2}{L O2 used and L CO2 produced per g protein}
#'   \item{protein_urea}{g protein oxidized per g urea excreted}
#'   \item{co2_cho_ratio}{L CO2 per g glucose, the k of the 13C equations}
#' }
#' @seealso [gas_yield()], [energy_factors], `vignette("stoichiometry")`
#' @references
#'   Telmosse, E. (2022). *Contribution des glucides exogènes à la fourniture
#'   d’énergie lors d’un effort prolongé : analyse par régression multiple*.
#'   Mémoire de maîtrise, Université de Montréal.
#'
#'   Livesey, G., & Elia, M. (1988). *The American Journal of Clinical
#'   Nutrition*, *47*(4), 608-628.
#'
#'   Institute of Medicine (2005). National Academies Press. \doi{10.17226/10490}
#'
#'   Péronnet, F., & Massicotte, D. (1991). *Canadian Journal of Sport
#'   Sciences*, *16*(1), 23-29.
#' @export
stoich_coefficients <- local({
  cho <- gas_yield(substrates$glucose$formula)
  fat <- gas_yield(substrates$lipid$formula)
  # [CHO, fat] (g/min) = k %*% [VO2, VCO2] (L/min)
  k <- solve(rbind(c(cho[["o2"]], fat[["o2"]]),
                   c(cho[["co2"]], fat[["co2"]])))
  pro <- substrates$protein
  urea_n <- 2 * atomic_weight[["N"]] / urea_molar_mass # g N per g urea
  list(
    cho_vo2 = k[1, 1], cho_vco2 = k[1, 2],
    fat_vo2 = k[2, 1], fat_vco2 = k[2, 2],
    protein_vo2 = pro$kj_g / pro$kj_l_o2,           # L O2 per g protein
    protein_vco2 = pro$rq * pro$kj_g / pro$kj_l_o2, # L CO2 per g protein
    protein_urea = pro$n_ratio * urea_n,            # g protein per g urea
    co2_cho_ratio = cho[["co2"]]                    # L CO2 per g glucose (13C)
  )
})
