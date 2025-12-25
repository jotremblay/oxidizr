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

#' Energy conversion factors
#'
#' Default energy conversion factors (kcal/g) for substrate oxidation
#'
#' @format A named list with three elements:
#' \describe{
#'   \item{cho}{Carbohydrate energy factor (default: 3.72 kcal/g)}
#'   \item{fat}{Fat energy factor (default: 9.44 kcal/g)}
#'   \item{protein}{Protein energy factor (default: 4.70 kcal/g)}
#' }
#' @export
energy_factors <- list(

  cho = 3.72,
  fat = 9.44,

  protein = 4.70
)

#' Stoichiometric coefficients
#'
#' Coefficients for calculating substrate oxidation from gas exchange,
#' as derived and presented in Telmosse (2022).
#'
#' @format A named list with coefficients for CHO and fat calculations
#' @references
#'   Telmosse, E. (2022). Contribution des glucides exogènes à la fourniture d’énergie lors d’un effort prolongé : analyse par régression multiple. Mémoire de maîtrise, Université de Montréal.
#'   Livesey, G., & Elia, M. (1988). Estimation of energy expenditure, net carbohydrate utilization, and net fat oxidation and synthesis by indirect calorimetry: evaluation of errors with special reference to the detailed composition of fuels. *The American Journal of Clinical Nutrition*, *47*(4), 608–628.
#'   Institute of Medicine, Food and Nutrition Board. (2005). *Dietary Reference Intakes for Energy, Carbohydrate, Fiber, Fat, Fatty Acids, Cholesterol, Protein, and Amino Acids*. National Academies Press.
#'   Péronnet, F., & Massicotte, D. (1991). Table of nonprotein respiratory quotient: an update. *Canadian Journal of Sport Sciences = Journal Canadien Des Sciences Du Sport*, *16*(1), 23–29.
#' @export
stoich_coefficients <- list(
  # CHO = 4.618 * VCO2 - 3.279 * VO2
  cho_vco2 = 4.618,
  cho_vo2 = -3.279,
  # Fat = -1.712 * VCO2 + 1.712 * VO2
  fat_vco2 = -1.712,
  fat_vo2 = 1.712,
  # Protein contribution to gas exchange
  protein_vo2 = 1.0075,  # L O2 per g protein
  protein_vco2 = 0.8443, # L CO2 per g protein
  # CO2/CHO ratio for isotope calculations
  co2_cho_ratio = 0.747
)
