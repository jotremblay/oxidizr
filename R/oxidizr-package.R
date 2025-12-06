#' @keywords internal
"_PACKAGE"

#' @import S7
#' @import ggplot2
#' @importFrom dplyr filter select mutate group_by ungroup summarise arrange
#' @importFrom dplyr left_join rename across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom gt gt
#' @importFrom stats sd
#' @importFrom graphics pairs
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
