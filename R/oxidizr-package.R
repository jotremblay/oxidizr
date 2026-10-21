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
# Every gas-exchange and energy constant comes from the options below, through
# oxidation_constants(). See ?oxidation_constants and vignette("stoichiometry").

# Atomic weights (g/mol): Péronnet & Massicotte (1991), Table 1; N from IUPAC.
atomic_weight <- c(C = 12.011, H = 1.00794, O = 15.9994, N = 14.0067)

# Molar mass of urea, CH4N2O (g/mol)
urea_molar_mass <- sum(atomic_weight * c(C = 1, H = 4, O = 1, N = 2))

# The options of each part of a constant set, with their sources. The first
# option of each part is the default.
constant_options <- list(
  lipid = list(
    diop = list(
      formula = c(C = 55, H = 102, O = 6),
      label = "dioleoyl-palmitoyl-glycerol C55H102O6",
      source = "Livesey & Elia (1988), Table 4; Elia & Livesey (1992), Table 1"
    ),
    tag_type = list(
      formula = c(C = 53.4, H = 99.2, O = 6),
      label = "TAG-type C53.4H99.2O6",
      source = "Telmosse (2022), p. 20; matches IOM (2005), Table 5-1"
    ),
    tripalmitin = list(
      formula = c(C = 51, H = 98, O = 6),
      label = "tripalmitin C51H98O6",
      source = "Kleiber (1961), Table 5.6"
    ),
    pos = list(
      formula = c(C = 55, H = 104, O = 6),
      label = "palmitoyl-stearoyl-oleoyl-glycerol C55H104O6",
      source = "Frayn (1983); Ferrannini (1988)"
    ),
    fatty_acid = list(
      formula = c(C = 17.2702, H = 32.7142, O = 2),
      label = "mean fatty acid C17.2702H32.7142O2",
      source = "P\u00e9ronnet & Massicotte (1991), Table 1"
    )
  ),
  molar_volume = list(
    livesey_elia = list(
      value = c(O2 = 22.414, CO2 = 22.414),
      label = "22.414 L/mol (ideal gas)",
      source = "Livesey & Elia (1988), p. 609"
    ),
    ideal_22.4 = list(
      value = c(O2 = 22.4, CO2 = 22.4),
      label = "22.4 L/mol (ideal gas)",
      source = "Telmosse (2022), p. 17"
    ),
    real_gas = list(
      value = c(O2 = 22.3858, CO2 = 22.2966),
      label = "O2 22.3858, CO2 22.2966 L/mol (real gases)",
      source = "P\u00e9ronnet & Massicotte (1991), Table 1"
    )
  ),
  protein = list(
    kleiber = list(
      formula = c(C = 100, H = 159, O = 32, N = 26),
      label = "Kleiber's standard protein without S, C100H159N26O32, N to urea",
      source = "Kleiber (1961), Table 4.2 and p. 85; formula of Elia & Livesey (1992), Table 1, without S"
    ),
    livesey_elia = list(
      kj_g = 19.68,    # energy density, kJ/g
      kj_l_o2 = 19.48, # heat equivalent of O2, kJ/L
      rq = 0.835,
      n_ratio = 6.25,  # g protein per g N
      label = "general values: 19.68 kJ/g, 19.48 kJ/L O2, RQ 0.835",
      source = "Livesey & Elia (1988), Table 6; 6.25 g/g N: p. 620"
    ),
    protein_type = list(
      formula = c(C = 89, H = 148.5, O = 32, N = 23.5),
      label = "protein-type C89H148.5O32N23.5, N to urea",
      source = "Telmosse (2022), pp. 21-22; matches IOM (2005)"
    ),
    telmosse_table1 = list(
      o2 = 1.0075, co2 = 0.8443, urea = 2.915,
      label = "1.0075 L O2/g, 0.8443 L CO2/g, 2.915 g/g urea",
      source = "Telmosse (2022), Table 1, p. 18"
    )
  ),
  energy = list(
    iom = list(
      cho = 3.72, fat = 9.44, protein = 4.70,
      label = "3.72 / 9.44 / 4.70 kcal/g",
      source = "IOM (2005), Table 5-1 = Livesey & Elia (1988), Tables 2 and 6"
    ),
    peronnet_massicotte = list(
      cho = 3.8683, fat = 9.7460, protein = 4.70,
      label = "3.8683 / 9.7460 / 4.70 kcal/g (37 degrees C)",
      source = "P\u00e9ronnet & Massicotte (1991), Table 1; protein: IOM (2005)"
    )
  )
)

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

# Protein oxidized to urea:
# CcHhOoNn + O2 -> (c - n/2) CO2 + (h/2 - n) H2O + n/2 CH4N2O
# Returns L O2 and L CO2 per g protein, and g protein per g urea.
protein_yield <- function(formula, molar_volume) {
  u <- formula[["N"]] / 2                                   # mol urea per mol
  mw <- sum(atomic_weight[c("C", "H", "O", "N")] * formula[c("C", "H", "O", "N")])
  cc <- formula[["C"]] - u                                  # C that leaves as CO2
  o2 <- cc + (formula[["H"]] - 4 * u) / 4 - (formula[["O"]] - u) / 2
  c(o2 = o2 * molar_volume[["O2"]] / mw,
    co2 = cc * molar_volume[["CO2"]] / mw,
    urea = mw / (u * urea_molar_mass))
}

#' Choose the Constants of the Calculations
#'
#' Build the set of constants that every calculation step uses: the CHO and fat
#' equations, the protein gas exchange and urea factor, the CO2 yield of glucose
#' for the 13C equations, and the energy per gram. Each part has documented
#' options; the defaults are those of oxidizr. Give the result to
#' [analyze_oxidation()] (argument `constants`), or compare several sets with
#' [compare_constants()].
#'
#' @param lipid The lipid whose oxidation reaction sets the fat equation:
#'   \describe{
#'     \item{`"diop"`}{(default) dioleoyl-palmitoyl-glycerol C55H102O6, the
#'       reference fat of Livesey & Elia (1988, Table 4) and Elia & Livesey
#'       (1992, Table 1), close to the average triacylglycerol of human adipose
#'       tissue (Péronnet & Massicotte, 1991): RQ 0.7097.}
#'     \item{`"tag_type"`}{TAG-type C53.4H99.2O6 of Telmosse (2022, p. 20),
#'       chosen to match IOM (2005, Table 5-1): RQ 0.710.}
#'     \item{`"tripalmitin"`}{tripalmitin C51H98O6, the fat of Kleiber (1961,
#'       Table 5.6): RQ 0.703.}
#'     \item{`"pos"`}{palmitoyl-stearoyl-oleoyl-glycerol C55H104O6, used by
#'       Frayn (1983) and Ferrannini (1988).}
#'     \item{`"fatty_acid"`}{the mean fatty acid of human adipose tissue,
#'       C17.2702H32.7142O2, of Péronnet & Massicotte (1991, Table 1), also
#'       used by Jeukendrup & Wallis (2005).}
#'   }
#' @param molar_volume The volume of one mole of O2 and CO2 at STPD:
#'   \describe{
#'     \item{`"livesey_elia"`}{(default) 22.414 L/mol for both gases (Livesey &
#'       Elia, 1988). Glucose has an RQ of 1.}
#'     \item{`"ideal_22.4"`}{22.4 L/mol for both gases (Telmosse, 2022, p. 17).}
#'     \item{`"real_gas"`}{22.3858 L/mol for O2 and 22.2966 L/mol for CO2,
#'       the real-gas values of Péronnet & Massicotte (1991, Table 1). Glucose
#'       has an RQ of 0.996, so CHO increases by about 2 percent and fat decreases
#'       by about 5 percent at RER 0.92.}
#'   }
#' @param protein The protein gas exchange and urea factor:
#'   \describe{
#'     \item{`"kleiber"`}{(default) Kleiber's standard protein (1961, Table
#'       4.2: 53% C, 7% H, 16% N, 23% O, 1% S) without its sulfur, which Kleiber
#'       (p. 85) judged small enough to neglect: C100H159N26O32, oxidized to
#'       urea at the chosen molar volume. RQ 0.835; 16.3% N, so 6.14 g protein
#'       per g N.}
#'     \item{`"livesey_elia"`}{the general values of Livesey & Elia
#'       (1988, Table 6): 19.68 kJ/g / 19.48 kJ/L O2 = 1.0103 L O2/g, RQ 0.835;
#'       6.25 g protein per g N (p. 620), so 2.9154 g protein per g urea.}
#'     \item{`"protein_type"`}{the protein-type C89H148.5O32N23.5 of Telmosse
#'       (2022, pp. 21-22), oxidized to urea, at the chosen molar volume.}
#'     \item{`"telmosse_table1"`}{1.0075 L O2/g, 0.8443 L CO2/g and 2.915 g
#'       per g urea (Telmosse, 2022, Table 1), the values of earlier oxidizr
#'       versions.}
#'   }
#' @param energy The energy per gram of CHO, fat and protein:
#'   \describe{
#'     \item{`"iom"`}{(default) 3.72, 9.44 and 4.70 kcal/g: IOM (2005, Table
#'       5-1), which repeats Livesey & Elia (1988, Tables 2 and 6).}
#'     \item{`"peronnet_massicotte"`}{3.8683 kcal/g for glucose and 9.7460
#'       kcal/g for the mean fatty acid at 37 degrees C (Péronnet & Massicotte,
#'       1991, Table 1); protein 4.70 kcal/g. Use it with
#'       `lipid = "fatty_acid"`.}
#'   }
#'
#' @return A list of class `oxidation_constants` with the elements of
#'   [stoich_coefficients], `energy` (a list like [energy_factors]), and
#'   `choices` (the option of each part).
#' @export
#'
#' @seealso [stoich_coefficients], [compare_constants()], `vignette("stoichiometry")`
#' @references
#'   Elia, M., & Livesey, G. (1992). Energy expenditure and fuel selection in
#'   biological systems. *World Review of Nutrition and Dietetics*, *70*,
#'   68-131.
#'
#'   Ferrannini, E. (1988). The theoretical bases of indirect calorimetry: a
#'   review. *Metabolism*, *37*(3), 287-301.
#'
#'   Frayn, K. N. (1983). Calculation of substrate oxidation rates in vivo from
#'   gaseous exchange. *Journal of Applied Physiology*, *55*(2), 628-634.
#'
#'   Institute of Medicine (2005). *Dietary Reference Intakes for Energy,
#'   Carbohydrate, Fiber, Fat, Fatty Acids, Cholesterol, Protein, and Amino
#'   Acids*. National Academies Press. \doi{10.17226/10490}
#'
#'   Jeukendrup, A. E., & Wallis, G. A. (2005). Measurement of substrate
#'   oxidation during exercise by means of gas exchange measurements.
#'   *International Journal of Sports Medicine*, *26*(Suppl 1), S28-S37.
#'
#'   Kleiber, M. (1961). *The Fire of Life: An Introduction to Animal
#'   Energetics*. Wiley.
#'
#'   Livesey, G., & Elia, M. (1988). *The American Journal of Clinical
#'   Nutrition*, *47*(4), 608-628.
#'
#'   Péronnet, F., & Massicotte, D. (1991). *Canadian Journal of Sport
#'   Sciences*, *16*(1), 23-29.
#'
#'   Telmosse, E. (2022). *Contribution des glucides exogènes à la fourniture
#'   d’énergie lors d’un effort prolongé : analyse par régression multiple*.
#'   Mémoire de maîtrise, Université de Montréal.
#'
#' @examples
#' oxidation_constants()
#'
#' # The equations of Péronnet & Massicotte (1991) and Jeukendrup & Wallis (2005)
#' oxidation_constants(lipid = "fatty_acid", molar_volume = "real_gas")
oxidation_constants <- function(lipid = "diop",
                                molar_volume = "livesey_elia",
                                protein = "kleiber",
                                energy = "iom") {
  lipid <- match.arg(lipid, names(constant_options$lipid))
  molar_volume <- match.arg(molar_volume, names(constant_options$molar_volume))
  protein <- match.arg(protein, names(constant_options$protein))
  energy <- match.arg(energy, names(constant_options$energy))

  if (energy == "peronnet_massicotte" && lipid != "fatty_acid") {
    cli::cli_warn(c(
      "The 9.7460 kcal/g of P\u00e9ronnet & Massicotte (1991) is for their mean fatty acid.",
      "i" = "Use {.code lipid = \"fatty_acid\"} with {.code energy = \"peronnet_massicotte\"}."
    ))
  }

  v <- constant_options$molar_volume[[molar_volume]]$value
  cho <- gas_yield(c(C = 6, H = 12, O = 6), v)
  fat <- gas_yield(constant_options$lipid[[lipid]]$formula, v)
  # [CHO, fat] (g/min) = k %*% [VO2, VCO2] (L/min)
  k <- solve(rbind(c(cho[["o2"]], fat[["o2"]]),
                   c(cho[["co2"]], fat[["co2"]])))

  p <- constant_options$protein[[protein]]
  pro <- if (!is.null(p$formula)) {
    protein_yield(p$formula, v)
  } else if (!is.null(p$rq)) {
    urea_n <- 2 * atomic_weight[["N"]] / urea_molar_mass  # g N per g urea
    c(o2 = p$kj_g / p$kj_l_o2, co2 = p$rq * p$kj_g / p$kj_l_o2, urea = p$n_ratio * urea_n)
  } else {
    c(o2 = p$o2, co2 = p$co2, urea = p$urea)
  }

  e <- constant_options$energy[[energy]]
  structure(
    list(
      cho_vo2 = k[1, 1], cho_vco2 = k[1, 2],
      fat_vo2 = k[2, 1], fat_vco2 = k[2, 2],
      protein_vo2 = pro[["o2"]],      # L O2 per g protein
      protein_vco2 = pro[["co2"]],    # L CO2 per g protein
      protein_urea = pro[["urea"]],   # g protein per g urea
      co2_cho_ratio = cho[["co2"]],   # L CO2 per g glucose (13C)
      energy = list(cho = e$cho, fat = e$fat, protein = e$protein),
      choices = list(lipid = lipid, molar_volume = molar_volume,
                     protein = protein, energy = energy)
    ),
    class = "oxidation_constants"
  )
}

#' @export
print.oxidation_constants <- function(x, ...) {
  cat("<oxidation_constants>\n")
  for (part in names(x$choices)) {
    opt <- constant_options[[part]][[x$choices[[part]]]]
    cat(sprintf("  %-13s %s\n  %-13s %s\n", gsub("_", " ", part), opt$label, "", opt$source))
  }
  f4 <- function(v) formatC(v, format = "f", digits = 4)
  cat(sprintf("  CHO (g/min) = %s VCO2 - %s VO2\n", f4(x$cho_vco2), f4(-x$cho_vo2)))
  cat(sprintf("  Fat (g/min) = %s VO2 - %s VCO2\n", f4(x$fat_vo2), f4(-x$fat_vco2)))
  cat(sprintf("  Protein: %s L O2/g, %s L CO2/g, %s g per g urea\n",
              f4(x$protein_vo2), f4(x$protein_vco2), f4(x$protein_urea)))
  cat(sprintf("  13C: k = %s L CO2 per g glucose\n", f4(x$co2_cho_ratio)))
  invisible(x)
}

#' Energy conversion factors
#'
#' Energy released per gram of substrate oxidized (kcal/g), from the default
#' [oxidation_constants()]. The values are the heats of combustion of IOM
#' (2005), Table 5-1, which repeats Livesey & Elia (1988): glucose 15.56 kJ/g
#' (Table 2), general fat 39.5 kJ/g and protein oxidized in the body 19.68
#' kJ/g (Table 6; N excreted as urea, ammonia and creatinine). Telmosse (2022,
#' Table 1) uses the same values.
#'
#' @format A named list with three elements:
#' \describe{
#'   \item{cho}{Glucose: 3.72 kcal/g}
#'   \item{fat}{Triacylglycerol: 9.44 kcal/g}
#'   \item{protein}{Protein oxidized in the body: 4.70 kcal/g}
#' }
#' @seealso [oxidation_constants()], [stoich_coefficients], `vignette("stoichiometry")`
#' @references
#'   Institute of Medicine (2005). *Dietary Reference Intakes for Energy,
#'   Carbohydrate, Fiber, Fat, Fatty Acids, Cholesterol, Protein, and Amino
#'   Acids*. National Academies Press. \doi{10.17226/10490}
#'
#'   Livesey, G., & Elia, M. (1988). *The American Journal of Clinical
#'   Nutrition*, *47*(4), 608-628.
#' @export
energy_factors <- oxidation_constants()$energy

#' Stoichiometric coefficients
#'
#' Coefficients that convert gas exchange (L/min) into substrate oxidation
#' (g/min), from the default [oxidation_constants()]. The package computes them
#' when it is built, from balanced reactions. Use [oxidation_constants()] to
#' choose other options.
#'
#' @details
#' The gas volumes per gram come from [gas_yield()] at 22.414 L/mol
#' (Livesey & Elia, 1988) for:
#' \itemize{
#'   \item glucose, C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O (0.7465 L O2 and CO2 per g);
#'   \item dioleoyl-palmitoyl-glycerol, the reference fat of Livesey & Elia
#'     (1988, Table 4) and Elia & Livesey (1992, Table 1), C55H102O6 + 77.5 O2
#'     -> 55 CO2 + 51 H2O (2.0212 L O2/g, 1.4344 L CO2/g, RQ 0.7097).
#' }
#' Solving VO2 = 0.7465 CHO + 2.0212 FAT and VCO2 = 0.7465 CHO + 1.4344 FAT
#' gives CHO = 4.6143 VCO2 - 3.2746 VO2 and FAT = 1.7041 (VO2 - VCO2).
#'
#' Protein is Kleiber's standard protein (1961, Table 4.2) without its sulfur,
#' C100H159N26O32, oxidized to urea:
#' C100H159N26O32 + 104.25 O2 -> 87 CO2 + 13 CO(NH2)2 + 53.5 H2O,
#' that is 1.0443 L O2/g and 0.8715 L CO2/g (RQ 0.835). The protein is 16.3% N,
#' so 1 g of urea comes from 2.8660 g of protein.
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
#' @seealso [oxidation_constants()], [gas_yield()], [energy_factors],
#'   `vignette("stoichiometry")`
#' @references
#'   Elia, M., & Livesey, G. (1992). Energy expenditure and fuel selection in
#'   biological systems. *World Review of Nutrition and Dietetics*, *70*,
#'   68-131.
#'
#'   Kleiber, M. (1961). *The Fire of Life: An Introduction to Animal
#'   Energetics*. Wiley.
#'
#'   Livesey, G., & Elia, M. (1988). *The American Journal of Clinical
#'   Nutrition*, *47*(4), 608-628.
#'
#'   Institute of Medicine (2005). National Academies Press. \doi{10.17226/10490}
#'
#'   Péronnet, F., & Massicotte, D. (1991). *Canadian Journal of Sport
#'   Sciences*, *16*(1), 23-29.
#' @export
stoich_coefficients <- unclass(oxidation_constants())[c(
  "cho_vo2", "cho_vco2", "fat_vo2", "fat_vco2",
  "protein_vo2", "protein_vco2", "protein_urea", "co2_cho_ratio"
)]
