#' Data Validation Functions
#'
#' @name validation
#' @description Functions for validating oxidation study data quality
NULL

# -----------------------------------------------------------------------------
# Physiological Thresholds
# -----------------------------------------------------------------------------

#' Calorimetry Validation Thresholds
#'
#' Default physiological thresholds for validating indirect calorimetry data.
#' Values based on exercise physiology literature.
#'
#' @format A named list with thresholds for each variable:
#' \describe{
#'   \item{vo2}{VO2 thresholds in L/min (min, max, typical_min, typical_max)}
#'   \item{vco2}{VCO2 thresholds in L/min}
#'   \item{rer}{Respiratory Exchange Ratio thresholds}
#'   \item{cho_oxidation}{CHO oxidation rate thresholds in g/min}
#'   \item{fat_oxidation}{Fat oxidation rate thresholds in g/min}
#'   \item{protein_oxidation}{Protein oxidation rate thresholds in g/min}
#' }
#'
#' @references
#' Astrand PO, Rodahl K, Dahl HA, Stromme SB. (2003). Textbook of Work Physiology.
#' Brooks GA, Mercier J. (1994). Balance of carbohydrate and lipid utilization
#'   during exercise. J Appl Physiol, 76(6), 2253-2261.
#' Jeukendrup AE. (2004). Carbohydrate intake during exercise and performance.
#'   Nutrition, 20(7-8), 669-677.
#' Achten J, Jeukendrup AE. (2004). Optimizing fat oxidation through exercise
#'   and diet. Nutrition, 20(7-8), 716-727.
#'
#' @export
calorimetry_thresholds <- list(
  vo2 = list(
    min = 0.2,
    max = 6.0,
    typical_min = 0.8,
    typical_max = 4.5,
    unit = "L/min",
    reference = "Astrand & Rodahl, 2003"
  ),
  vco2 = list(
    min = 0.15,
    max = 5.5,
    typical_min = 0.7,
    typical_max = 4.0,
    unit = "L/min",
    reference = "Astrand & Rodahl, 2003"
  ),
  rer = list(
    min = 0.70,
    max = 1.10,
    warning_low = 0.75,
    warning_high = 1.05,
    unit = "ratio",
    reference = "Brooks & Mercier, 1994"
  ),
  cho_oxidation = list(
    min = 0,
    max = 5.0,
    warning_high = 3.5,
    unit = "g/min",
    reference = "Jeukendrup, 2004"
  ),
  fat_oxidation = list(
    min = 0,
    max = 1.5,
    warning_high = 1.2,
    unit = "g/min",
    reference = "Achten & Jeukendrup, 2004"
  ),
  protein_oxidation = list(
    min = 0,
    max = 0.5,
    warning_high = 0.3,
    unit = "g/min",
    reference = "Tarnopolsky, 2004"
  )
)

#' Isotope Validation Thresholds
#'
#' Default thresholds for validating 13C isotope enrichment data.
#'
#' @format A named list with thresholds for each isotope measurement
#'
#' @references
#' Craig H. (1957). Isotopic standards for carbon and oxygen and correction
#'   factors for mass-spectrometric analysis. Geochimica et Cosmochimica Acta.
#' Pirnay F, et al. (1977). Fate of exogenous glucose during exercise of
#'   different intensities in humans. J Appl Physiol.
#'
#' @export
isotope_thresholds <- list(
  rexp = list(
    min = -35,
    max = 100,
    unit = "delta per mil",
    reference = "Craig, 1957; Pirnay et al., 1977"
  ),
  rexo = list(
    min = -30,
    max = 200,
    unit = "delta per mil",
    reference = "Lefebvre, 1985"
  ),
  rref = list(
    min = -35,
    max = -15,
    unit = "delta per mil",
    reference = "Pirnay et al., 1977"
  ),
  rpla = list(
    min = -30,
    max = 100,
    unit = "delta per mil",
    reference = "Mosora et al., 1976"
  )
)

#' Environment Validation Thresholds
#'
#' Default thresholds for validating environmental conditions.
#'
#' @format A named list with thresholds for temperature, humidity, and pressure
#'
#' @references
#' ACSM. (2022). ACSM's Guidelines for Exercise Testing and Prescription.
#'
#' @export
environment_thresholds <- list(
  temperature = list(
    min = 10,
    max = 45,
    typical_min = 18,
    typical_max = 25,
    unit = "Celsius",
    reference = "ACSM Guidelines, 2022"
  ),
  humidity = list(
    min = 0,
    max = 100,
    typical_min = 30,
    typical_max = 70,
    unit = "percent",
    reference = "ACSM Guidelines, 2022"
  ),
  pressure = list(
    min = 650,
    max = 800,
    typical_min = 700,
    typical_max = 780,
    unit = "mmHg",
    reference = "ACSM Guidelines, 2022"
  )
)

#' Urea Validation Thresholds
#'
#' Default thresholds for validating urea concentration data.
#'
#' @format A named list with thresholds for sweat and urine urea
#'
#' @references
#' Shirreffs SM, Maughan RJ. (1997). Whole body sweat collection in humans.
#'   J Appl Physiol, 82(1), 336-341.
#' Lemon PW. (1991). Effect of exercise on protein requirements. J Sports Sci.
#'
#' @export
urea_thresholds <- list(

  sweat_urea = list(
    min = 1,
    max = 30,
    typical_min = 3,
    typical_max = 15,
    unit = "mmol/L",
    reference = "Shirreffs & Maughan, 1997"
  ),
  urine_urea = list(
    min = 50,
    max = 700,
    typical_min = 150,
    typical_max = 500,
    unit = "mmol/L",
    reference = "Lemon, 1991"
  ),
  urine_volume = list(
    min = 0,
    max = 3000,
    typical_min = 100,
    typical_max = 1500,
    unit = "mL",
    reference = "Maughan & Shirreffs, 2010"
  )
)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Create Empty Issues Tibble
#'
#' Create an empty tibble with the standard issues structure.
#'
#' @return An empty tibble with validation issue columns
#' @keywords internal
empty_issues <- function() {
  tibble::tibble(
    check_id = character(),
    category = character(),
    variable = character(),
    severity = character(),
    message = character(),
    n_affected = integer(),
    pct_affected = numeric(),
    subject_ids = list(),
    time_points = list(),
    values = list(),
    threshold = character(),
    reference = character()
  )
}

#' Add Validation Issue
#'
#' Add a validation issue to the issues tibble.
#'
#' @param issues Existing issues tibble
#' @param check_id Unique identifier for this check
#' @param category Category of check (calorimetry, isotope, environment, urea, consistency)
#' @param variable Variable being checked
#' @param severity Severity level (error, warning, info)
#' @param message Human-readable message
#' @param n_affected Number of affected observations
#' @param n_total Total number of observations
#' @param subject_ids Vector of affected subject IDs
#' @param time_points Vector of affected time points
#' @param values Sample of problematic values
#' @param threshold Threshold that was violated
#' @param reference Literature reference
#'
#' @return Updated issues tibble
#' @keywords internal
add_issue <- function(issues,
                       check_id,
                       category,
                       variable,
                       severity,
                       message,
                       n_affected = 0L,
                       n_total = 0L,
                       subject_ids = NULL,
                       time_points = NULL,
                       values = NULL,
                       threshold = "",
                       reference = "") {

  pct_affected <- if (n_total > 0) round(n_affected / n_total * 100, 2) else 0

  new_issue <- tibble::tibble(
    check_id = check_id,
    category = category,
    variable = variable,
    severity = severity,
    message = message,
    n_affected = as.integer(n_affected),
    pct_affected = pct_affected,
    subject_ids = list(subject_ids),
    time_points = list(time_points),
    values = list(values),
    threshold = threshold,
    reference = reference
  )

  dplyr::bind_rows(issues, new_issue)
}

#' Detect Outliers Using IQR Method
#'
#' Identify outliers using the interquartile range method.
#'
#' @param x Numeric vector
#' @param k Multiplier for IQR (default: 1.5)
#'
#' @return Logical vector indicating outliers
#' @keywords internal
detect_outliers_iqr <- function(x, k = 1.5) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - k * iqr
  upper <- q3 + k * iqr
  !is.na(x) & (x < lower | x > upper)
}

#' Calculate Data Completeness Summary
#'
#' Calculate completeness metrics for data columns.
#'
#' @param data A data frame
#' @param variables Variables to check (NULL = all numeric)
#'
#' @return A tibble with completeness metrics
#' @keywords internal
calc_completeness <- function(data, variables = NULL) {
  if (is.null(variables)) {
    variables <- names(data)[sapply(data, is.numeric)]
  }

  purrr::map_dfr(variables, function(var) {
    if (!var %in% names(data)) {
      return(tibble::tibble(
        variable = var,
        n_total = nrow(data),
        n_complete = 0L,
        n_missing = as.integer(nrow(data)),
        pct_complete = 0
      ))
    }
    vals <- data[[var]]
    tibble::tibble(
      variable = var,
      n_total = length(vals),
      n_complete = sum(!is.na(vals)),
      n_missing = sum(is.na(vals)),
      pct_complete = round(sum(!is.na(vals)) / length(vals) * 100, 2)
    )
  })
}

# -----------------------------------------------------------------------------
# Validate Calorimetry
# -----------------------------------------------------------------------------

#' Validate Calorimetry Data
#'
#' Check indirect calorimetry data against physiological ranges and for
#' data quality issues.
#'
#' @param calo A CalorimetryData object or data frame
#' @param thresholds Custom thresholds (NULL = use calorimetry_thresholds)
#' @param check_outliers Logical; detect statistical outliers using IQR method
#' @param check_missing Logical; check for missing data patterns
#' @param check_rer Logical; validate derived RER values
#' @param id_col Name of ID column (required if data frame)
#' @param time_col Name of time column (required if data frame)
#' @param vo2_col Name of VO2 column (required if data frame)
#' @param vco2_col Name of VCO2 column (required if data frame)
#' @param vo2_unit Unit for VO2/VCO2 (default: "L/min")
#'
#' @return A ValidationResult S7 object
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   time = c(30, 60, 30, 60),
#'   vo2 = c(2.5, 2.6, 2.4, 7.0),
#'   vco2 = c(2.3, 2.4, 2.2, 2.3)
#' )
#' calo <- CalorimetryData(data = df)
#' validation <- validate_calorimetry(calo)
#' print(validation)
#' }
validate_calorimetry <- function(calo,
                                  thresholds = NULL,
                                  check_outliers = TRUE,
                                  check_missing = TRUE,
                                  check_rer = TRUE,
                                  id_col = NULL,
                                  time_col = NULL,
                                  vo2_col = NULL,
                                  vco2_col = NULL,
                                  vo2_unit = "L/min") {

  # Use default thresholds if not provided
  if (is.null(thresholds)) {
    thresholds <- calorimetry_thresholds
  }

  # Extract data and column names
 if (S7_inherits(calo, CalorimetryData)) {
    data <- calo@data
    id_col <- calo@id_col
    time_col <- calo@time_col
    vo2_col <- calo@vo2_col
    vco2_col <- calo@vco2_col
    vo2_unit <- calo@vo2_unit
  } else if (is.data.frame(calo)) {
    data <- calo
    if (is.null(id_col) || is.null(time_col) || is.null(vo2_col) || is.null(vco2_col)) {
      cli::cli_abort("Must specify id_col, time_col, vo2_col, vco2_col for data frame input")
    }
  } else {
    cli::cli_abort("calo must be a CalorimetryData object or data frame")
  }

  # Convert units if needed
  unit_factor <- if (vo2_unit == "mL/min") 1/1000 else 1
  vo2_vals <- data[[vo2_col]] * unit_factor
  vco2_vals <- data[[vco2_col]] * unit_factor

  # Initialize issues
  issues <- empty_issues()
  n_total <- nrow(data)
  recommendations <- character()

  # ---------------------------------------------------------------------------
  # VO2 Range Checks
  # ---------------------------------------------------------------------------

  # Check for values below minimum
  below_min <- vo2_vals < thresholds$vo2$min & !is.na(vo2_vals)
  if (any(below_min)) {
    affected_rows <- which(below_min)
    issues <- add_issue(
      issues,
      check_id = "vo2_below_min",
      category = "calorimetry",
      variable = "vo2",
      severity = "error",
      message = glue::glue(
        "VO2 values below physiological minimum ({thresholds$vo2$min} L/min)"
      ),
      n_affected = sum(below_min),
      n_total = n_total,
      subject_ids = unique(data[[id_col]][affected_rows]),
      time_points = unique(data[[time_col]][affected_rows]),
      values = head(vo2_vals[affected_rows], 5),
      threshold = glue::glue(">= {thresholds$vo2$min} L/min"),
      reference = thresholds$vo2$reference
    )
    recommendations <- c(recommendations,
      "Review VO2 values below minimum - may indicate measurement errors or data entry issues")
  }

  # Check for values above maximum
  above_max <- vo2_vals > thresholds$vo2$max & !is.na(vo2_vals)
  if (any(above_max)) {
    affected_rows <- which(above_max)
    issues <- add_issue(
      issues,
      check_id = "vo2_above_max",
      category = "calorimetry",
      variable = "vo2",
      severity = "error",
      message = glue::glue(
        "VO2 values above physiological maximum ({thresholds$vo2$max} L/min)"
      ),
      n_affected = sum(above_max),
      n_total = n_total,
      subject_ids = unique(data[[id_col]][affected_rows]),
      time_points = unique(data[[time_col]][affected_rows]),
      values = head(vo2_vals[affected_rows], 5),
      threshold = glue::glue("<= {thresholds$vo2$max} L/min"),
      reference = thresholds$vo2$reference
    )
    recommendations <- c(recommendations,
      "Review VO2 values above maximum - check units and equipment calibration")
  }

  # Check for atypical VO2 values (warning)
  atypical_vo2 <- (vo2_vals < thresholds$vo2$typical_min | vo2_vals > thresholds$vo2$typical_max) &
                  vo2_vals >= thresholds$vo2$min & vo2_vals <= thresholds$vo2$max & !is.na(vo2_vals)
  if (any(atypical_vo2)) {
    affected_rows <- which(atypical_vo2)
    issues <- add_issue(
      issues,
      check_id = "vo2_atypical",
      category = "calorimetry",
      variable = "vo2",
      severity = "warning",
      message = glue::glue(
        "VO2 values outside typical exercise range ({thresholds$vo2$typical_min}-{thresholds$vo2$typical_max} L/min)"
      ),
      n_affected = sum(atypical_vo2),
      n_total = n_total,
      subject_ids = unique(data[[id_col]][affected_rows]),
      time_points = unique(data[[time_col]][affected_rows]),
      values = head(vo2_vals[affected_rows], 5),
      threshold = glue::glue("{thresholds$vo2$typical_min}-{thresholds$vo2$typical_max} L/min"),
      reference = thresholds$vo2$reference
    )
  }

  # ---------------------------------------------------------------------------
  # VCO2 Range Checks
  # ---------------------------------------------------------------------------

  # Check for values below minimum
  below_min <- vco2_vals < thresholds$vco2$min & !is.na(vco2_vals)
  if (any(below_min)) {
    affected_rows <- which(below_min)
    issues <- add_issue(
      issues,
      check_id = "vco2_below_min",
      category = "calorimetry",
      variable = "vco2",
      severity = "error",
      message = glue::glue(
        "VCO2 values below physiological minimum ({thresholds$vco2$min} L/min)"
      ),
      n_affected = sum(below_min),
      n_total = n_total,
      subject_ids = unique(data[[id_col]][affected_rows]),
      time_points = unique(data[[time_col]][affected_rows]),
      values = head(vco2_vals[affected_rows], 5),
      threshold = glue::glue(">= {thresholds$vco2$min} L/min"),
      reference = thresholds$vco2$reference
    )
  }

  # Check for values above maximum
  above_max <- vco2_vals > thresholds$vco2$max & !is.na(vco2_vals)
  if (any(above_max)) {
    affected_rows <- which(above_max)
    issues <- add_issue(
      issues,
      check_id = "vco2_above_max",
      category = "calorimetry",
      variable = "vco2",
      severity = "error",
      message = glue::glue(
        "VCO2 values above physiological maximum ({thresholds$vco2$max} L/min)"
      ),
      n_affected = sum(above_max),
      n_total = n_total,
      subject_ids = unique(data[[id_col]][affected_rows]),
      time_points = unique(data[[time_col]][affected_rows]),
      values = head(vco2_vals[affected_rows], 5),
      threshold = glue::glue("<= {thresholds$vco2$max} L/min"),
      reference = thresholds$vco2$reference
    )
  }

  # ---------------------------------------------------------------------------
  # RER Checks
  # ---------------------------------------------------------------------------

  if (check_rer) {
    rer_vals <- vco2_vals / vo2_vals

    # RER below minimum
    below_min <- rer_vals < thresholds$rer$min & !is.na(rer_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "rer_below_min",
        category = "calorimetry",
        variable = "rer",
        severity = "error",
        message = glue::glue(
          "RER values below physiological minimum ({thresholds$rer$min})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(round(rer_vals[affected_rows], 3), 5),
        threshold = glue::glue(">= {thresholds$rer$min}"),
        reference = thresholds$rer$reference
      )
      recommendations <- c(recommendations,
        "RER below 0.70 indicates measurement error - check gas analyzer calibration")
    }

    # RER above maximum
    above_max <- rer_vals > thresholds$rer$max & !is.na(rer_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "rer_above_max",
        category = "calorimetry",
        variable = "rer",
        severity = "error",
        message = glue::glue(
          "RER values above physiological maximum ({thresholds$rer$max})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(round(rer_vals[affected_rows], 3), 5),
        threshold = glue::glue("<= {thresholds$rer$max}"),
        reference = thresholds$rer$reference
      )
      recommendations <- c(recommendations,
        "RER above 1.10 indicates hyperventilation or measurement error")
    }

    # RER in warning range
    warning_low <- rer_vals < thresholds$rer$warning_low & rer_vals >= thresholds$rer$min & !is.na(rer_vals)
    if (any(warning_low)) {
      affected_rows <- which(warning_low)
      issues <- add_issue(
        issues,
        check_id = "rer_warning_low",
        category = "calorimetry",
        variable = "rer",
        severity = "warning",
        message = glue::glue(
          "RER values unusually low for exercise ({thresholds$rer$min}-{thresholds$rer$warning_low})"
        ),
        n_affected = sum(warning_low),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(round(rer_vals[affected_rows], 3), 5),
        threshold = glue::glue(">= {thresholds$rer$warning_low}"),
        reference = thresholds$rer$reference
      )
    }

    warning_high <- rer_vals > thresholds$rer$warning_high & rer_vals <= thresholds$rer$max & !is.na(rer_vals)
    if (any(warning_high)) {
      affected_rows <- which(warning_high)
      issues <- add_issue(
        issues,
        check_id = "rer_warning_high",
        category = "calorimetry",
        variable = "rer",
        severity = "warning",
        message = glue::glue(
          "RER values approaching anaerobic threshold ({thresholds$rer$warning_high}-{thresholds$rer$max})"
        ),
        n_affected = sum(warning_high),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(round(rer_vals[affected_rows], 3), 5),
        threshold = glue::glue("<= {thresholds$rer$warning_high}"),
        reference = thresholds$rer$reference
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Outlier Detection
  # ---------------------------------------------------------------------------

  if (check_outliers) {
    # VO2 outliers
    vo2_outliers <- detect_outliers_iqr(vo2_vals)
    if (any(vo2_outliers)) {
      affected_rows <- which(vo2_outliers)
      issues <- add_issue(
        issues,
        check_id = "vo2_outliers",
        category = "calorimetry",
        variable = "vo2",
        severity = "warning",
        message = "Statistical outliers detected in VO2 (IQR method)",
        n_affected = sum(vo2_outliers),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(vo2_vals[affected_rows], 5),
        threshold = "1.5 * IQR",
        reference = "Tukey, 1977"
      )
    }

    # VCO2 outliers
    vco2_outliers <- detect_outliers_iqr(vco2_vals)
    if (any(vco2_outliers)) {
      affected_rows <- which(vco2_outliers)
      issues <- add_issue(
        issues,
        check_id = "vco2_outliers",
        category = "calorimetry",
        variable = "vco2",
        severity = "warning",
        message = "Statistical outliers detected in VCO2 (IQR method)",
        n_affected = sum(vco2_outliers),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        values = head(vco2_vals[affected_rows], 5),
        threshold = "1.5 * IQR",
        reference = "Tukey, 1977"
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Missing Data Checks
  # ---------------------------------------------------------------------------

  if (check_missing) {
    # VO2 missing
    vo2_missing <- is.na(data[[vo2_col]])
    if (any(vo2_missing)) {
      affected_rows <- which(vo2_missing)
      severity <- if (sum(vo2_missing) / n_total > 0.1) "warning" else "info"
      issues <- add_issue(
        issues,
        check_id = "vo2_missing",
        category = "calorimetry",
        variable = "vo2",
        severity = severity,
        message = "Missing VO2 values detected",
        n_affected = sum(vo2_missing),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        threshold = "0% missing",
        reference = ""
      )
      if (sum(vo2_missing) / n_total > 0.1) {
        recommendations <- c(recommendations,
          "Consider investigating cause of missing VO2 data (>10% missing)")
      }
    }

    # VCO2 missing
    vco2_missing <- is.na(data[[vco2_col]])
    if (any(vco2_missing)) {
      affected_rows <- which(vco2_missing)
      severity <- if (sum(vco2_missing) / n_total > 0.1) "warning" else "info"
      issues <- add_issue(
        issues,
        check_id = "vco2_missing",
        category = "calorimetry",
        variable = "vco2",
        severity = severity,
        message = "Missing VCO2 values detected",
        n_affected = sum(vco2_missing),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        time_points = unique(data[[time_col]][affected_rows]),
        threshold = "0% missing",
        reference = ""
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Create ValidationResult
  # ---------------------------------------------------------------------------

  # Calculate severity summary
  severity_summary <- list(
    error = sum(issues$severity == "error"),
    warning = sum(issues$severity == "warning"),
    info = sum(issues$severity == "info")
  )

  # Determine if passed (no errors)
  passed <- severity_summary$error == 0

  # Data completeness summary
  data_summary <- calc_completeness(data, c(vo2_col, vco2_col))

  # Unique recommendations
  recommendations <- unique(recommendations)

  ValidationResult(
    issues = issues,
    passed = passed,
    severity_summary = severity_summary,
    data_summary = data_summary,
    recommendations = recommendations,
    timestamp = Sys.time()
  )
}

# -----------------------------------------------------------------------------
# Validate Isotopes
# -----------------------------------------------------------------------------

#' Validate Isotope Data
#'
#' Check 13C isotope enrichment data against physiological ranges and for
#' data quality issues.
#'
#' @param isotopes An IsotopeData object
#' @param thresholds Custom thresholds (NULL = use isotope_thresholds)
#' @param check_enrichment_order Logical; verify Rexp is between Rref and Rexo
#' @param check_missing Logical; check for missing data
#'
#' @return A ValidationResult S7 object
#' @export
validate_isotopes <- function(isotopes,
                               thresholds = NULL,
                               check_enrichment_order = TRUE,
                               check_missing = TRUE) {

  if (!S7_inherits(isotopes, IsotopeData)) {
    cli::cli_abort("isotopes must be an IsotopeData object")
  }

  if (is.null(thresholds)) {
    thresholds <- isotope_thresholds
  }

  issues <- empty_issues()
  recommendations <- character()

  # Extract data
  rexp_df <- isotopes@rexp
  rexo_df <- isotopes@rexo
  rref_df <- isotopes@rref

  rexp_col <- isotopes@rexp_col
  rexo_col <- isotopes@rexo_col
  rref_col <- isotopes@rref_col
  id_col <- isotopes@id_col
  time_col <- isotopes@time_col

  n_total <- nrow(rexp_df)

  # ---------------------------------------------------------------------------
  # Rexp Range Checks
  # ---------------------------------------------------------------------------

  if (rexp_col %in% names(rexp_df)) {
    rexp_vals <- rexp_df[[rexp_col]]

    # Below minimum
    below_min <- rexp_vals < thresholds$rexp$min & !is.na(rexp_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "rexp_below_min",
        category = "isotope",
        variable = "rexp",
        severity = "warning",
        message = glue::glue(
          "Rexp values below expected range ({thresholds$rexp$min} {thresholds$rexp$unit})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = if (id_col %in% names(rexp_df)) unique(rexp_df[[id_col]][affected_rows]) else NULL,
        time_points = if (time_col %in% names(rexp_df)) unique(rexp_df[[time_col]][affected_rows]) else NULL,
        values = head(rexp_vals[affected_rows], 5),
        threshold = glue::glue(">= {thresholds$rexp$min}"),
        reference = thresholds$rexp$reference
      )
    }

    # Above maximum
    above_max <- rexp_vals > thresholds$rexp$max & !is.na(rexp_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "rexp_above_max",
        category = "isotope",
        variable = "rexp",
        severity = "warning",
        message = glue::glue(
          "Rexp values above expected range ({thresholds$rexp$max} {thresholds$rexp$unit})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = if (id_col %in% names(rexp_df)) unique(rexp_df[[id_col]][affected_rows]) else NULL,
        time_points = if (time_col %in% names(rexp_df)) unique(rexp_df[[time_col]][affected_rows]) else NULL,
        values = head(rexp_vals[affected_rows], 5),
        threshold = glue::glue("<= {thresholds$rexp$max}"),
        reference = thresholds$rexp$reference
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Rref Range Checks
  # ---------------------------------------------------------------------------

  if (rref_col %in% names(rref_df)) {
    rref_vals <- rref_df[[rref_col]]

    # Below minimum
    below_min <- rref_vals < thresholds$rref$min & !is.na(rref_vals)
    if (any(below_min)) {
      issues <- add_issue(
        issues,
        check_id = "rref_below_min",
        category = "isotope",
        variable = "rref",
        severity = "warning",
        message = glue::glue(
          "Rref (baseline) values below expected range ({thresholds$rref$min} {thresholds$rref$unit})"
        ),
        n_affected = sum(below_min),
        n_total = nrow(rref_df),
        values = head(rref_vals[below_min], 5),
        threshold = glue::glue(">= {thresholds$rref$min}"),
        reference = thresholds$rref$reference
      )
    }

    # Above maximum
    above_max <- rref_vals > thresholds$rref$max & !is.na(rref_vals)
    if (any(above_max)) {
      issues <- add_issue(
        issues,
        check_id = "rref_above_max",
        category = "isotope",
        variable = "rref",
        severity = "warning",
        message = glue::glue(
          "Rref (baseline) values above expected range ({thresholds$rref$max} {thresholds$rref$unit})"
        ),
        n_affected = sum(above_max),
        n_total = nrow(rref_df),
        values = head(rref_vals[above_max], 5),
        threshold = glue::glue("<= {thresholds$rref$max}"),
        reference = thresholds$rref$reference
      )
      recommendations <- c(recommendations,
        "Elevated Rref suggests possible contamination or prior tracer exposure")
    }
  }

  # ---------------------------------------------------------------------------
  # Enrichment Order Check
  # ---------------------------------------------------------------------------

  if (check_enrichment_order && rexp_col %in% names(rexp_df) &&
      rexo_col %in% names(rexo_df) && rref_col %in% names(rref_df)) {

    # This is a simplified check - in practice would need to join data
    mean_rexp <- mean(rexp_df[[rexp_col]], na.rm = TRUE)
    mean_rexo <- mean(rexo_df[[rexo_col]], na.rm = TRUE)
    mean_rref <- mean(rref_df[[rref_col]], na.rm = TRUE)

    # Rexp should be between Rref and Rexo during tracer ingestion
    if (!is.na(mean_rexp) && !is.na(mean_rexo) && !is.na(mean_rref)) {
      if (mean_rexp > mean_rexo) {
        issues <- add_issue(
          issues,
          check_id = "enrichment_order_rexp_high",
          category = "isotope",
          variable = "rexp",
          severity = "error",
          message = "Mean Rexp exceeds Rexo - check data or tracer labeling",
          n_affected = n_total,
          n_total = n_total,
          values = c(rexp = mean_rexp, rexo = mean_rexo),
          threshold = "Rexp < Rexo",
          reference = "Mosora et al., 1976"
        )
        recommendations <- c(recommendations,
          "Rexp exceeding Rexo indicates data error - verify isotope measurements")
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Missing Data
  # ---------------------------------------------------------------------------

  if (check_missing && rexp_col %in% names(rexp_df)) {
    rexp_missing <- is.na(rexp_df[[rexp_col]])
    if (any(rexp_missing)) {
      severity <- if (sum(rexp_missing) / n_total > 0.1) "warning" else "info"
      issues <- add_issue(
        issues,
        check_id = "rexp_missing",
        category = "isotope",
        variable = "rexp",
        severity = severity,
        message = "Missing Rexp values detected",
        n_affected = sum(rexp_missing),
        n_total = n_total,
        threshold = "0% missing",
        reference = ""
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Create ValidationResult
  # ---------------------------------------------------------------------------

  severity_summary <- list(
    error = sum(issues$severity == "error"),
    warning = sum(issues$severity == "warning"),
    info = sum(issues$severity == "info")
  )

  passed <- severity_summary$error == 0
  data_summary <- calc_completeness(rexp_df, rexp_col)
  recommendations <- unique(recommendations)

  ValidationResult(
    issues = issues,
    passed = passed,
    severity_summary = severity_summary,
    data_summary = data_summary,
    recommendations = recommendations,
    timestamp = Sys.time()
  )
}

# -----------------------------------------------------------------------------
# Validate Environment
# -----------------------------------------------------------------------------

#' Validate Environment Data
#'
#' Check environmental conditions against expected ranges.
#'
#' @param environment An EnvironmentData object
#' @param thresholds Custom thresholds (NULL = use environment_thresholds)
#'
#' @return A ValidationResult S7 object
#' @export
validate_environment <- function(environment,
                                  thresholds = NULL) {

  if (!S7_inherits(environment, EnvironmentData)) {
    cli::cli_abort("environment must be an EnvironmentData object")
  }

  if (is.null(thresholds)) {
    thresholds <- environment_thresholds
  }

  issues <- empty_issues()
  recommendations <- character()

  data <- environment@data
  id_col <- environment@id_col
  temp_col <- environment@temp_col
  humidity_col <- environment@humidity_col
  pressure_col <- environment@pressure_col

  n_total <- nrow(data)

  # ---------------------------------------------------------------------------
  # Temperature Checks
  # ---------------------------------------------------------------------------

  if (temp_col %in% names(data)) {
    temp_vals <- data[[temp_col]]

    # Below minimum
    below_min <- temp_vals < thresholds$temperature$min & !is.na(temp_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "temp_below_min",
        category = "environment",
        variable = "temperature",
        severity = "warning",
        message = glue::glue(
          "Temperature below expected range ({thresholds$temperature$min} {thresholds$temperature$unit})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(temp_vals[affected_rows], 5),
        threshold = glue::glue(">= {thresholds$temperature$min}"),
        reference = thresholds$temperature$reference
      )
    }

    # Above maximum
    above_max <- temp_vals > thresholds$temperature$max & !is.na(temp_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "temp_above_max",
        category = "environment",
        variable = "temperature",
        severity = "warning",
        message = glue::glue(
          "Temperature above expected range ({thresholds$temperature$max} {thresholds$temperature$unit})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(temp_vals[affected_rows], 5),
        threshold = glue::glue("<= {thresholds$temperature$max}"),
        reference = thresholds$temperature$reference
      )
      recommendations <- c(recommendations,
        "High ambient temperature may affect thermoregulation and substrate oxidation")
    }
  }

  # ---------------------------------------------------------------------------
  # Humidity Checks
  # ---------------------------------------------------------------------------

  if (humidity_col %in% names(data)) {
    humidity_vals <- data[[humidity_col]]

    # Below minimum (0%)
    below_min <- humidity_vals < thresholds$humidity$min & !is.na(humidity_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "humidity_below_min",
        category = "environment",
        variable = "humidity",
        severity = "error",
        message = "Humidity values below 0% (impossible)",
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(humidity_vals[affected_rows], 5),
        threshold = ">= 0%",
        reference = ""
      )
    }

    # Above maximum (100%)
    above_max <- humidity_vals > thresholds$humidity$max & !is.na(humidity_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "humidity_above_max",
        category = "environment",
        variable = "humidity",
        severity = "error",
        message = "Humidity values above 100% (impossible)",
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(humidity_vals[affected_rows], 5),
        threshold = "<= 100%",
        reference = ""
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Pressure Checks
  # ---------------------------------------------------------------------------

  if (pressure_col %in% names(data)) {
    pressure_vals <- data[[pressure_col]]

    # Below minimum
    below_min <- pressure_vals < thresholds$pressure$min & !is.na(pressure_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "pressure_below_min",
        category = "environment",
        variable = "pressure",
        severity = "warning",
        message = glue::glue(
          "Atmospheric pressure below expected range ({thresholds$pressure$min} {thresholds$pressure$unit})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(pressure_vals[affected_rows], 5),
        threshold = glue::glue(">= {thresholds$pressure$min}"),
        reference = thresholds$pressure$reference
      )
      recommendations <- c(recommendations,
        "Low pressure may indicate high altitude - consider altitude corrections")
    }

    # Above maximum
    above_max <- pressure_vals > thresholds$pressure$max & !is.na(pressure_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "pressure_above_max",
        category = "environment",
        variable = "pressure",
        severity = "warning",
        message = glue::glue(
          "Atmospheric pressure above expected range ({thresholds$pressure$max} {thresholds$pressure$unit})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(pressure_vals[affected_rows], 5),
        threshold = glue::glue("<= {thresholds$pressure$max}"),
        reference = thresholds$pressure$reference
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Create ValidationResult
  # ---------------------------------------------------------------------------

  severity_summary <- list(
    error = sum(issues$severity == "error"),
    warning = sum(issues$severity == "warning"),
    info = sum(issues$severity == "info")
  )

  passed <- severity_summary$error == 0
  data_summary <- calc_completeness(data, c(temp_col, humidity_col, pressure_col))
  recommendations <- unique(recommendations)

  ValidationResult(
    issues = issues,
    passed = passed,
    severity_summary = severity_summary,
    data_summary = data_summary,
    recommendations = recommendations,
    timestamp = Sys.time()
  )
}

# -----------------------------------------------------------------------------
# Validate Urea
# -----------------------------------------------------------------------------

#' Validate Urea Data
#'
#' Check urea concentration data against physiological ranges.
#'
#' @param urea A UreaData object
#' @param thresholds Custom thresholds (NULL = use urea_thresholds)
#'
#' @return A ValidationResult S7 object
#' @export
validate_urea <- function(urea,
                          thresholds = NULL) {

  if (!S7_inherits(urea, UreaData)) {
    cli::cli_abort("urea must be a UreaData object")
  }

  if (is.null(thresholds)) {
    thresholds <- urea_thresholds
  }

  issues <- empty_issues()
  recommendations <- character()

  data <- urea@data
  id_col <- urea@id_col
  sweat_col <- urea@sweat_urea_col
  urine_col <- urea@urine_urea_col

  n_total <- nrow(data)

  # ---------------------------------------------------------------------------
  # Sweat Urea Checks
  # ---------------------------------------------------------------------------

  if (sweat_col %in% names(data)) {
    sweat_vals <- data[[sweat_col]]

    # Below minimum
    below_min <- sweat_vals < thresholds$sweat_urea$min & !is.na(sweat_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "sweat_urea_below_min",
        category = "urea",
        variable = "sweat_urea",
        severity = "warning",
        message = glue::glue(
          "Sweat urea below expected range ({thresholds$sweat_urea$min} {thresholds$sweat_urea$unit})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(sweat_vals[affected_rows], 5),
        threshold = glue::glue(">= {thresholds$sweat_urea$min}"),
        reference = thresholds$sweat_urea$reference
      )
    }

    # Above maximum
    above_max <- sweat_vals > thresholds$sweat_urea$max & !is.na(sweat_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "sweat_urea_above_max",
        category = "urea",
        variable = "sweat_urea",
        severity = "warning",
        message = glue::glue(
          "Sweat urea above expected range ({thresholds$sweat_urea$max} {thresholds$sweat_urea$unit})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(sweat_vals[affected_rows], 5),
        threshold = glue::glue("<= {thresholds$sweat_urea$max}"),
        reference = thresholds$sweat_urea$reference
      )
      recommendations <- c(recommendations,
        "Elevated sweat urea may indicate sample contamination or dehydration")
    }
  }

  # ---------------------------------------------------------------------------
  # Urine Urea Checks
  # ---------------------------------------------------------------------------

  if (urine_col %in% names(data)) {
    urine_vals <- data[[urine_col]]

    # Below minimum
    below_min <- urine_vals < thresholds$urine_urea$min & !is.na(urine_vals)
    if (any(below_min)) {
      affected_rows <- which(below_min)
      issues <- add_issue(
        issues,
        check_id = "urine_urea_below_min",
        category = "urea",
        variable = "urine_urea",
        severity = "warning",
        message = glue::glue(
          "Urine urea below expected range ({thresholds$urine_urea$min} {thresholds$urine_urea$unit})"
        ),
        n_affected = sum(below_min),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(urine_vals[affected_rows], 5),
        threshold = glue::glue(">= {thresholds$urine_urea$min}"),
        reference = thresholds$urine_urea$reference
      )
    }

    # Above maximum
    above_max <- urine_vals > thresholds$urine_urea$max & !is.na(urine_vals)
    if (any(above_max)) {
      affected_rows <- which(above_max)
      issues <- add_issue(
        issues,
        check_id = "urine_urea_above_max",
        category = "urea",
        variable = "urine_urea",
        severity = "warning",
        message = glue::glue(
          "Urine urea above expected range ({thresholds$urine_urea$max} {thresholds$urine_urea$unit})"
        ),
        n_affected = sum(above_max),
        n_total = n_total,
        subject_ids = unique(data[[id_col]][affected_rows]),
        values = head(urine_vals[affected_rows], 5),
        threshold = glue::glue("<= {thresholds$urine_urea$max}"),
        reference = thresholds$urine_urea$reference
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Create ValidationResult
  # ---------------------------------------------------------------------------

  severity_summary <- list(
    error = sum(issues$severity == "error"),
    warning = sum(issues$severity == "warning"),
    info = sum(issues$severity == "info")
  )

  passed <- severity_summary$error == 0
  data_summary <- calc_completeness(data, c(sweat_col, urine_col))
  recommendations <- unique(recommendations)

  ValidationResult(
    issues = issues,
    passed = passed,
    severity_summary = severity_summary,
    data_summary = data_summary,
    recommendations = recommendations,
    timestamp = Sys.time()
  )
}

# -----------------------------------------------------------------------------
# Check Consistency
# -----------------------------------------------------------------------------

#' Check Cross-Dataset Consistency
#'
#' Verify consistency across different data components in an OxidationStudy.
#'
#' @param study An OxidationStudy object
#'
#' @return A tibble of consistency issues
#' @keywords internal
check_consistency <- function(study) {

  if (!S7_inherits(study, OxidationStudy)) {
    cli::cli_abort("study must be an OxidationStudy object")
  }

  issues <- empty_issues()

  calo <- study@calorimetry
  calo_ids <- unique(calo@data[[calo@id_col]])
  calo_times <- unique(calo@data[[calo@time_col]])

  # ---------------------------------------------------------------------------
  # Check Isotope Data Consistency
  # ---------------------------------------------------------------------------

  if (!is.null(study@isotopes)) {
    iso <- study@isotopes
    iso_ids <- unique(iso@rexp[[iso@id_col]])

    # Check subject IDs match
    missing_in_iso <- setdiff(calo_ids, iso_ids)
    if (length(missing_in_iso) > 0) {
      issues <- add_issue(
        issues,
        check_id = "iso_missing_subjects",
        category = "consistency",
        variable = "id",
        severity = "warning",
        message = glue::glue(
          "{length(missing_in_iso)} subject(s) in calorimetry but not in isotope data"
        ),
        n_affected = length(missing_in_iso),
        n_total = length(calo_ids),
        subject_ids = missing_in_iso,
        threshold = "All subjects present",
        reference = ""
      )
    }

    extra_in_iso <- setdiff(iso_ids, calo_ids)
    if (length(extra_in_iso) > 0) {
      issues <- add_issue(
        issues,
        check_id = "iso_extra_subjects",
        category = "consistency",
        variable = "id",
        severity = "info",
        message = glue::glue(
          "{length(extra_in_iso)} subject(s) in isotope data but not in calorimetry"
        ),
        n_affected = length(extra_in_iso),
        n_total = length(iso_ids),
        subject_ids = extra_in_iso,
        threshold = "All subjects matched",
        reference = ""
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Check Urea Data Consistency
  # ---------------------------------------------------------------------------

  if (!is.null(study@urea)) {
    urea <- study@urea
    urea_ids <- unique(urea@data[[urea@id_col]])

    missing_in_urea <- setdiff(calo_ids, urea_ids)
    if (length(missing_in_urea) > 0) {
      issues <- add_issue(
        issues,
        check_id = "urea_missing_subjects",
        category = "consistency",
        variable = "id",
        severity = "warning",
        message = glue::glue(
          "{length(missing_in_urea)} subject(s) in calorimetry but not in urea data"
        ),
        n_affected = length(missing_in_urea),
        n_total = length(calo_ids),
        subject_ids = missing_in_urea,
        threshold = "All subjects present",
        reference = ""
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Check Environment Data Consistency
  # ---------------------------------------------------------------------------

  if (!is.null(study@environment)) {
    env <- study@environment
    env_ids <- unique(env@data[[env@id_col]])

    missing_in_env <- setdiff(calo_ids, env_ids)
    if (length(missing_in_env) > 0) {
      issues <- add_issue(
        issues,
        check_id = "env_missing_subjects",
        category = "consistency",
        variable = "id",
        severity = "warning",
        message = glue::glue(
          "{length(missing_in_env)} subject(s) in calorimetry but not in environment data"
        ),
        n_affected = length(missing_in_env),
        n_total = length(calo_ids),
        subject_ids = missing_in_env,
        threshold = "All subjects present",
        reference = ""
      )
    }
  }

  issues
}

# -----------------------------------------------------------------------------
# Validate Study (Main Entry Point)
# -----------------------------------------------------------------------------

#' Validate Oxidation Study
#'
#' Comprehensive validation of an OxidationStudy object including all
#' data components and cross-dataset consistency.
#'
#' @param study An OxidationStudy object
#' @param strict Logical; if TRUE, errors halt execution; if FALSE, issues are warnings
#' @param thresholds Named list of custom thresholds for each component (NULL = use defaults)
#' @param checks Character vector of checks to run (NULL = all available)
#' @param verbose Logical; print progress messages
#'
#' @return A ValidationResult S7 object
#' @export
#'
#' @examples
#' \dontrun{
#' study <- oxidation_study(calorimetry = calo_data, isotopes = iso_data)
#' validation <- validate_study(study)
#' print(validation)
#'
#' # With strict mode
#' validation <- validate_study(study, strict = TRUE)
#' }
validate_study <- function(study,
                            strict = FALSE,
                            thresholds = NULL,
                            checks = NULL,
                            verbose = TRUE) {

  if (!S7_inherits(study, OxidationStudy)) {
    cli::cli_abort("study must be an OxidationStudy object")
  }

  # Default checks
  available_checks <- c("calorimetry", "isotopes", "environment", "urea", "consistency")
  if (is.null(checks)) {
    checks <- available_checks
  } else {
    invalid_checks <- setdiff(checks, available_checks)
    if (length(invalid_checks) > 0) {
      cli::cli_warn("Unknown checks ignored: {paste(invalid_checks, collapse = ', ')}")
    }
    checks <- intersect(checks, available_checks)
  }

  all_issues <- empty_issues()
  all_recommendations <- character()
  all_data_summary <- NULL

  # ---------------------------------------------------------------------------
  # Validate Calorimetry (always present)
  # ---------------------------------------------------------------------------

  if ("calorimetry" %in% checks) {
    if (verbose) cli::cli_alert_info("Validating calorimetry data...")

    calo_thresholds <- if (!is.null(thresholds$calorimetry)) {
      thresholds$calorimetry
    } else {
      calorimetry_thresholds
    }

    calo_result <- validate_calorimetry(
      study@calorimetry,
      thresholds = calo_thresholds
    )

    all_issues <- dplyr::bind_rows(all_issues, calo_result@issues)
    all_recommendations <- c(all_recommendations, calo_result@recommendations)
    all_data_summary <- dplyr::bind_rows(all_data_summary, calo_result@data_summary)
  }

  # ---------------------------------------------------------------------------
  # Validate Isotopes (if present)
  # ---------------------------------------------------------------------------

  if ("isotopes" %in% checks && !is.null(study@isotopes)) {
    if (verbose) cli::cli_alert_info("Validating isotope data...")

    iso_thresholds <- if (!is.null(thresholds$isotopes)) {
      thresholds$isotopes
    } else {
      isotope_thresholds
    }

    iso_result <- validate_isotopes(
      study@isotopes,
      thresholds = iso_thresholds
    )

    all_issues <- dplyr::bind_rows(all_issues, iso_result@issues)
    all_recommendations <- c(all_recommendations, iso_result@recommendations)
    all_data_summary <- dplyr::bind_rows(all_data_summary, iso_result@data_summary)
  }

  # ---------------------------------------------------------------------------
  # Validate Environment (if present)
  # ---------------------------------------------------------------------------

  if ("environment" %in% checks && !is.null(study@environment)) {
    if (verbose) cli::cli_alert_info("Validating environment data...")

    env_thresholds <- if (!is.null(thresholds$environment)) {
      thresholds$environment
    } else {
      environment_thresholds
    }

    env_result <- validate_environment(
      study@environment,
      thresholds = env_thresholds
    )

    all_issues <- dplyr::bind_rows(all_issues, env_result@issues)
    all_recommendations <- c(all_recommendations, env_result@recommendations)
    all_data_summary <- dplyr::bind_rows(all_data_summary, env_result@data_summary)
  }

  # ---------------------------------------------------------------------------
  # Validate Urea (if present)
  # ---------------------------------------------------------------------------

  if ("urea" %in% checks && !is.null(study@urea)) {
    if (verbose) cli::cli_alert_info("Validating urea data...")

    urea_thresh <- if (!is.null(thresholds$urea)) {
      thresholds$urea
    } else {
      urea_thresholds
    }

    urea_result <- validate_urea(
      study@urea,
      thresholds = urea_thresh
    )

    all_issues <- dplyr::bind_rows(all_issues, urea_result@issues)
    all_recommendations <- c(all_recommendations, urea_result@recommendations)
    all_data_summary <- dplyr::bind_rows(all_data_summary, urea_result@data_summary)
  }

  # ---------------------------------------------------------------------------
  # Check Consistency
  # ---------------------------------------------------------------------------

  if ("consistency" %in% checks) {
    if (verbose) cli::cli_alert_info("Checking cross-dataset consistency...")

    consistency_issues <- check_consistency(study)
    all_issues <- dplyr::bind_rows(all_issues, consistency_issues)
  }

  # ---------------------------------------------------------------------------
  # Create Final ValidationResult
  # ---------------------------------------------------------------------------

  severity_summary <- list(
    error = sum(all_issues$severity == "error"),
    warning = sum(all_issues$severity == "warning"),
    info = sum(all_issues$severity == "info")
  )

  passed <- severity_summary$error == 0
  all_recommendations <- unique(all_recommendations)

  result <- ValidationResult(
    issues = all_issues,
    passed = passed,
    severity_summary = severity_summary,
    data_summary = all_data_summary,
    recommendations = all_recommendations,
    timestamp = Sys.time()
  )

  # Handle strict mode
  if (strict && !passed) {
    cli::cli_abort(c(
      "Validation failed with {severity_summary$error} error(s)",
      "i" = "Set strict = FALSE to continue with warnings",
      "i" = "Errors: {paste(all_issues$message[all_issues$severity == 'error'], collapse = '; ')}"
    ))
  }

  if (verbose) {
    if (passed) {
      cli::cli_alert_success("Validation complete: PASSED")
    } else {
      cli::cli_alert_warning(
        "Validation complete: {severity_summary$error} error(s), {severity_summary$warning} warning(s)"
      )
    }
  }

  result
}
