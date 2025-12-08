#' S7 Classes for Oxidation Analysis
#'
#' @name oxidizr-classes
#' @description S7 class definitions for managing oxidation analysis data
NULL

# -----------------------------------------------------------------------------
# CalorimetryData Class
# -----------------------------------------------------------------------------

#' Calorimetry Data Class
#'
#' S7 class for storing indirect calorimetry measurements (VO2, VCO2, RER).
#'
#' @param data A data frame containing calorimetry measurements
#' @param id_col Name of the subject ID column (default: "id")
#' @param time_col Name of the time column (default: "time")
#' @param vo2_col Name of the VO2 column (default: "vo2")
#' @param vco2_col Name of the VCO2 column (default: "vco2")
#' @param vo2_unit Unit for VO2/VCO2: "L/min" or "mL/min" (default: "L/min")
#' @param protocol_col Name of the protocol/condition column (default: NULL)
#'
#' @return A CalorimetryData S7 object
#' @usage NULL
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   time = c(30, 60, 30, 60),
#'   vo2 = c(2.5, 2.6, 2.4, 2.5),
#'   vco2 = c(2.3, 2.4, 2.2, 2.3)
#' )
#' calo <- CalorimetryData(data = df)
CalorimetryData <- S7::new_class(
  "CalorimetryData",
  properties = list(
    data = S7::class_data.frame,
    id_col = S7::new_property(S7::class_character, default = "id"),
    time_col = S7::new_property(S7::class_character, default = "time"),
    vo2_col = S7::new_property(S7::class_character, default = "vo2"),
    vco2_col = S7::new_property(S7::class_character, default = "vco2"),
    vo2_unit = S7::new_property(S7::class_character, default = "L/min"),
    protocol_col = S7::new_property(
      S7::new_union(S7::class_character, NULL),
      default = NULL
    )
  ),
  validator = function(self) {
    errors <- character()

    # Check required columns exist (only if data is populated)
    if (ncol(self@data) > 0) {
      required_cols <- c(self@id_col, self@time_col, self@vo2_col, self@vco2_col)
      missing_cols <- setdiff(required_cols, names(self@data))
      if (length(missing_cols) > 0) {
        errors <- c(errors, glue::glue(
          "Missing required columns: {paste(missing_cols, collapse = ', ')}"
        ))
      }

      # Check protocol column if specified
      if (length(self@protocol_col) > 0 && !self@protocol_col %in% names(self@data)) {
        errors <- c(errors, glue::glue(
          "Protocol column '{self@protocol_col}' not found in data"
        ))
      }

      # Check for non-negative values (if columns exist)
      if (self@vo2_col %in% names(self@data)) {
        if (any(self@data[[self@vo2_col]] < 0, na.rm = TRUE)) {
          errors <- c(errors, "VO2 values must be non-negative")
        }
      }
      if (self@vco2_col %in% names(self@data)) {
        if (any(self@data[[self@vco2_col]] < 0, na.rm = TRUE)) {
          errors <- c(errors, "VCO2 values must be non-negative")
        }
      }
    }

    # Check for valid units
    valid_units <- c("L/min", "mL/min")
    if (!self@vo2_unit %in% valid_units) {
      errors <- c(errors, glue::glue(
        "@vo2_unit must be one of: {paste(valid_units, collapse = ', ')}"
      ))
    }

    if (length(errors) > 0) errors else NULL
  }
)

# -----------------------------------------------------------------------------
# IsotopeData Class
# -----------------------------------------------------------------------------

#' Isotope Data Class
#'
#' S7 class for storing 13C isotope enrichment data for tracer studies.
#'
#' @param rexp Data frame with expired CO2 13C enrichment (delta per mil)
#' @param rexo Data frame with exogenous substrate 13C enrichment
#' @param rref Data frame with reference/baseline 13C enrichment
#' @param rpla Data frame with plasma glucose 13C enrichment (optional)
#' @param id_col Name of the subject ID column
#' @param time_col Name of the time column
#' @param protocol_col Name of the protocol column (optional)
#' @param rexp_col Name of the rexp column in rexp data frame (default: "rexp")
#' @param rexo_col Name of the rexo column in rexo data frame (default: "rexo")
#' @param rref_col Name of the rref column in rref data frame (default: "rref")
#' @param rpla_col Name of the rpla column in rpla data frame (default: "rpla")
#'
#' @return An IsotopeData S7 object
#' @usage NULL
#' @export
IsotopeData <- S7::new_class(
  "IsotopeData",
  properties = list(
    rexp = S7::class_data.frame,
    rexo = S7::class_data.frame,
    rref = S7::class_data.frame,
    rpla = S7::new_property(
      S7::new_union(S7::class_data.frame, NULL),
      default = NULL
    ),
    id_col = S7::new_property(S7::class_character, default = "id"),
    time_col = S7::new_property(S7::class_character, default = "time"),
    protocol_col = S7::new_property(
      S7::new_union(S7::class_character, NULL),
      default = NULL
    ),
    rexp_col = S7::new_property(S7::class_character, default = "rexp"),
    rexo_col = S7::new_property(S7::class_character, default = "rexo"),
    rref_col = S7::new_property(S7::class_character, default = "rref"),
    rpla_col = S7::new_property(S7::class_character, default = "rpla")
  ),
  validator = function(self) {
    errors <- character()

    # Check rexp has required columns
    if (ncol(self@rexp) > 0 && !self@rexp_col %in% names(self@rexp)) {
      errors <- c(errors, glue::glue(
        "Column '{self@rexp_col}' not found in rexp data"
      ))
    }

    # Check rexo has required column
    if (ncol(self@rexo) > 0 && !self@rexo_col %in% names(self@rexo)) {
      errors <- c(errors, glue::glue(
        "Column '{self@rexo_col}' not found in rexo data"
      ))
    }

    # Check rref has required column
    if (ncol(self@rref) > 0 && !self@rref_col %in% names(self@rref)) {
      errors <- c(errors, glue::glue(
        "Column '{self@rref_col}' not found in rref data"
      ))
    }

    # Check rpla if provided
    if (!is.null(self@rpla) && ncol(self@rpla) > 0 && !self@rpla_col %in% names(self@rpla)) {
      errors <- c(errors, glue::glue(
        "Column '{self@rpla_col}' not found in rpla data"
      ))
    }

    if (length(errors) > 0) errors else NULL
  }
)

# -----------------------------------------------------------------------------
# UreaData Class
# -----------------------------------------------------------------------------

#' Urea Data Class
#'
#' S7 class for storing urea concentration data for protein oxidation estimation.
#'
#' @param data Data frame with urea concentrations
#' @param id_col Name of the subject ID column
#' @param sweat_urea_col Name of the sweat urea concentration column (mmol/L)
#' @param urine_urea_col Name of the urine urea concentration column (mmol/L)
#' @param urine_vol_col Name of the urine volume column (mL)
#'
#' @return A UreaData S7 object
#' @usage NULL
#' @export
UreaData <- S7::new_class(
  "UreaData",
  properties = list(
    data = S7::class_data.frame,
    id_col = S7::new_property(S7::class_character, default = "id"),
    sweat_urea_col = S7::new_property(S7::class_character, default = "sweat_urea"),
    urine_urea_col = S7::new_property(S7::class_character, default = "urine_urea"),
    urine_vol_col = S7::new_property(S7::class_character, default = "urine_vol")
  ),
  validator = function(self) {
    errors <- character()

    if (ncol(self@data) > 0) {
      required_cols <- c(
        self@id_col,
        self@sweat_urea_col,
        self@urine_urea_col
      )
      missing_cols <- setdiff(required_cols, names(self@data))
      if (length(missing_cols) > 0) {
        errors <- c(errors, glue::glue(
          "Missing required columns: {paste(missing_cols, collapse = ', ')}"
        ))
      }
    }

    if (length(errors) > 0) errors else NULL
  }
)

# -----------------------------------------------------------------------------
# EnvironmentData Class
# -----------------------------------------------------------------------------

#' Environment Data Class
#'
#' S7 class for storing environmental conditions during testing.
#'
#' @param data Data frame with environmental measurements
#' @param id_col Name of the subject ID column
#' @param temp_col Name of the temperature column (Celsius)
#' @param humidity_col Name of the relative humidity column (%)
#' @param pressure_col Name of the atmospheric pressure column (mmHg)
#'
#' @return An EnvironmentData S7 object
#' @usage NULL
#' @export
EnvironmentData <- S7::new_class(
  "EnvironmentData",
  properties = list(
    data = S7::class_data.frame,
    id_col = S7::new_property(S7::class_character, default = "id"),
    temp_col = S7::new_property(S7::class_character, default = "temperature"),
    humidity_col = S7::new_property(S7::class_character, default = "humidity"),
    pressure_col = S7::new_property(S7::class_character, default = "pressure")
  ),
  validator = function(self) {
    errors <- character()

    if (ncol(self@data) > 0) {
      required_cols <- c(
        self@id_col,
        self@temp_col,
        self@humidity_col,
        self@pressure_col
      )
      missing_cols <- setdiff(required_cols, names(self@data))
      if (length(missing_cols) > 0) {
        errors <- c(errors, glue::glue(
          "Missing required columns: {paste(missing_cols, collapse = ', ')}"
        ))
      }
    }

    if (length(errors) > 0) errors else NULL
  }
)

# -----------------------------------------------------------------------------
# SubjectData Class
# -----------------------------------------------------------------------------

#' Subject Data Class
#'
#' S7 class for storing subject characteristics and daily measurements.
#'
#' @param data Data frame with subject information
#' @param id_col Name of the subject ID column
#' @param protocol_col Name of the protocol/condition column (optional)
#' @param mass_cols Named list of mass columns (initial, final)
#' @param fluid_cols Named list of fluid columns (water, saline, urine)
#'
#' @return A SubjectData S7 object
#' @usage NULL
#' @export
SubjectData <- S7::new_class(
  "SubjectData",
  properties = list(
    data = S7::class_data.frame,
    id_col = S7::new_property(S7::class_character, default = "id"),
    protocol_col = S7::new_property(
      S7::new_union(S7::class_character, NULL),
      default = NULL
    ),
    mass_cols = S7::new_property(
      S7::class_list,
      default = list(initial = "initial_body_mass", final = "final_body_mass")
    ),
    fluid_cols = S7::new_property(
      S7::class_list,
      default = list(water = "water_ingested", saline = "saline", urine = "urine")
    )
  )
)

# -----------------------------------------------------------------------------
# OxidationStudy Class
# -----------------------------------------------------------------------------

#' Oxidation Study Class
#'
#' Main container S7 class that holds all data components for an oxidation study.
#'
#' @param calorimetry A CalorimetryData object
#' @param isotopes An IsotopeData object (optional, default NULL)
#' @param urea A UreaData object (optional, default NULL)
#' @param environment An EnvironmentData object (optional, default NULL)
#' @param subjects A SubjectData object (optional, default NULL)
#' @param protocols Factor of protocol/condition names (optional, default NULL)
#'
#' @return An OxidationStudy S7 object
#' @usage NULL
#' @export
OxidationStudy <- S7::new_class(
  "OxidationStudy",
  properties = list(
    calorimetry = CalorimetryData,
    isotopes = S7::new_property(
      S7::new_union(IsotopeData, NULL),
      default = NULL
    ),
    urea = S7::new_property(
      S7::new_union(UreaData, NULL),
      default = NULL
    ),
    environment = S7::new_property(
      S7::new_union(EnvironmentData, NULL),
      default = NULL
    ),
    subjects = S7::new_property(
      S7::new_union(SubjectData, NULL),
      default = NULL
    ),
    protocols = S7::new_property(
      S7::new_union(S7::class_factor, NULL),
      default = NULL
    )
  )
)

# -----------------------------------------------------------------------------
# OxidationResults Class
# -----------------------------------------------------------------------------

#' Oxidation Results Class
#'
#' S7 class for storing the results of oxidation analysis.
#'
#' @param oxidation_rates Data frame with oxidation rates (g/min)
#' @param energy_contributions Data frame with energy contributions (kcal and %)
#' @param partition Data frame with CHO partitioning (exo/endo/mus/liv)
#' @param study The original OxidationStudy object
#' @param settings List of analysis settings used
#'
#' @return An OxidationResults S7 object
#' @usage NULL
#' @export
OxidationResults <- S7::new_class(
  "OxidationResults",
  properties = list(
    oxidation_rates = S7::class_data.frame,
    energy_contributions = S7::new_property(
      S7::new_union(S7::class_data.frame, NULL),
      default = NULL
    ),
    partition = S7::new_property(
      S7::new_union(S7::class_data.frame, NULL),
      default = NULL
    ),
    study = S7::new_property(
      S7::new_union(OxidationStudy, NULL),
      default = NULL
    ),
    settings = S7::new_property(
      S7::class_list,
      default = list()
    )
  )
)

# -----------------------------------------------------------------------------
# ValidationResult Class
# -----------------------------------------------------------------------------

#' Validation Result Class
#'
#' S7 class for storing data validation findings from quality checks.
#'
#' @param issues A tibble with validation issues found. Columns include:
#'   check_id, category, variable, severity, message, n_affected, pct_affected,
#'   subject_ids, time_points, values, threshold, reference.
#' @param passed Logical indicating if all critical checks passed (no errors)
#' @param severity_summary Named list with counts by severity level (error, warning, info)
#' @param data_summary Tibble with data completeness metrics by variable
#' @param recommendations Character vector of prioritized recommendations
#' @param timestamp POSIXct timestamp when validation was performed
#'
#' @return A ValidationResult S7 object
#' @usage NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # Typically created by validate_study() or validate_calorimetry()
#' validation <- validate_study(study)
#' print(validation)
#' }
ValidationResult <- S7::new_class(
  "ValidationResult",
  properties = list(
    issues = S7::class_data.frame,
    passed = S7::new_property(S7::class_logical, default = TRUE),
    severity_summary = S7::new_property(S7::class_list, default = list()),
    data_summary = S7::new_property(
      S7::new_union(S7::class_data.frame, NULL),
      default = NULL
    ),
    recommendations = S7::new_property(S7::class_character, default = character()),
    timestamp = S7::new_property(
      S7::new_union(S7::class_any, NULL),
      default = NULL
    )
  ),
  validator = function(self) {
    errors <- character()

    # Validate issues has required columns if not empty
    if (nrow(self@issues) > 0) {
      required_cols <- c("check_id", "category", "variable", "severity", "message")
      missing_cols <- setdiff(required_cols, names(self@issues))
      if (length(missing_cols) > 0) {
        errors <- c(errors, glue::glue(
          "Issues tibble missing required columns: {paste(missing_cols, collapse = ', ')}"
        ))
      }

      # Validate severity values
      if ("severity" %in% names(self@issues)) {
        valid_severities <- c("error", "warning", "info")
        invalid <- setdiff(unique(self@issues$severity), valid_severities)
        if (length(invalid) > 0) {
          errors <- c(errors, glue::glue(
            "Invalid severity values: {paste(invalid, collapse = ', ')}. ",
            "Must be one of: {paste(valid_severities, collapse = ', ')}"
          ))
        }
      }
    }

    if (length(errors) > 0) errors else NULL
  }
)
