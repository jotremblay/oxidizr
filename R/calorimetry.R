#' Calorimetry Data Functions
#'
#' @name calorimetry
#' @description Functions for importing and processing indirect calorimetry data
NULL

#' Read Calorimetry Data
#'
#' Import calorimetry data from various file formats.
#'
#' @param file Path to the data file
#' @param format File format: "csv", "xlsx", "auto" (default: "auto")
#' @param id_col Name of the subject ID column
#' @param time_col Name of the time column
#' @param vo2_col Name of the VO2 column
#' @param vco2_col Name of the VCO2 column
#' @param vo2_unit Unit for VO2/VCO2: "L/min" or "mL/min"
#' @param protocol_col Name of the protocol column (optional)
#' @param sheet Sheet name or number for Excel files (default: 1)
#' @param ... Additional arguments passed to read functions
#'
#' @return A CalorimetryData S7 object
#' @export
#'
#' @examples
#' \dontrun{
#' calo <- read_calorimetry("vo2_data.csv",
#'                          id_col = "subject",
#'                          time_col = "time_min",
#'                          vo2_col = "VO2",
#'                          vco2_col = "VCO2")
#' }
read_calorimetry <- function(file,
                              format = c("auto", "csv", "xlsx"),
                              id_col = "id",
                              time_col = "time",
                              vo2_col = "vo2",
                              vco2_col = "vco2",
                              vo2_unit = "L/min",
                              protocol_col = NULL,
                              sheet = 1,
                              ...) {
  format <- match.arg(format)

  # Auto-detect format
 if (format == "auto") {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      "csv" = "csv",
      "xlsx" = "xlsx",
      "xls" = "xlsx",
      cli::cli_abort("Cannot auto-detect format for extension '.{ext}'")
    )
  }

  # Read data
  data <- switch(format,
    csv = readr::read_csv(file, show_col_types = FALSE, ...),
    xlsx = readxl::read_excel(file, sheet = sheet, ...)
  )

  # Create CalorimetryData object
  CalorimetryData(
    data = as.data.frame(data),
    id_col = id_col,
    time_col = time_col,
    vo2_col = vo2_col,
    vco2_col = vco2_col,
    vo2_unit = vo2_unit,
    protocol_col = protocol_col
  )
}

#' Calculate RER from Calorimetry Data
#'
#' Calculate Respiratory Exchange Ratio (VCO2/VO2).
#'
#' @param calo A CalorimetryData object or data frame
#' @param vo2_col Name of the VO2 column (if data frame)
#' @param vco2_col Name of the VCO2 column (if data frame)
#'
#' @return A numeric vector of RER values
#' @export
calc_rer <- function(calo, vo2_col = NULL, vco2_col = NULL) {
  if (S7_inherits(calo, CalorimetryData)) {
    vo2 <- calo@data[[calo@vo2_col]]
    vco2 <- calo@data[[calo@vco2_col]]
  } else if (is.data.frame(calo)) {
    if (is.null(vo2_col) || is.null(vco2_col)) {
      cli::cli_abort("Must specify vo2_col and vco2_col for data frame input")
    }
    vo2 <- calo[[vo2_col]]
    vco2 <- calo[[vco2_col]]
  } else {
    cli::cli_abort("calo must be a CalorimetryData object or data frame")
  }

  vco2 / vo2
}

#' Normalize VO2/VCO2 Units
#'
#' Convert VO2 and VCO2 values between L/min and mL/min.
#'
#' @param calo A CalorimetryData object
#' @param to_unit Target unit: "L/min" or "mL/min"
#'
#' @return A CalorimetryData object with normalized units
#' @export
normalize_gas_units <- function(calo, to_unit = "L/min") {
  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }

  if (calo@vo2_unit == to_unit) {
    return(calo)
  }

  data <- calo@data
  factor <- if (to_unit == "L/min") 1/1000 else 1000

  data[[calo@vo2_col]] <- data[[calo@vo2_col]] * factor
  data[[calo@vco2_col]] <- data[[calo@vco2_col]] * factor

  CalorimetryData(
    data = data,
    id_col = calo@id_col,
    time_col = calo@time_col,
    vo2_col = calo@vo2_col,
    vco2_col = calo@vco2_col,
    vo2_unit = to_unit,
    protocol_col = calo@protocol_col
  )
}

#' Aggregate Calorimetry Data
#'
#' Aggregate calorimetry measurements by subject, time, and optionally protocol.
#'
#' @param calo A CalorimetryData object
#' @param by Character vector of grouping columns (uses id_col, time_col, protocol_col by default)
#' @param fun Aggregation function (default: mean)
#'
#' @return A CalorimetryData object with aggregated data
#' @export
aggregate_calorimetry <- function(calo, by = NULL, fun = mean) {
  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }

  # Default grouping columns
  if (is.null(by)) {
    by <- c(calo@id_col, calo@time_col)
    if (!is.null(calo@protocol_col)) {
      by <- c(by, calo@protocol_col)
    }
  }

  # Check columns exist
  missing <- setdiff(by, names(calo@data))
  if (length(missing) > 0) {
    cli::cli_abort("Columns not found: {paste(missing, collapse = ', ')}")
  }

  # Aggregate
  agg_data <- calo@data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c(calo@vo2_col, calo@vco2_col)),
        \(x) fun(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  CalorimetryData(
    data = agg_data,
    id_col = calo@id_col,
    time_col = calo@time_col,
    vo2_col = calo@vo2_col,
    vco2_col = calo@vco2_col,
    vo2_unit = calo@vo2_unit,
    protocol_col = calo@protocol_col
  )
}

#' Filter Calorimetry Data by Time
#'
#' Filter calorimetry data to a specific time range.
#'
#' @param calo A CalorimetryData object
#' @param time_range Numeric vector of length 2 specifying min and max time
#'
#' @return A CalorimetryData object with filtered data
#' @export
filter_time_range <- function(calo, time_range) {
  if (!S7_inherits(calo, CalorimetryData)) {
    cli::cli_abort("calo must be a CalorimetryData object")
  }

  if (length(time_range) != 2) {
    cli::cli_abort("time_range must be a vector of length 2")
  }

  time_var <- calo@data[[calo@time_col]]

  # Convert to numeric if factor
  if (is.factor(time_var)) {
    time_var <- as.numeric(as.character(time_var))
  }

  filtered <- calo@data |>
    dplyr::filter(time_var >= time_range[1] & time_var <= time_range[2])

  CalorimetryData(
    data = as.data.frame(filtered),
    id_col = calo@id_col,
    time_col = calo@time_col,
    vo2_col = calo@vo2_col,
    vco2_col = calo@vco2_col,
    vo2_unit = calo@vo2_unit,
    protocol_col = calo@protocol_col
  )
}
