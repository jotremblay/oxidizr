# Validate Calorimetry Data

Check indirect calorimetry data against physiological ranges and for
data quality issues.

## Usage

``` r
validate_calorimetry(
  calo,
  thresholds = NULL,
  check_outliers = TRUE,
  check_missing = TRUE,
  check_rer = TRUE,
  id_col = NULL,
  time_col = NULL,
  vo2_col = NULL,
  vco2_col = NULL,
  vo2_unit = "L/min"
)
```

## Arguments

- calo:

  A CalorimetryData object or data frame

- thresholds:

  Custom thresholds (NULL = use calorimetry_thresholds)

- check_outliers:

  Logical; detect statistical outliers using IQR method

- check_missing:

  Logical; check for missing data patterns

- check_rer:

  Logical; validate derived RER values

- id_col:

  Name of ID column (required if data frame)

- time_col:

  Name of time column (required if data frame)

- vo2_col:

  Name of VO2 column (required if data frame)

- vco2_col:

  Name of VCO2 column (required if data frame)

- vo2_unit:

  Unit for VO2/VCO2 (default: "L/min")

## Value

A ValidationResult S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c(1, 1, 2, 2),
  time = c(30, 60, 30, 60),
  vo2 = c(2.5, 2.6, 2.4, 7.0),
  vco2 = c(2.3, 2.4, 2.2, 2.3)
)
calo <- CalorimetryData(data = df)
validation <- validate_calorimetry(calo)
print(validation)
} # }
```
