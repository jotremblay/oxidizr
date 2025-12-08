# Detect Steady-State Using Coefficient of Variation

Identifies steady-state periods where the coefficient of variation (CV)
of a variable falls below a specified threshold within a rolling window.

## Usage

``` r
detect_steady_state_cv(
  data,
  variable = "vo2",
  cv_threshold = 0.1,
  window_size = 3,
  min_duration = 2,
  id_col = "id",
  time_col = "time"
)
```

## Arguments

- data:

  A data frame or CalorimetryData object containing the measurements.

- variable:

  Character string specifying the variable to analyze (default: "vo2").

- cv_threshold:

  Numeric CV threshold (default: 0.10 = 10%). Values at or below this
  threshold are considered steady-state.

- window_size:

  Integer specifying the number of consecutive observations in the
  rolling window (default: 3).

- min_duration:

  Minimum number of consecutive steady-state windows required to qualify
  as a steady-state period (default: 2).

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

## Value

A tibble with columns:

- Subject ID column

- Time column

- `cv`: Rolling CV for each time point

- `is_steady`: Logical indicating if CV is below threshold

- `steady_period`: Integer identifying consecutive steady-state periods

- `meets_duration`: Logical indicating if period meets minimum duration

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect steady-state in VO2 data
ss_result <- detect_steady_state_cv(calo_data, variable = "vo2", cv_threshold = 0.10)

# Use stricter threshold
ss_strict <- detect_steady_state_cv(calo_data, variable = "vo2", cv_threshold = 0.05)
} # }
```
