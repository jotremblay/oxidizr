# Detect Steady-State Using Rolling Variance

Identifies steady-state periods where the rolling variance of a variable
falls below a specified threshold or automatically determined threshold.

## Usage

``` r
detect_steady_state_variance(
  data,
  variable = "vo2",
  variance_threshold = NULL,
  window_size = 5,
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

- variance_threshold:

  Numeric variance threshold. If NULL (default), automatically
  determined as the 25th percentile of observed variances.

- window_size:

  Integer specifying the number of consecutive observations in the
  rolling window (default: 5).

- min_duration:

  Minimum number of consecutive low-variance windows required to qualify
  as a steady-state period (default: 2).

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

## Value

A tibble with columns:

- Subject ID column

- Time column

- `variance`: Rolling variance for each time point

- `variance_threshold`: The threshold used

- `is_steady`: Logical indicating if variance is below threshold

- `steady_period`: Integer identifying consecutive steady-state periods

- `meets_duration`: Logical indicating if period meets minimum duration

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect steady-state with auto threshold
ss_result <- detect_steady_state_variance(calo_data, variable = "vo2")

# Use specific variance threshold
ss_specific <- detect_steady_state_variance(calo_data, variable = "vo2",
                                             variance_threshold = 0.01)
} # }
```
