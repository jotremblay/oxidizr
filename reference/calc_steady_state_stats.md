# Calculate Steady-State Statistics

Calculates summary statistics for variables during steady-state periods.

## Usage

``` r
calc_steady_state_stats(
  data,
  steady_state_result,
  variables = c("vo2", "vco2"),
  id_col = "id",
  time_col = "time"
)
```

## Arguments

- data:

  Original data frame.

- steady_state_result:

  Result from any of the detect_steady_state functions.

- variables:

  Character vector of variables to summarize.

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

## Value

A tibble with summary statistics (mean, sd, cv, min, max, n) for each
variable during steady-state periods, grouped by subject.
