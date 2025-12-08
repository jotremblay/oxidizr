# Detect Steady-State Using Combined Methods

Combines CV threshold and rolling variance methods for robust
steady-state detection. Can use either method alone or both with
configurable combination logic.

## Usage

``` r
detect_steady_state(
  data,
  variables = c("vo2", "vco2"),
  cv_threshold = 0.1,
  variance_threshold = NULL,
  method = c("both", "cv", "variance"),
  combine_method = c("and", "or"),
  window_size_cv = 3,
  window_size_var = 5,
  min_duration = 2,
  id_col = "id",
  time_col = "time",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame or CalorimetryData object containing the measurements.

- variables:

  Character vector of variables to analyze (default: c("vo2", "vco2")).
  When multiple variables are specified, steady-state is determined
  based on all variables meeting the criteria.

- cv_threshold:

  Numeric CV threshold (default: 0.10 = 10%).

- variance_threshold:

  Numeric variance threshold. If NULL (default), automatically
  determined per variable.

- method:

  Character specifying which method(s) to use: "both" (default), "cv",
  or "variance".

- combine_method:

  How to combine results when method = "both": "and" (default) requires
  both methods agree, "or" accepts either.

- window_size_cv:

  Window size for CV method (default: 3).

- window_size_var:

  Window size for variance method (default: 5).

- min_duration:

  Minimum consecutive steady-state points (default: 2).

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

- verbose:

  Logical, whether to print progress messages (default: TRUE).

## Value

A tibble with columns:

- Subject ID column

- Time column

- Variable columns

- For each variable: `{var}_cv`, `{var}_variance`, `{var}_steady`

- `is_steady_all`: Combined steady-state indicator across all variables

- `steady_period`: Integer identifying consecutive steady-state periods

- `meets_duration`: Logical indicating if period meets minimum duration

- `method`: The detection method used

## Examples

``` r
if (FALSE) { # \dontrun{
# Combined detection on VO2 and VCO2
ss <- detect_steady_state(calo_data, variables = c("vo2", "vco2"))

# Use only CV method
ss_cv <- detect_steady_state(calo_data, method = "cv", cv_threshold = 0.08)

# Accept steady-state if EITHER method indicates it
ss_or <- detect_steady_state(calo_data, method = "both", combine_method = "or")
} # }
```
