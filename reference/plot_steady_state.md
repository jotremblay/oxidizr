# Plot Steady-State Analysis

Visualizes steady-state detection results showing the variable time
course with rolling statistics and highlighted steady-state periods.

## Usage

``` r
plot_steady_state(
  steady_state_result,
  data = NULL,
  variable = "vo2",
  show_cv = TRUE,
  show_variance = TRUE,
  highlight_periods = TRUE,
  id_col = "id",
  time_col = "time",
  facet_by = NULL
)
```

## Arguments

- steady_state_result:

  Result from detect_steady_state() functions.

- data:

  Optional original data frame for additional context.

- variable:

  Variable to plot (default: "vo2"). Only used if not present in
  steady_state_result.

- show_cv:

  Logical, show CV overlay (default: TRUE if available).

- show_variance:

  Logical, show variance overlay (default: TRUE if available).

- highlight_periods:

  Logical, highlight qualifying steady-state periods (default: TRUE).

- id_col:

  Subject ID column name (default: "id").

- time_col:

  Time column name (default: "time").

- facet_by:

  Character, facet by this column (default: id_col for multiple
  subjects).

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
ss_result <- detect_steady_state(calo_data)
plot_steady_state(ss_result)
} # }
```
