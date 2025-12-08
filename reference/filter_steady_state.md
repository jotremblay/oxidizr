# Filter Data to Steady-State Periods

Filters a data frame to include only observations within qualifying
steady-state periods.

## Usage

``` r
filter_steady_state(
  data,
  steady_state_result,
  require_meets_duration = TRUE,
  id_col = "id",
  time_col = "time"
)
```

## Arguments

- data:

  Original data frame to filter.

- steady_state_result:

  Result from any of the detect_steady_state functions.

- require_meets_duration:

  Logical, whether to require periods meet minimum duration (default:
  TRUE).

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

## Value

Filtered data frame containing only steady-state observations.
