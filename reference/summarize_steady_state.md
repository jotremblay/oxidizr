# Summarize Steady-State Periods

Generates a summary of steady-state periods identified in the data.

## Usage

``` r
summarize_steady_state(steady_state_result, id_col = "id", time_col = "time")
```

## Arguments

- steady_state_result:

  Result from any of the detect_steady_state functions.

- id_col:

  Character string specifying the subject ID column (default: "id").

- time_col:

  Character string specifying the time column (default: "time").

## Value

A tibble with columns:

- Subject ID column

- `period`: Period number

- `start_time`: Start time of period

- `end_time`: End time of period

- `duration_points`: Number of data points in period

- `meets_duration`: Whether period meets minimum duration

- `mean_cv`: Mean CV during period (if available)

- `mean_variance`: Mean variance during period (if available)
