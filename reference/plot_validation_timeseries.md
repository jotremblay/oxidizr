# Plot Validation Time Series

Creates a faceted time series plot showing variables with out-of-range
values highlighted based on validation thresholds.

## Usage

``` r
plot_validation_timeseries(
  data,
  thresholds = NULL,
  variables = NULL,
  id_col = "id",
  time_col = "time",
  show_thresholds = TRUE
)
```

## Arguments

- data:

  A data frame or CalorimetryData object.

- thresholds:

  Named list of threshold lists (e.g., calorimetry_thresholds).

- variables:

  Variables to plot (default: auto-detect from thresholds).

- id_col:

  Subject ID column name (default: "id").

- time_col:

  Time column name (default: "time").

- show_thresholds:

  Logical, show threshold lines (default: TRUE).

## Value

A ggplot2 object
