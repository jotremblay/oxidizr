# Plot RER Time Course

Create a time course plot of Respiratory Exchange Ratio.

## Usage

``` r
plot_rer_timecourse(
  results,
  by = NULL,
  time_col = "time",
  show_error = TRUE,
  error_type = c("sd", "se")
)
```

## Arguments

- results:

  An OxidationResults object or data frame

- by:

  Grouping variable for color (e.g., "protocol")

- time_col:

  Name of the time column

- show_error:

  Show error bars

- error_type:

  Type of error bars: "sd" or "se"

## Value

A ggplot2 object
