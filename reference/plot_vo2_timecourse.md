# Plot VO2 Time Course

Create a time course plot of oxygen consumption.

## Usage

``` r
plot_vo2_timecourse(
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

  Grouping variable for color

- time_col:

  Name of time column

- show_error:

  Show error bars

- error_type:

  Type of error bars

## Value

A ggplot2 object
