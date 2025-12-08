# Plot Oxidation Time Course

Create a time course plot of substrate oxidation rates.

## Usage

``` r
plot_oxidation_timecourse(
  results,
  substrate = c("all", "cho", "fat", "protein"),
  by = NULL,
  time_col = "time",
  show_error = TRUE,
  error_type = c("sd", "se"),
  dodge_width = 3
)
```

## Arguments

- results:

  An OxidationResults object or data frame

- substrate:

  Which substrate(s) to plot: "cho", "fat", "protein", or "all"

- by:

  Grouping variable for color/linetype (e.g., "protocol")

- time_col:

  Name of the time column

- show_error:

  Show error bars (SD or SE)

- error_type:

  Type of error bars: "sd" or "se"

- dodge_width:

  Width for position_dodge (default: 3)

## Value

A ggplot2 object
