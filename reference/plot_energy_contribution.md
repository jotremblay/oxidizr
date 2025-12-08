# Plot Energy Contribution

Create a stacked bar chart of energy contributions by substrate.

## Usage

``` r
plot_energy_contribution(
  results,
  by = "protocol",
  partition = FALSE,
  show_error = TRUE
)
```

## Arguments

- results:

  An OxidationResults object

- by:

  Grouping variable (typically "protocol")

- partition:

  Show CHO partition (exo/endo) instead of total

- show_error:

  Show error bars

## Value

A ggplot2 object
