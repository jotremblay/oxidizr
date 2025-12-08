# Plot Isotope Enrichment

Create a time course plot of 13C isotope enrichment.

## Usage

``` r
plot_isotope_enrichment(
  results,
  type = c("expired", "plasma"),
  by = NULL,
  time_col = "time",
  show_error = TRUE
)
```

## Arguments

- results:

  An OxidationResults object with isotope data

- type:

  Type of enrichment: "expired" (Rexp), "plasma" (Rpla)

- by:

  Grouping variable

- time_col:

  Name of time column

- show_error:

  Show error bars

## Value

A ggplot2 object
