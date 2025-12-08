# Plot CHO Partition

Create a plot showing the partitioning of CHO oxidation sources.

## Usage

``` r
plot_cho_partition(results, by = "protocol", type = c("exo_endo", "mus_liv"))
```

## Arguments

- results:

  An OxidationResults object

- by:

  Grouping variable

- type:

  Type of partition: "exo_endo" or "mus_liv"

## Value

A ggplot2 object
