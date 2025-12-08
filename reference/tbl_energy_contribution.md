# Energy Contribution Table

Create a table of energy contributions by substrate.

## Usage

``` r
tbl_energy_contribution(
  results,
  by = NULL,
  show_kcal = TRUE,
  show_percent = TRUE,
  decimals = 1
)
```

## Arguments

- results:

  An OxidationResults object

- by:

  Grouping variable

- show_kcal:

  Show absolute energy (kcal/min)

- show_percent:

  Show percentage contributions

- decimals:

  Number of decimal places

## Value

A gt table object
