# Calculate Cumulative Energy

Calculate cumulative energy expenditure over time.

## Usage

``` r
calc_cumulative_energy(
  energy,
  time_col = "time",
  time_interval = 30,
  by = NULL
)
```

## Arguments

- energy:

  A data frame with energy columns

- time_col:

  Name of the time column

- time_interval:

  Time interval between measurements (minutes)

- by:

  Optional grouping variable(s)

## Value

A tibble with cumulative energy columns added
