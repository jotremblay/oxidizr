# Calculate Energy Percentages

Calculate the percentage contribution of each substrate to total energy.

## Usage

``` r
calc_energy_percent(energy)
```

## Arguments

- energy:

  A data frame with energy yield columns (from calc_energy_yield)

## Value

A tibble with percentage contribution columns added

## Examples

``` r
if (FALSE) { # \dontrun{
ox <- calc_substrate_oxidation(calo)
energy <- calc_energy_yield(ox)
energy_pct <- calc_energy_percent(energy)
} # }
```
