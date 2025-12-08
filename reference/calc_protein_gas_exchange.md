# Calculate Protein Contribution to Gas Exchange

Calculate the VO2 and VCO2 attributable to protein oxidation.

## Usage

``` r
calc_protein_gas_exchange(
  protein_ox,
  vo2_factor = 1.0075,
  vco2_factor = 0.8443
)
```

## Arguments

- protein_ox:

  Protein oxidation rate (g/min) - vector or data frame

- vo2_factor:

  VO2 per gram protein (default: 1.0075 L/g, derived from Institute of
  Medicine, 2005, as presented in Telmosse, 2022)

- vco2_factor:

  VCO2 per gram protein (default: 0.8443 L/g, derived from Institute of
  Medicine, 2005, as presented in Telmosse, 2022)

## Value

A tibble with VO2p and VCO2p values
