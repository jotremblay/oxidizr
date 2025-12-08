# Calculate Exogenous CHO Oxidation

Calculate the rate of exogenous (ingested) carbohydrate oxidation using
13C isotope enrichment data.

## Usage

``` r
calc_exogenous_cho(calo, isotopes, rref, co2_cho_ratio = 0.747)
```

## Arguments

- calo:

  A CalorimetryData object

- isotopes:

  An IsotopeData object

- rref:

  Reference enrichment (from calc_rref or data frame with rref column)

- co2_cho_ratio:

  CO2/CHO conversion ratio (default: 0.747)

## Value

A tibble with exogenous CHO oxidation rates (g/min)

## Details

The calculation uses the formula: CHOexo = VCO2 \* ((Rexp - Rref) /
(Rexo - Rref)) / 0.747

Where:

- Rexp = 13C enrichment of expired CO2

- Rref = baseline 13C enrichment (from placebo)

- Rexo = 13C enrichment of ingested substrate

- 0.747 = L CO2 per gram CHO oxidized (Péronnet & Massicotte, 1991;
  Telmosse, 2022)
