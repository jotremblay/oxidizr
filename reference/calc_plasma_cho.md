# Calculate Plasma CHO Oxidation

Calculate plasma-derived carbohydrate oxidation using Rpla enrichment.

## Usage

``` r
calc_plasma_cho(calo, isotopes, rref, co2_cho_ratio = 0.747)
```

## Arguments

- calo:

  A CalorimetryData object

- isotopes:

  An IsotopeData object (must include rpla)

- rref:

  Reference enrichment

- co2_cho_ratio:

  CO2/CHO ratio (default: 0.747)

## Value

A tibble with plasma CHO oxidation rates

## Details

CHOpla = VCO2 \* ((Rexp - Rref) / (Rpla - Rref)) / 0.747

This represents total CHO oxidation from plasma glucose pool. The 0.747
L CO2 per gram CHO oxidized factor is based on Péronnet & Massicotte
(1991) and presented in Telmosse (2022).
