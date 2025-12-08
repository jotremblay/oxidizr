# Calculate RER from Calorimetry Data

Calculate Respiratory Exchange Ratio (VCO2/VO2).

## Usage

``` r
calc_rer(calo, vo2_col = NULL, vco2_col = NULL)
```

## Arguments

- calo:

  A CalorimetryData object or data frame

- vo2_col:

  Name of the VO2 column (if data frame)

- vco2_col:

  Name of the VCO2 column (if data frame)

## Value

A numeric vector of RER values
