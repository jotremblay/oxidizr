# Calorimetry Data Class

S7 class for storing indirect calorimetry measurements (VO2, VCO2, RER).

## Arguments

- data:

  A data frame containing calorimetry measurements

- id_col:

  Name of the subject ID column (default: "id")

- time_col:

  Name of the time column (default: "time")

- vo2_col:

  Name of the VO2 column (default: "vo2")

- vco2_col:

  Name of the VCO2 column (default: "vco2")

- vo2_unit:

  Unit for VO2/VCO2: "L/min" or "mL/min" (default: "L/min")

- protocol_col:

  Name of the protocol/condition column (default: NULL)

## Value

A CalorimetryData S7 object

## Examples

``` r
df <- data.frame(
  id = c(1, 1, 2, 2),
  time = c(30, 60, 30, 60),
  vo2 = c(2.5, 2.6, 2.4, 2.5),
  vco2 = c(2.3, 2.4, 2.2, 2.3)
)
calo <- CalorimetryData(data = df)
```
