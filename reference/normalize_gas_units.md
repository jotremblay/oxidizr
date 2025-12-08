# Normalize VO2/VCO2 Units

Convert VO2 and VCO2 values between L/min and mL/min.

## Usage

``` r
normalize_gas_units(calo, to_unit = "L/min")
```

## Arguments

- calo:

  A CalorimetryData object

- to_unit:

  Target unit: "L/min" or "mL/min"

## Value

A CalorimetryData object with normalized units
