# Calculate Reference Enrichment (Rref)

Calculate baseline 13C enrichment from control/placebo condition. This
represents the natural 13C abundance in expired CO2.

## Usage

``` r
calc_rref(isotopes, control_protocol, by_time = TRUE)
```

## Arguments

- isotopes:

  An IsotopeData object

- control_protocol:

  Name of the control/placebo protocol

- by_time:

  Logical, calculate Rref at each time point (default: TRUE)

## Value

A tibble with Rref values
