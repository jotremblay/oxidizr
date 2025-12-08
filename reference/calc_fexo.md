# Calculate Fraction Exogenous (Fexo)

Calculate the fraction of plasma glucose derived from exogenous sources.

## Usage

``` r
calc_fexo(isotopes, rref)
```

## Arguments

- isotopes:

  An IsotopeData object (must include rpla)

- rref:

  Reference enrichment value or data frame

## Value

A tibble with Fexo values (%)

## Details

Fexo = ((Rpla - Rref) / (Rexo - Rref)) \* 100

Where Rpla is the 13C enrichment of plasma glucose.
