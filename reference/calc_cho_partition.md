# Calculate CHO Source Partitioning

Partition total CHO oxidation into exogenous, endogenous, muscle, and
liver sources.

## Usage

``` r
calc_cho_partition(substrate_ox, cho_exo, cho_pla = NULL)
```

## Arguments

- substrate_ox:

  Substrate oxidation from calc_substrate_oxidation

- cho_exo:

  Exogenous CHO oxidation from calc_exogenous_cho

- cho_pla:

  Plasma CHO oxidation from calc_plasma_cho (optional)

## Value

A tibble with partitioned CHO oxidation rates

## Details

Partitioning calculations:

- CHOendo = CHOtot - CHOexo (endogenous = total - exogenous)

- CHOmus = CHOtot - CHOpla (muscle = total - plasma-derived)

- CHOliv = CHOpla - CHOexo (liver output = plasma - exogenous)
