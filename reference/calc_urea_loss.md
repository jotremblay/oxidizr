# Calculate Urea Loss

Calculate total urea nitrogen loss from sweat and urine.

## Usage

``` r
calc_urea_loss(urea, sweat_loss, subjects = NULL)
```

## Arguments

- urea:

  A UreaData object

- sweat_loss:

  A data frame with sweat loss (from calc_sweat_loss)

- subjects:

  A SubjectData object (for urine volume)

## Value

A tibble with urea loss components
