# Check Required Columns

Internal helper to validate that required columns exist in a data frame.

## Usage

``` r
check_required_cols(df, required, context = "data")
```

## Arguments

- df:

  Data frame to check

- required:

  Character vector of required column names

- context:

  Context string for error message

## Value

TRUE if all columns present, otherwise throws error
