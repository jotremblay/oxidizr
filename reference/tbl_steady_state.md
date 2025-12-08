# Steady-State Summary Table

Create a summary table of steady-state detection results.

## Usage

``` r
tbl_steady_state(steady_state_result, id_col = "id", decimals = 2)
```

## Arguments

- steady_state_result:

  Result from detect_steady_state() functions.

- id_col:

  Subject ID column name (default: "id").

- decimals:

  Number of decimal places (default: 2).

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
ss_result <- detect_steady_state(calo_data)
tbl_steady_state(ss_result)
} # }
```
