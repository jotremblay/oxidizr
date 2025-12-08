# Subject Characteristics Table

Create a formatted table of subject characteristics.

## Usage

``` r
tbl_subjects(study, by = NULL, vars = NULL, decimals = 1)
```

## Arguments

- study:

  An OxidationStudy object

- by:

  Grouping variable (e.g., "protocol")

- vars:

  Variables to include (NULL = auto-detect)

- decimals:

  Number of decimal places

## Value

A gt table object
