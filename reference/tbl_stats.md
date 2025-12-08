# Statistical Results Table

Create a table of statistical test results.

## Usage

``` r
tbl_stats(model, type = c("anova", "posthoc"), decimals = 3)
```

## Arguments

- model:

  A fitted model object (aov, lmer, etc.)

- type:

  Type of table: "anova" or "posthoc"

- decimals:

  Number of decimal places

## Value

A gt table object
