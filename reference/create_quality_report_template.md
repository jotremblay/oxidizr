# Create Quality Report Template

Copy the default quality report template to a location for
customization.

## Usage

``` r
create_quality_report_template(dest = "quality_report.qmd", overwrite = FALSE)
```

## Arguments

- dest:

  Destination file path.

- overwrite:

  Overwrite existing file (default: FALSE).

## Value

Invisible path to created file.

## Examples

``` r
if (FALSE) { # \dontrun{
create_quality_report_template("my_quality_template.qmd")
} # }
```
