# Set or Get Global Options for dualplot

Set or Get Global Options for dualplot

## Usage

``` r
dualplot_options(...)
```

## Arguments

- ...:

  options specified as `name = value` pairs (e.g.,
  `engine = "ggplot2"`), character strings to query specific options, or
  empty to retrieve all current `dualplot.*` options.

## Value

A named list of current or previous option values.

## Examples

``` r
old <- dualplot_options(engine = "ggplot2")
dualplot_options("engine")
#> $engine
#> [1] "ggplot2"
#> 
dualplot_options(old)
```
