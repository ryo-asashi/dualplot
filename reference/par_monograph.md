# Monograph Graphical Parameters for Base Graphics

Wraps [`par()`](https://rdrr.io/r/graphics/par.html) to apply a clean,
publication-ready base graphics aesthetic matching
[`theme_monograph()`](https://ryo-asashi.github.io/dualplot/reference/theme_monograph.md).

## Usage

``` r
par_monograph(...)
```

## Arguments

- ...:

  graphical parameters in `name = value` form passed to
  [`par()`](https://rdrr.io/r/graphics/par.html) to override the
  monograph defaults.

## Value

Invisibly returns a named list of the previous values of the modified
graphical parameters.

## See also

[`theme_monograph`](https://ryo-asashi.github.io/dualplot/reference/theme_monograph.md)

## Examples

``` r
old_par <- par_monograph()
draw_bars(c(A = 0.5, B = -0.3), theme = "azurerose", xlab = "Effect")

graphics::par(old_par)
```
