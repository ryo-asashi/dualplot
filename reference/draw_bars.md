# Draw Bar, Dot, and Waterfall Charts

Draw Bar, Dot, and Waterfall Charts

## Usage

``` r
draw_bars(
  to,
  from = 0,
  ticks = NULL,
  horizontal = TRUE,
  type = c("bar", "dot"),
  middle = 0,
  limits = NULL,
  width = NULL,
  vline = NULL,
  connect = FALSE,
  ...,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = NULL,
  theme.values = NULL,
  legend = NULL,
  legend.title = NULL,
  engine = getOption("dualplot.engine", "graphics")
)
```

## Arguments

- to:

  numeric vector or matrix (`nrow` categories by `ncol` groups) of end
  values.

- from:

  numeric vector or matrix of start values (default `0`).

- ticks:

  character vector of category tick labels (length `nrow(to)`).

- horizontal:

  logical; if `TRUE`, bars/dots are drawn horizontally (top-to-bottom).

- type:

  one of `"bar"` or `"dot"`.

- middle:

  numeric midpoint for diverging color themes (default `0`).

- limits:

  optional numeric vector of length 2 for value axis limits.

- width:

  optional numeric bar/dodge width.

- vline:

  optional numeric intercept for a reference line.

- connect:

  logical; if `TRUE`, draws step lines connecting bars (for waterfall
  charts).

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- theme.values:

  optional vector or matrix mapped to colors via `theme`.

- legend:

  logical or character legend position (`FALSE`, `TRUE`, `"topright"`,
  `"topleft"`, `"bottomright"`, `"bottomleft"`, `"top"`, `"bottom"`,
  `"left"`, `"right"`).

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.

## Examples

``` r
vals <- c(Age = 0.42, Income = -0.28, Tenure = 0.65, Region = -0.15)

# Horizontal bar chart with diverging color theme
draw_bars(vals, theme = "azurerose", xlab = "Contribution", vline = 0)


# Dot chart
draw_bars(vals, type = "dot", theme = "azurerose", xlab = "Contribution")


# Waterfall chart
draw_bars(
  to = c(1.0, 1.4, 1.1, 1.5),
  from = c(0.0, 1.0, 1.4, 1.1),
  ticks = c("Intercept", "Age", "Income", "Total"),
  connect = TRUE, vline = 0, theme = "azurerose"
)


if (requireNamespace("ggplot2", quietly = TRUE)) {
  draw_bars(vals, theme = "azurerose", xlab = "Contribution", engine = "ggplot2")
}
```
