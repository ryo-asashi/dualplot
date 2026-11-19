# Draw Multi-Line Series and ICE Curves

Draw Multi-Line Series and ICE Curves

## Usage

``` r
draw_lines(
  x,
  y,
  discrete = NULL,
  limits = NULL,
  points = NULL,
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

- x:

  vector of x-axis values (numeric, factor, or character).

- y:

  numeric matrix or vector of y-values (`NROW(y) == length(x)`).

- discrete:

  logical; if `NULL`, inferred automatically from `x`.

- limits:

  optional numeric vector of length 2 for y-axis limits.

- points:

  optional `data.frame` with columns `x` and `y` (and optional `col`,
  `pch`, `cex`) for overlaying points.

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

- theme.values:

  optional vector (length `ncol(y)`) mapped to line colors via `theme`.

- legend:

  logical or character legend position.

- legend.title:

  optional title string for the legend.

- engine:

  graphics backend: `"graphics"` or `"ggplot2"`.

## Value

Invisibly returns `NULL` for `"graphics"`, or a `"ggplot"` object for
`"ggplot2"`.

## Examples

``` r
x <- seq(0, 2 * pi, length.out = 50)
y <- cbind(Sine = sin(x), Cosine = cos(x))
obs <- data.frame(x = c(1, 3, 5), y = sin(c(1, 3, 5)))

draw_lines(
  x, y, points = obs, theme = "Okabe-Ito",
  xlab = "x", ylab = "Value", legend = "topright", legend.title = "Series"
)


if (requireNamespace("ggplot2", quietly = TRUE)) {
  draw_lines(x, y, points = obs, theme = "Okabe-Ito", engine = "ggplot2")
}
```
