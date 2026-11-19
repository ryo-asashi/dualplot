# Draw 2D Surface / Heatmap Plots

Draw 2D Surface / Heatmap Plots

## Usage

``` r
draw_surface(
  x,
  y,
  z,
  zlim = NULL,
  middle = 0,
  xticks = NULL,
  yticks = NULL,
  points = NULL,
  ...,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = NULL,
  legend = NULL,
  legend.title = NULL,
  engine = getOption("dualplot.engine", "graphics")
)
```

## Arguments

- x, y:

  numeric vectors of grid coordinates.

- z:

  numeric matrix of surface values (`length(x)` by `length(y)`).

- zlim:

  optional numeric vector of length 2 for color scale limits.

- middle:

  numeric midpoint for diverging color themes (default `0`).

- xticks, yticks:

  optional character vectors for discrete axis tick labels.

- points:

  optional `data.frame` with `x` and `y` columns to overlay.

- ...:

  additional graphical parameters.

- main, sub, xlab, ylab:

  optional plot titles and axis labels.

- theme:

  optional color theme name or `"colortheme"` object.

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
gx <- seq(-2, 2, length.out = 25)
gy <- seq(-2, 2, length.out = 25)
gz <- outer(gx, gy, function(a, b) sin(a) * cos(b))
pts <- data.frame(x = c(-1, 0.5, 1.2), y = c(0.8, -0.5, 1.0))

draw_surface(
  gx, gy, gz, points = pts,
  xlab = "X1", ylab = "X2", legend = "right", legend.title = "Effect"
)


if (requireNamespace("ggplot2", quietly = TRUE)) {
  draw_surface(
    gx, gy, gz, points = pts,
    xlab = "X1", ylab = "X2", legend.title = "Effect", engine = "ggplot2"
  )
}
```
