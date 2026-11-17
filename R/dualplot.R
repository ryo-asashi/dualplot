# R/dualplot.R
# Unified dual-backend plotting API (graphics & ggplot2)

#' Unified Dual-Backend Plotting Primitives
#'
#' These functions provide a single consistent interface that dispatches to either
#' base R \strong{graphics} or \strong{ggplot2} based on the \code{engine} argument (or \code{getOption("dualplot.engine")}).
#'
#' @name dualplot
#' @param engine character string; either \code{"base"} (default), \code{"graphics"}, \code{"gg"}, or \code{"ggplot2"}.
#' @inheritParams draw_bars_base
#'
#' @return Invisibly returns \code{NULL} when \code{engine = "graphics"}, or a
#'   \code{ggplot} object when \code{engine = "ggplot2"}.
#'
#' @examples
#' # 1. Horizontal barplot & waterfall chart
#' vals <- c(2.5, -1.2, 0.8, -0.4)
#' names(vals) <- c("carat", "clarity", "cut", "color")
#'
#' draw_bars(vals, theme = "azurerose", xlab = "Contribution", engine = "graphics")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_bars(vals, theme = "azurerose", xlab = "Contribution", engine = "ggplot2")
#' }
#'
#' # Waterfall chart
#' cs <- cumsum(c(10, vals))
#' draw_bars(to = cs[-1], from = cs[-length(cs)], labels = names(vals),
#'           connect = TRUE, vline = 10, theme = "steps")
#'
#' # 2. Multi-line series / ICE curves
#' x_grid <- seq(0, 10, length.out = 30)
#' y_mat <- sapply(1:5, function(i) sin(x_grid / 2) * i * 0.4)
#' draw_series(x_grid, y_mat, theme = "Okabe-Ito", xlab = "Temp", ylab = "Effect")
#'
#' # 3. 2D Surface plot
#' g <- seq(-2, 2, length.out = 25)
#' z_mat <- outer(g, g, function(a, b) a * b)
#' draw_surface(g, g, z_mat, theme = "azurerose", xlab = "x1", ylab = "x2")
#'
#' # 4. Matrix heatmap & Boxplots
#' m <- cor(mtcars[, 1:4])
#' draw_matrix(m, theme = "RdBu")
#' draw_boxes(split(iris$Sepal.Length, iris$Species), theme = "Set 2", xlab = "Length")
#'
#' @export
#'
draw_bars <- function(
    to, from = 0, labels = NULL, horizontal = TRUE,
    type = c("barplot", "dotchart", "none"), theme = NULL,
    theme.by = NULL, middle = 0, limits = NULL, width = NULL,
    vline = NULL, connect = FALSE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_bars_base else draw_bars_gg
  fun(
    to = to, from = from, labels = labels, horizontal = horizontal,
    type = type, theme = theme, theme.by = theme.by, middle = middle,
    limits = limits, width = width, vline = vline, connect = connect,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' @rdname dualplot
#' @inheritParams draw_series_base
#' @export
#'
draw_series <- function(
    x, y, theme = NULL, theme.by = NULL, discrete = NULL,
    limits = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "",
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_series_base else draw_series_gg
  fun(
    x = x, y = y, theme = theme, theme.by = theme.by, discrete = discrete,
    limits = limits, points = points,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' @rdname dualplot
#' @inheritParams draw_points_base
#' @export
#'
draw_points <- function(
    x, y, theme = NULL, theme.by = NULL, middle = 0,
    xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, main = NULL, sub = NULL, xlab = "", ylab = "",
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_points_base else draw_points_gg
  fun(
    x = x, y = y, theme = theme, theme.by = theme.by, middle = middle,
    xticks = xticks, yticks = yticks, xlim = xlim, ylim = ylim,
    overlay = overlay, main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' @rdname dualplot
#' @inheritParams draw_surface_base
#' @export
#'
draw_surface <- function(
    x, y, z, theme = NULL, zlim = NULL, middle = 0,
    xticks = NULL, yticks = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "",
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_surface_base else draw_surface_gg
  fun(
    x = x, y = y, z = z, theme = theme, zlim = zlim, middle = middle,
    xticks = xticks, yticks = yticks, points = points,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' @rdname dualplot
#' @inheritParams draw_matrix_base
#' @export
#'
draw_matrix <- function(
    z, labels = rownames(z), theme = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "",
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_matrix_base else draw_matrix_gg
  fun(
    z = z, labels = labels, theme = theme,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' @rdname dualplot
#' @inheritParams draw_boxes_base
#' @export
#'
draw_boxes <- function(
    x, theme = NULL, theme.by = NULL, horizontal = TRUE,
    main = NULL, sub = NULL, xlab = "", ylab = NULL,
    engine = getOption("dualplot.engine", "base"), ...
) {
  engine <- match.arg(engine, c("base", "gg", "graphics", "ggplot2"))
  fun <- if (engine == "graphics" || engine == "base")
    draw_boxes_base else draw_boxes_gg
  fun(
    x = x, theme = theme, theme.by = theme.by, horizontal = horizontal,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}
