# tests/testthat/test-dualplot.R

test_that("dualplot_options gets, sets, and restores options cleanly", {
  old <- dualplot_options()
  on.exit(options(old), add = TRUE)

  prev <- dualplot_options(engine = "ggplot2")
  expect_equal(dualplot_options("engine")$engine, "ggplot2")

  dualplot_options(prev)
  expect_equal(dualplot_options("engine")$engine, old$engine %||% "graphics")

  expect_warning(dualplot_options(unknown_opt = 1), "unknown option")
  expect_error(dualplot_options("a" = 1, 2), "name = value")
})

test_that("parameter translation and alpha manipulation work bidirectionally", {
  base_p <- dualplot:::cast_to_base_params(
    list(colour = "red", size = 2, shape = 17, linetype = 2, linewidth = 1.5, alpha = 0.5),
    defaults = list(col = "black", pch = 16)
  )
  expect_equal(base_p$pch, 17)
  expect_equal(base_p$cex, 2)
  expect_equal(base_p$lty, 2)
  expect_equal(base_p$lwd, 1.5)
  expect_true(grepl("^#FF0000", base_p$col))
  expect_null(base_p$alpha)

  gg_p <- dualplot:::cast_to_gg_params(
    list(col = "blue", cex = 3, pch = 15, lty = 3, lwd = 2)
  )
  expect_equal(gg_p$colour, "blue")
  expect_equal(gg_p$size, 3)
  expect_equal(gg_p$shape, 15)
  expect_equal(gg_p$linetype, 3)
  expect_equal(gg_p$linewidth, 2)
})

test_that("draw_bars works for bar, dot, grouped, and waterfall charts on both engines", {
  skip_if_not_installed("colortheme")

  vals <- c(0.4, -0.2, 0.7)
  names(vals) <- c("Age", "Income", "Region")

  # Base graphics: bar, dot, vertical, waterfall, outside legend
  expect_null(draw_bars(vals, theme = "azurerose", engine = "graphics", legend = "right", legend.title = "Effect"))
  expect_null(draw_bars(vals, type = "dot", horizontal = FALSE, theme = "azurerose", engine = "graphics", legend = "bottom"))
  expect_null(draw_bars(
    to = c(1.0, 1.3, 1.1), from = c(0.0, 1.0, 1.3),
    ticks = c("Base", "Step 1", "Step 2"),
    connect = TRUE, vline = 0, theme = "azurerose", engine = "graphics"
  ))

  # Grouped matrix
  mat <- cbind(A = c(0.3, 0.5), B = c(0.2, 0.6))
  rownames(mat) <- c("X1", "X2")
  expect_null(draw_bars(mat, theme = "Okabe-Ito", engine = "graphics", legend = "left"))

  # ggplot2
  skip_if_not_installed("ggplot2")
  p_bar <- draw_bars(vals, theme = "azurerose", engine = "ggplot2", legend = "bottom", legend.title = "")
  expect_s3_class(p_bar, "ggplot")

  p_dot <- draw_bars(vals, type = "dot", horizontal = FALSE, theme = "azurerose", engine = "ggplot2", legend = "topright", legend.title = "Val")
  expect_s3_class(p_dot, "ggplot")

  p_wf <- draw_bars(
    to = c(1.0, 1.3, 1.1), from = c(0.0, 1.0, 1.3),
    ticks = c("Base", "Step 1", "Step 2"),
    horizontal = FALSE, connect = TRUE, theme = "azurerose", engine = "ggplot2"
  )
  expect_s3_class(p_wf, "ggplot")
})

test_that("draw_lines handles continuous and discrete x with points overlay", {
  skip_if_not_installed("colortheme")

  x <- seq(0, 1, length.out = 10)
  y <- cbind(Main = sin(pi * x), Alt = cos(pi * x))
  pts <- data.frame(x = c(0.2, 0.8), y = c(0.5, -0.5))

  expect_null(draw_lines(x, y, theme = "Okabe-Ito", points.df = pts, engine = "graphics", legend = "topright"))
  expect_null(draw_lines(c("Low", "Mid", "High"), y[1:3, ], theme = "Okabe-Ito", engine = "graphics", legend = "bottom"))

  skip_if_not_installed("ggplot2")
  p <- draw_lines(x, y, theme = "Okabe-Ito", points.df = pts, engine = "ggplot2", legend.title = "Series")
  expect_s3_class(p, "ggplot")
})

test_that("draw_points, draw_surface, draw_matrix, and draw_boxes run cleanly on both engines", {
  skip_if_not_installed("colortheme")

  # Points
  expect_null(draw_points(1:10, rnorm(10), theme = "viridis", theme.values = 1:10, engine = "graphics", legend = "right"))
  expect_null(draw_points(1:3, 1:3, overlay = TRUE, col = "red", engine = "graphics"))

  # Surface
  gx <- seq(-1, 1, length.out = 8)
  gy <- seq(-1, 1, length.out = 8)
  gz <- outer(gx, gy, "*")
  expect_null(draw_surface(gx, gy, gz, engine = "graphics", legend = "right", legend.title = "Interaction"))

  # Matrix
  cm <- cor(mtcars[, 1:4])
  expect_null(draw_matrix(cm, theme = "azurerose", engine = "graphics", legend = "bottom", legend.title = "Corr"))

  # Boxes
  bx <- list(A = rnorm(20), B = rnorm(20, 1), C = rnorm(20, -1))
  expect_null(draw_boxes(bx, theme = "Okabe-Ito", engine = "graphics", legend = "topleft"))

  skip_if_not_installed("ggplot2")
  expect_s3_class(draw_points(1:10, rnorm(10), theme = "viridis", theme.values = 1:10, engine = "ggplot2"), "ggplot")
  expect_s3_class(draw_surface(gx, gy, gz, engine = "ggplot2", legend.title = "Interaction"), "ggplot")
  expect_s3_class(draw_matrix(cm, theme = "azurerose", engine = "ggplot2", legend.title = "Corr"), "ggplot")
  expect_s3_class(draw_boxes(bx, theme = "Okabe-Ito", engine = "ggplot2"), "ggplot")
})

test_that("theme_monograph and par_monograph apply clean monograph aesthetics", {
  old_par <- par_monograph()
  on.exit(graphics::par(old_par), add = TRUE)
  expect_equal(graphics::par("family"), "serif")
  expect_equal(graphics::par("bg"), "white")

  skip_if_not_installed("ggplot2")
  th <- theme_monograph(grid.type = "y")
  expect_s3_class(th, "theme")
})
