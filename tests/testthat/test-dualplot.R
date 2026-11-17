test_that("cast_to_base_params and cast_to_gg_params translate parameters bidirectionally", {
  base_args <- cast_to_base_params(
    defaults = list(col = "black", lwd = 1),
    dots = list(colour = "red", linewidth = 2, alpha = 0.5)
  )
  expect_equal(base_args$lwd, 2)
  expect_equal(base_args$col, apply_alpha("red", 0.5))
  expect_null(base_args$alpha)

  gg_args <- cast_to_gg_params(
    dots = list(col = "blue", cex = 2, pch = 17, lty = 2, lwd = 1.5),
    allowed = c("colour", "size", "shape", "linetype", "linewidth")
  )
  expect_equal(gg_args$colour, "blue")
  expect_equal(gg_args$size, 2)
  expect_equal(gg_args$shape, 17)
  expect_equal(gg_args$linetype, 2)
  expect_equal(gg_args$linewidth, 1.5)
})

test_that("graphics backend renders all 6 primitives without error", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)

  # Bars, dots, waterfall
  v <- c(A = 1.5, B = -0.8, C = 0.3)
  expect_invisible(draw_bars_base(v, theme = "azurerose"))
  expect_invisible(draw_bars_base(v, type = "dotchart", theme = "Okabe-Ito", size = 1.5))
  cs <- cumsum(c(5, v))
  expect_invisible(draw_bars_base(cs[-1], from = cs[-4], labels = names(v), connect = TRUE, vline = 5))

  # Grouped bars
  m <- cbind(M1 = c(1, 2, 3), M2 = c(1.5, 1.8, 2.5))
  rownames(m) <- c("x1", "x2", "x3")
  expect_invisible(draw_bars_base(m, theme = "Set 1"))

  # Series (continuous & discrete) + points overlay
  x <- 1:5
  y <- cbind(1:5, 5:1)
  pts <- data.frame(x = c(2, 4), y = c(2, 2))
  expect_invisible(draw_series_base(x, y, theme = "Viridis", points_df = pts))
  expect_invisible(draw_series_base(letters[1:5], y, discrete = TRUE))

  # Points & overlay
  expect_invisible(draw_points_base(1:10, 1:10, theme = "Viridis", color_by = 1:10))
  expect_invisible(draw_points_base(c(2, 5), c(3, 6), overlay = TRUE, colour = "red"))

  # Surface
  g <- seq(-1, 1, length.out = 10)
  z <- outer(g, g)
  expect_invisible(draw_surface_base(g, g, z, theme = "azurerose", points_df = data.frame(x = 0, y = 0)))

  # Matrix & boxes
  expect_invisible(draw_matrix_base(cor(mtcars[, 1:3]), theme = "RdBu"))
  expect_invisible(draw_boxes_base(list(a = 1:5, b = 2:8), theme = "Set 2"))
})

test_that("ggplot2 backend returns valid ggplot objects for all 6 primitives", {
  skip_if_not_installed("ggplot2")

  v <- c(A = 1.5, B = -0.8, C = 0.3)
  expect_s3_class(draw_bars(v, theme = "azurerose", engine = "ggplot2"), "ggplot")
  expect_s3_class(draw_bars(v, type = "dotchart", theme = "Okabe-Ito", engine = "ggplot2"), "ggplot")
  cs <- cumsum(c(5, v))
  expect_s3_class(
    draw_bars(cs[-1], from = cs[-4], labels = names(v), connect = TRUE, vline = 5, engine = "ggplot2"),
    "ggplot"
  )

  x <- 1:5
  y <- cbind(1:5, 5:1)
  pts <- data.frame(x = c(2, 4), y = c(2, 2))
  expect_s3_class(draw_series(x, y, theme = "Viridis", points_df = pts, engine = "ggplot2"), "ggplot")

  expect_s3_class(draw_points(1:10, 1:10, theme = "Viridis", color_by = 1:10, engine = "ggplot2"), "ggplot")
  expect_true(inherits(draw_points(1:2, 1:2, overlay = TRUE, engine = "ggplot2"), "LayerInstance"))

  g <- seq(-1, 1, length.out = 10)
  z <- outer(g, g)
  expect_s3_class(draw_surface(g, g, z, theme = "azurerose", engine = "ggplot2"), "ggplot")
  expect_s3_class(draw_matrix(cor(mtcars[, 1:3]), theme = "RdBu", engine = "ggplot2"), "ggplot")
  expect_s3_class(draw_boxes(list(a = 1:5, b = 2:8), theme = "Set 2", engine = "ggplot2"), "ggplot")
})
