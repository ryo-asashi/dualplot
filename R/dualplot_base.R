# R/dualplot_base.R
# Base graphics rendering primitives

adjust_left_margin <- function(labels, pad = 1 / 16) {
  mai <- graphics::par("mai")
  if (is.null(labels) || length(labels) == 0L) return(mai)
  cex <- graphics::par("cex.axis")
  req <- max(
    graphics::strwidth(as.character(labels), units = "inches", cex = cex),
    na.rm = TRUE
  )
  req <- mai[4L] + req + pad
  if (mai[2L] < req) mai[2L] <- req
  mai
}

as_param_matrix <- function(x, nrow, ncol) {
  if (is.matrix(x) && nrow(x) == nrow && ncol(x) == ncol) {
    return(x)
  } else if (length(x) == ncol && ncol > 1L) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = TRUE)
  } else if (length(x) == nrow || length(x) == 1L) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = FALSE)
  } else {
    matrix(rep_len(x, nrow * ncol), nrow = nrow, ncol = ncol)
  }
}

#' Base Graphics Bar, Dot, and Waterfall Charts
#'
#' @param to numeric vector or matrix (\code{nrow} categories by \code{ncol} groups) of end values.
#' @param from numeric vector or matrix of start values (default \code{0}).
#' @param labels character vector of category labels (length \code{nrow(to)}).
#' @param horizontal logical; if \code{TRUE}, bars/dots are drawn horizontally (top-to-bottom).
#' @param type one of \code{"barplot"}, \code{"dotchart"}, or \code{"none"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.by optional vector or matrix used to map colors via \code{theme}.
#'   Defaults to \code{to - from} when \code{ncol(to) == 1}, or group indices when \code{ncol(to) > 1}.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param limits optional numeric vector of length 2 for value axis limits.
#' @param width optional numeric bar/dodge width.
#' @param vline optional numeric intercept for a reference line.
#' @param connect logical; if \code{TRUE}, draws step lines connecting bars (for waterfall charts).
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... additional graphical parameters (base or ggplot2 names supported).
#'
#' @return invisibly returns \code{NULL}.
#' @export
#'
draw_bars_base <- function(
    to, from = 0, labels = NULL, horizontal = TRUE,
    type = c("barplot", "dotchart", "none"), theme = NULL,
    theme.by = NULL, middle = 0, limits = NULL, width = NULL,
    vline = NULL, connect = FALSE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  type <- match.arg(type)
  dots <- cast_to_base_params(dots = list(...), defaults = list(), alpha.on = NULL)

  main <- dots$main %||% main
  sub  <- dots$sub  %||% sub
  xlab <- dots$xlab %||% xlab
  ylab <- dots$ylab %||% ylab

  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  labels <- labels %||% rownames(to) %||% as.character(seq_len(n))
  from <- as_param_matrix(from %||% 0, n, m)

  if (horizontal) {
    opar <- graphics::par(c("mai", "las"))
    on.exit(graphics::par(opar), add = TRUE)
    graphics::par(mai = adjust_left_margin(labels), las = 1L)
  }

  # Resolve theme colors if theme is provided and explicit fill/col is not
  theme_cols <- NULL
  if (!is.null(theme)) {
    ct <- colortheme::colortheme(theme)
    cval <- theme.by %||% if (m > 1L) {
      factor(rep(colnames(to) %||% seq_len(m), each = n),
             levels = colnames(to) %||% as.character(seq_len(m)))
    } else {
      as.vector(to - from)
    }
    theme_cols <- colortheme::map_to_colors(cval, theme = ct, middle = middle)
  }

  if (type == "barplot") {
    raw_fill <- dots$fill %||% theme_cols %||% "gray65"
    raw_col  <- dots$border %||% dots$col %||% NA
    fill_mat <- as_param_matrix(apply_alpha(raw_fill, dots$alpha), n, m)
    col_mat  <- as_param_matrix(raw_col, n, m)
  } else {
    raw_col  <- dots$col %||% theme_cols %||% "gray35"
    col_mat  <- as_param_matrix(apply_alpha(raw_col, dots$alpha), n, m)
  }
  pch_mat <- as_param_matrix(dots$pch %||% 16L, n, m)
  cex_val <- dots$cex %||% 1
  lty_val <- dots$lty %||% (if (type == "dotchart") 3L else 1L)
  lwd_val <- dots$lwd %||% 1L

  rng <- range(c(to, from, vline), na.rm = TRUE)
  mgn <- if (!is.finite(diff(rng)) || diff(rng) == 0) 0.5 else abs(diff(rng)) * 0.02
  limits <- limits %||% c(rng[1L] - mgn, rng[2L] + mgn)
  at <- if (horizontal) rev(seq_len(n)) else seq_len(n)

  graphics::plot.new()
  graphics::plot.window(
    xlim = if (horizontal) limits else c(0.5, n + 0.5),
    ylim = if (horizontal) c(0.5, n + 0.5) else limits
  )
  graphics::box()
  graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  graphics::axis(side = if (horizontal) 1L else 2L)
  graphics::axis(side = if (horizontal) 2L else 1L, at = at, labels = labels)

  if (!is.null(vline)) {
    v_lty <- if (connect) 3L else 1L
    if (horizontal) {
      graphics::abline(v = vline, lwd = 0.5, lty = v_lty, col = graphics::par("fg"))
    } else {
      graphics::abline(h = vline, lwd = 0.5, lty = v_lty, col = graphics::par("fg"))
    }
  }

  if (type == "barplot") {
    bw <- (width %||% (if (connect) 0.6 else 0.9)) / m
    offsets <- (seq_len(m) - (m + 1) / 2) * bw
    centers <- outer(at, offsets, "+")
    hw <- bw / 2

    if (horizontal) {
      graphics::rect(
        xleft = as.vector(from), xright = as.vector(to),
        ybottom = as.vector(centers - hw), ytop = as.vector(centers + hw),
        col = as.vector(fill_mat), border = as.vector(col_mat),
        lty = lty_val, lwd = lwd_val
      )
      if (connect && n > 1L && m == 1L) {
        seg_col <- if (all(is.na(col_mat))) graphics::par("fg") else col_mat[-n, 1L]
        graphics::segments(
          x0 = to[-n, 1L], y0 = at[-n] + hw,
          x1 = to[-n, 1L], y1 = at[-1L] - hw,
          col = seg_col, lty = lty_val, lwd = lwd_val
        )
      }
    } else {
      graphics::rect(
        xleft = as.vector(centers - hw), xright = as.vector(centers + hw),
        ybottom = as.vector(from), ytop = as.vector(to),
        col = as.vector(fill_mat), border = as.vector(col_mat),
        lty = lty_val, lwd = lwd_val
      )
    }
  } else if (type == "dotchart") {
    bw <- (width %||% 0.6) / m
    offsets <- (seq_len(m) - (m + 1) / 2) * bw
    centers <- outer(at, offsets, "+")
    lcol <- graphics::par("fg") %||% "black"

    if (horizontal) {
      graphics::segments(
        x0 = as.vector(from), y0 = as.vector(centers),
        x1 = as.vector(to),   y1 = as.vector(centers),
        col = lcol, lty = lty_val, lwd = lwd_val
      )
      graphics::points(
        x = as.vector(to), y = as.vector(centers),
        col = as.vector(col_mat), pch = as.vector(pch_mat), cex = cex_val
      )
    } else {
      graphics::segments(
        x0 = as.vector(centers), y0 = as.vector(from),
        x1 = as.vector(centers), y1 = as.vector(to),
        col = lcol, lty = lty_val, lwd = lwd_val
      )
      graphics::points(
        x = as.vector(centers), y = as.vector(to),
        col = as.vector(col_mat), pch = as.vector(pch_mat), cex = cex_val
      )
    }
  }
  invisible(NULL)
}

#' Base Graphics Multi-Line Series and ICE Curves
#'
#' @param x vector of x-axis values (numeric, factor, or character).
#' @param y numeric matrix of y-values (\code{nrow(y) == length(x)}).
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.by optional vector (length \code{ncol(y)}) mapped to line colors via \code{theme}.
#' @param discrete logical; if \code{NULL}, inferred automatically from \code{x}.
#' @param limits optional numeric vector of length 2 for y-axis limits.
#' @param points optional \code{data.frame} with columns \code{x} and \code{y} (and optional \code{col}, \code{pch}, \code{cex})
#'   for overlaying observation points.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... additional graphical parameters.
#'
#' @return invisibly returns \code{NULL}.
#' @export
#'
draw_series_base <- function(
    x, y, theme = NULL, theme.by = NULL, discrete = NULL,
    limits = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  y <- as.matrix(y)
  m <- ncol(y)
  discrete <- discrete %||% is_discrete(x)
  x_num <- if (discrete) seq_along(x) else as.numeric(x)
  ord <- if (discrete) seq_along(x_num) else order(x_num)

  dots <- list(...)
  g_names <- colnames(y) %||% paste0("series_", seq_len(m))
  theme_cols <- NULL
  if (!is.null(theme)) {
    cval <- theme.by %||% factor(g_names, levels = g_names)
    theme_cols <- colortheme::map_to_colors(cval, theme = theme)
  }

  defaults <- list(
    x = x_num[ord],
    y = y[ord, , drop = FALSE],
    type = if (discrete) "b" else "l",
    pch = 16L, lty = 1L, lwd = 1L,
    col = theme_cols %||% "black",
    main = main, sub = sub, xlab = xlab, ylab = ylab,
    ylim = limits %||% range(c(y, points$y), na.rm = TRUE),
    xaxt = if (discrete) "n" else "s"
  )
  args <- cast_to_base_params(dots = dots, defaults = defaults, alpha.on = "col")
  do.call(graphics::matplot, args)

  if (discrete) {
    graphics::axis(side = 1L, at = x_num, labels = as.character(x))
  }
  if (!is.null(points) && nrow(points) > 0L) {
    px <- if (discrete) {
      if (is.numeric(points$x)) points$x else match(as.character(points$x), as.character(x))
    } else {
      points$x
    }
    p_defaults <- list(
      x = px,
      y = points$y,
      pch = points$pch %||% args$pch %||% 16L,
      col = points$col %||% args$col,
      cex = points$cex %||% args$cex %||% 1
    )
    p_dots <- dots[!names(dots) %in% c("lty", "lwd", "type", "main", "sub", "xlab", "ylab")]
    p_args <- cast_to_base_params(
      dots = p_dots, defaults = p_defaults, alpha.on = "col",
      allowed = c("x", "y", "col", "pch", "cex", "bg")
    )
    do.call(graphics::points, p_args)
  }
  invisible(NULL)
}

#' Base Graphics Scatter and Jitter Plots
#'
#' @param x,y numeric vectors of coordinates.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.by optional vector mapped to point colors via \code{theme}.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param xticks,yticks optional character vectors for discrete axis ticks.
#' @param xlim,ylim optional axis limits.
#' @param overlay logical; if \code{TRUE}, adds points to an existing plot via \code{graphics::points()}.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... additional graphical parameters.
#'
#' @return invisibly returns \code{NULL}.
#' @export
#'
draw_points_base <- function(
    x, y, theme = NULL, theme.by = NULL, middle = 0,
    xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  theme_cols <- NULL
  if (!is.null(theme) && !is.null(theme.by)) {
    theme_cols <- colortheme::map_to_colors(theme.by, theme = theme, middle = middle)
  }

  if (overlay) {
    defaults <- list(
      x = x, y = y, col = theme_cols %||% 1L, pch = 16L, cex = 1
    )
    args <- cast_to_base_params(
      dots = list(...),
      defaults = defaults,
      allowed = c("x", "y", "col", "pch", "cex", "bg"),
      alpha.on = "col"
    )
    do.call(graphics::points.default, args)
    return(invisible(NULL))
  }

  has_custom_axes <- !is.null(xticks) || !is.null(yticks)
  defaults <- list(
    x = x, y = y, type = "p",
    col = theme_cols %||% 1L, pch = 16L, cex = 1,
    xlim = xlim %||% (if (!is.null(xticks)) c(0.5, length(xticks) + 0.5) else NULL),
    ylim = ylim %||% (if (!is.null(yticks)) c(0.5, length(yticks) + 0.5) else NULL),
    xlab = xlab, ylab = ylab, main = main, sub = sub,
    axes = !has_custom_axes
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "col")
  do.call(graphics::plot.default, args)

  if (has_custom_axes) {
    graphics::box()
    if (!is.null(xticks)) {
      graphics::axis(1L, at = seq_along(xticks), labels = xticks)
    } else {
      graphics::axis(1L)
    }
    if (!is.null(yticks)) {
      graphics::axis(2L, at = seq_along(yticks), labels = yticks)
    } else {
      graphics::axis(2L)
    }
  }
  invisible(NULL)
}

#' Base Graphics 2D Interaction Surface / Filled Contour Plot
#'
#' @param x,y numeric vectors of grid coordinates.
#' @param z numeric matrix of surface values (\code{length(x)} by \code{length(y)}).
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param zlim optional numeric vector of length 2 for color scale limits.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param xticks,yticks optional character vectors for discrete axis ticks.
#' @param points optional \code{data.frame} with \code{x} and \code{y} columns to overlay.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... additional graphical parameters.
#'
#' @return invisibly returns \code{NULL}.
#' @export
#'
draw_surface_base <- function(
    x, y, z, theme = NULL, zlim = NULL, middle = 0,
    xticks = NULL, yticks = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  ct <- colortheme::colortheme(theme %||% getOption("colortheme.diverging", "azurerose"))
  zlim <- zlim %||% range(z, na.rm = TRUE)
  if (ct$type == "diverging") {
    zmax <- max(abs(zlim - middle))
    zlim <- c(-zmax, zmax) + middle
  }

  dots <- cast_to_base_params(dots = list(...), defaults = list(), alpha.on = NULL)
  pal <- if (!is.null(dots$alpha)) {
    function(n) apply_alpha(ct$palette(n), dots$alpha)
  } else {
    ct$palette
  }

  pt_args <- NULL
  if (!is.null(points) && nrow(points) > 0L) {
    pt_args <- cast_to_base_params(
      dots = dots,
      defaults = list(
        x = points$x, y = points$y,
        pch = points$pch %||% 16L,
        col = points$col %||% 1L,
        cex = points$cex %||% 1
      ),
      allowed = c("x", "y", "col", "pch", "cex", "bg"),
      alpha.on = "col"
    )
  }

  plot_axes <- substitute({
    graphics::title(main = "", xlab = "", ylab = "")
    if (!is.null(xticks)) {
      graphics::axis(1L, at = seq_along(xticks), labels = xticks)
    } else {
      graphics::axis(1L)
    }
    if (!is.null(yticks)) {
      graphics::axis(2L, at = seq_along(yticks), labels = yticks)
    } else {
      graphics::axis(2L)
    }
    if (!is.null(pt_args)) {
      do.call(graphics::points.default, pt_args)
    }
  })

  fc_args <- list(
    x = x, y = y, z = z, zlim = zlim,
    xlab = dots$xlab %||% xlab,
    ylab = dots$ylab %||% ylab,
    main = dots$main %||% main,
    color.palette = pal,
    plot.axes = plot_axes,
    las = graphics::par("las")
  )
  do.call(graphics::filled.contour, fc_args)
  invisible(NULL)
}

#' Base Graphics Matrix Heatmap
#'
#' @param z numeric matrix of values to plot.
#' @param labels character vector of row/column labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... additional graphical parameters.
#'
#' @return invisibly returns \code{NULL}.
#' @export
#'
draw_matrix_base <- function(
    z, labels = rownames(z), theme = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  m <- nrow(z)
  labels <- labels %||% as.character(seq_len(m))
  opar <- graphics::par(c("mai", "las"))
  on.exit(graphics::par(opar), add = TRUE)
  graphics::par(mai = adjust_left_margin(labels), las = 1L)

  ct <- colortheme::colortheme(theme %||% getOption("colortheme.sequential", "grayscale"))
  palette <- ct$palette(12L)

  defaults <- list(
    x = seq_len(m), y = seq_len(m), z = z,
    fill = palette, col = NA, axes = FALSE,
    main = main, sub = sub, xlab = xlab, ylab = ylab,
    lty = 1L, lwd = 1L
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "fill")
  grid_col <- args$col[1L]
  args$col <- args$fill
  args$fill <- NULL

  img_allowed <- c("x", "y", "z", "col", "zlim", "xlim", "ylim", "axes", "xlab", "ylab", "main", "sub")
  do.call(graphics::image.default, args[names(args) %in% img_allowed])

  at <- seq_len(m + 2L) - 1L
  graphics::axis(1L, at = at, labels = c("", labels, ""))
  graphics::axis(2L, at = at, labels = c("", labels, ""))
  graphics::axis(3L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  graphics::axis(4L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  if (!is.na(grid_col)) {
    graphics::abline(
      v = seq_len(m) - 0.5, h = seq_len(m) - 0.5,
      col = grid_col, lty = args$lty, lwd = args$lwd
    )
  }
  invisible(NULL)
}

#' Base Graphics Boxplots
#'
#' @param x A named list of numeric vectors (one per category).
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.by optional vector (length \code{length(x)}) mapped to box fills via \code{theme}.
#' @param horizontal Logical; if \code{TRUE}, boxes are drawn horizontally.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
draw_boxes_base <- function(
    x, theme = NULL, theme.by = NULL, horizontal = TRUE,
    main = NULL, sub = NULL, xlab = "", ylab = NULL, ...
) {
  if (horizontal) {
    opar <- graphics::par(c("mai", "las"))
    on.exit(graphics::par(opar), add = TRUE)
    graphics::par(mai = adjust_left_margin(names(x)), las = 1L)
  }
  nms <- names(x) %||% as.character(seq_along(x))
  theme_cols <- NULL
  if (!is.null(theme)) {
    cval <- theme.by %||% factor(nms, levels = nms)
    theme_cols <- colortheme::map_to_colors(cval, theme = theme)
  }
  defaults <- list(
    x = x, fill = theme_cols %||% NA, col = "black", pch = 16L,
    main = main, sub = sub, xlab = xlab, ylab = ylab,
    horizontal = horizontal
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "fill")
  args$border <- args$col
  args$col <- args$fill
  args$fill <- NULL
  args$boxwex <- args$boxwex %||% args$width
  args$width <- NULL
  do.call(graphics::boxplot.default, args)
  invisible(NULL)
}
