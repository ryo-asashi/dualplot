# R/dualplot_gg.R
# ggplot2 layer constructors and high-level builders (ggplot2 in Suggests)

if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}

gg_common_params <- c(
  "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
)

make_gg_layer <- function(geom_name, allowed_aes) {
  allowed <- unique(c(allowed_aes, gg_common_params))
  function(mapping = NULL, data = NULL, ...) {
    check_ggplot2()
    geom_fun <- getExportedValue("ggplot2", geom_name)
    dots <- cast_to_gg_params(list(...), allowed = allowed)
    do.call(geom_fun, c(list(mapping = mapping, data = data), dots))
  }
}

#' Low-Level Filtered ggplot2 Layer Constructors
#' @name gg_layers
#' @param mapping set of aesthetic mappings created by \code{ggplot2::aes()}.
#' @param data a data frame to be displayed in this layer.
#' @param ... additional parameters (both base and ggplot2 names supported).
#' @return a \code{ggplot2} layer object (or list of layers for compound geoms).
#' @keywords internal
#'
layer_point <- make_gg_layer(
  "geom_point",
  c("colour", "alpha", "shape", "size", "fill", "stroke")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_line <- make_gg_layer(
  "geom_line",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_path <- make_gg_layer(
  "geom_path",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_linerange <- make_gg_layer(
  "geom_linerange",
  c("colour", "alpha", "linetype", "linewidth")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_col <- make_gg_layer(
  "geom_col",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "just")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_rect <- make_gg_layer(
  "geom_rect",
  c("colour", "fill", "alpha", "linewidth", "linetype", "linejoin")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_tile <- make_gg_layer(
  "geom_tile",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "height")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_raster <- make_gg_layer(
  "geom_raster",
  c("fill", "alpha", "interpolate", "hjust", "vjust")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_boxplot <- make_gg_layer(
  "geom_boxplot",
  c("colour", "fill", "alpha", "linewidth", "linetype", "shape", "size", "width",
    "outlier.colour", "outlier.fill", "outlier.shape", "outlier.size",
    "outlier.stroke", "outlier.alpha", "notch", "notchwidth", "varwidth")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_linepoint <- function(mapping = NULL, data = NULL, ...) {
  list(
    layer_line(mapping = mapping, data = data, ...),
    layer_point(mapping = mapping, data = data, ...)
  )
}

#' @rdname gg_layers
#' @keywords internal
#'
layer_dotchart <- function(mapping = NULL, data = NULL, ...) {
  check_ggplot2()
  dots <- cast_to_gg_params(list(...))
  line_dots <- dots[names(dots) != "colour"]
  tli <- ggplot2::theme_get()$line
  line_defaults <- list(
    colour    = tli$colour %||% "black",
    linetype  = 3L,
    linewidth = tli$linewidth %||% 0.5
  )
  line_args <- c(
    list(mapping = mapping, data = data),
    utils::modifyList(line_defaults, line_dots)
  )
  pt_mapping <- mapping
  pt_mapping$xmin <- pt_mapping$xmax <- pt_mapping$ymin <- pt_mapping$ymax <- NULL
  pt_args <- c(list(mapping = pt_mapping, data = data), dots)

  list(
    do.call(layer_linerange, line_args),
    do.call(layer_point, pt_args)
  )
}

#' @rdname gg_layers
#' @keywords internal
#'
layer_vline <- function(xintercept = 0, scale_lwd = 0.5, ...) {
  check_ggplot2()
  tli <- ggplot2::theme_get()$line
  defaults <- list(
    xintercept = xintercept,
    linewidth  = (tli$linewidth %||% 0.5) * scale_lwd,
    colour     = tli$colour %||% "black",
    linetype   = 1L
  )
  allowed <- c("xintercept", "colour", "linewidth", "linetype", "alpha", "na.rm", "show.legend")
  args <- cast_to_gg_params(list(...), allowed = allowed, defaults = defaults)
  do.call(ggplot2::geom_vline, args)
}

# --- High-level ggplot2 Builders (Symmetric with backend_base.R) ---

#' ggplot2 Bar, Dot, and Waterfall Charts
#' @inheritParams draw_bars_base
#' @return a \code{ggplot} object.
#' @export
#'
draw_bars_gg <- function(
    to, from = 0, labels = NULL, horizontal = TRUE,
    type = c("barplot", "dotchart", "none"), theme = NULL,
    theme.by = NULL, middle = 0, limits = NULL, width = NULL,
    vline = NULL, connect = FALSE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  check_ggplot2()
  type <- match.arg(type)
  dots <- cast_to_gg_params(list(...))

  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  labels <- labels %||% rownames(to) %||% as.character(seq_len(n))
  from <- as_param_matrix(from %||% 0, n, m)
  g_names <- colnames(to) %||% paste0("V", seq_len(m))

  cat_levels <- if (horizontal) rev(labels) else labels
  df <- data.frame(
    category = factor(rep(labels, times = m), levels = cat_levels),
    group    = factor(rep(g_names, each = n), levels = g_names),
    from     = as.vector(from),
    to       = as.vector(to),
    diff     = as.vector(to - from),
    stringsAsFactors = FALSE
  )

  use_theme <- !is.null(theme)
  if (use_theme) {
    ct <- colortheme::colortheme(theme)
    df$.cval <- if (!is.null(theme.by)) {
      rep_len(theme.by, nrow(df))
    } else if (m > 1L) {
      df$group
    } else if (connect && ct$type == "qualitative") {
      factor(ifelse(df$diff >= 0, "> 0", "< 0"), levels = c("> 0", "< 0"))
    } else {
      df$diff
    }
  }

  if (horizontal) {
    pl <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["to"]], y = .data[["category"]]))
  } else {
    pl <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["category"]], y = .data[["to"]]))
  }

  if (!is.null(vline)) {
    v_lty <- if (connect) 3L else 1L
    if (horizontal) {
      pl <- pl + layer_vline(xintercept = vline, linetype = v_lty)
    } else {
      pl <- pl + ggplot2::geom_hline(yintercept = vline, linewidth = 0.25, linetype = v_lty)
    }
  }

  if (type == "barplot") {
    w <- width %||% (if (connect) 0.6 else 0.9)
    if (connect && m == 1L) {
      hw <- w / 2
      df$idx <- as.integer(df$category)
      df$ymin <- df$idx - hw
      df$ymax <- df$idx + hw
      rect_aes <- ggplot2::aes(
        xmin = .data[["from"]], xmax = .data[["to"]],
        ymin = .data[["ymin"]], ymax = .data[["ymax"]]
      )
      if (use_theme) rect_aes$fill <- ggplot2::aes(fill = .data[[".cval"]])[[1L]]
      pl <- pl + layer_rect(mapping = rect_aes, data = df, ...)
      if (n > 1L) {
        seg_df <- df[-n, , drop = FALSE]
        seg_df$y_next <- df$ymin[-1L]
        tli <- ggplot2::theme_get()$line
        pl <- pl + layer_linerange(
          mapping = ggplot2::aes(
            x = .data[["to"]], ymin = .data[["y_next"]], ymax = .data[["ymax"]]
          ),
          data = seg_df,
          colour = dots$colour %||% tli$colour %||% "black",
          linetype = dots$linetype %||% 1L,
          linewidth = dots$linewidth %||% 0.5
        )
      }
    } else {
      bar_aes <- if (m > 1L) {
        ggplot2::aes(group = .data[["group"]])
      } else {
        ggplot2::aes()
      }
      if (use_theme) bar_aes$fill <- ggplot2::aes(fill = .data[[".cval"]])[[1L]]
      pos <- if (m > 1L) ggplot2::position_dodge(width = w) else "identity"
      col_args <- list(mapping = bar_aes, position = pos, width = w)
      if (!use_theme && is.null(dots$fill)) {
        col_args$fill <- "gray65"
      }
      pl <- pl + do.call(layer_col, c(col_args, list(...)))
    }
    if (use_theme) {
      pl <- pl + colortheme::scale_fill_theme(theme = ct, middle = middle)
    }
  } else if (type == "dotchart") {
    w <- width %||% 0.6
    dot_aes <- if (horizontal) {
      ggplot2::aes(xmin = .data[["from"]], xmax = .data[["to"]])
    } else {
      ggplot2::aes(ymin = .data[["from"]], ymax = .data[["to"]])
    }
    if (m > 1L) dot_aes$group <- ggplot2::aes(group = .data[["group"]])[[1L]]
    if (use_theme) dot_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
    pos <- if (m > 1L) ggplot2::position_dodge(width = w) else "identity"
    dot_args <- list(mapping = dot_aes, position = pos)
    if (!use_theme && is.null(dots$colour)) {
      dot_args$colour <- "gray35"
    }
    pl <- pl + do.call(layer_dotchart, c(dot_args, list(...)))
    if (use_theme) {
      pl <- pl + colortheme::scale_color_theme(theme = ct, middle = middle)
    }
  } else {
    pl <- pl + ggplot2::geom_blank()
  }

  if (!is.null(limits)) {
    if (horizontal) {
      pl <- pl + ggplot2::coord_cartesian(xlim = limits)
    } else {
      pl <- pl + ggplot2::coord_cartesian(ylim = limits)
    }
  }

  pl + extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}

#' ggplot2 Multi-Line Series and ICE Curves
#'
#' @inheritParams draw_series_base
#' @return a \code{ggplot} object.
#' @export
#'
draw_series_gg <- function(
    x, y, theme = NULL, theme.by = NULL, discrete = NULL,
    limits = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  check_ggplot2()
  y <- as.matrix(y)
  n <- nrow(y)
  m <- ncol(y)
  discrete <- discrete %||% is_discrete(x)
  x_vec <- if (discrete) factor(x, levels = unique(x)) else as.numeric(x)
  g_names <- colnames(y) %||% paste0("series_", seq_len(m))

  df <- data.frame(
    x     = rep(x_vec, times = m),
    y     = as.vector(y),
    group = factor(rep(g_names, each = n), levels = g_names),
    stringsAsFactors = FALSE
  )

  use_theme <- !is.null(theme)
  if (use_theme) {
    cval <- theme.by %||% g_names
    if (is.character(cval)) {
      cval <- factor(cval, levels = unique(cval))
    }
    df$.cval <- rep(cval, each = n)
  }

  base_aes <- ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[["group"]])
  if (use_theme) {
    base_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
  }

  pl <- ggplot2::ggplot(df, base_aes)
  pl <- pl + if (discrete) layer_linepoint(...) else layer_line(...)

  if (!is.null(points) && nrow(points) > 0L) {
    pdf <- points
    if (discrete) pdf$x <- factor(pdf$x, levels = levels(x_vec))
    pt_aes <- ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
    pl <- pl + layer_point(mapping = pt_aes, data = pdf, inherit.aes = FALSE, ...)
  }

  if (use_theme) {
    pl <- pl + colortheme::scale_color_theme(theme = theme)
  }
  if (!is.null(limits)) {
    pl <- pl + ggplot2::coord_cartesian(ylim = limits)
  }

  pl + extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}

#' ggplot2 Scatter and Jitter Plots
#'
#' @inheritParams draw_points_base
#' @return a \code{ggplot} object, or a \code{ggplot2} layer if \code{overlay = TRUE}.
#' @export
#'
draw_points_gg <- function(
    x, y, theme = NULL, theme.by = NULL, middle = 0,
    xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  check_ggplot2()
  df <- data.frame(x = x, y = y)
  use_theme <- !is.null(theme) && !is.null(theme.by)
  if (use_theme) df$.cval <- theme.by

  pt_aes <- ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
  if (use_theme) pt_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]

  if (overlay) {
    return(layer_point(mapping = pt_aes, data = df, inherit.aes = FALSE, ...))
  }

  pl <- ggplot2::ggplot(df, pt_aes) + layer_point(...)
  if (use_theme) {
    pl <- pl + colortheme::scale_color_theme(theme = theme, middle = middle)
  }
  if (!is.null(xticks)) {
    pl <- pl + ggplot2::scale_x_continuous(breaks = seq_along(xticks), labels = xticks)
  }
  if (!is.null(yticks)) {
    pl <- pl + ggplot2::scale_y_continuous(breaks = seq_along(yticks), labels = yticks)
  }
  if (!is.null(xlim) || !is.null(ylim)) {
    pl <- pl + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  pl + extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}

#' ggplot2 2D Interaction Surface / Raster Plot
#'
#' @inheritParams draw_surface_base
#' @return a \code{ggplot} object.
#' @export
#'
draw_surface_gg <- function(
    x, y, z, theme = NULL, zlim = NULL, middle = 0,
    xticks = NULL, yticks = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  check_ggplot2()
  ct <- colortheme::colortheme(theme %||% getOption("colortheme.diverging", "azurerose"))
  zlim <- zlim %||% range(z, na.rm = TRUE)
  if (ct$type == "diverging") {
    zmax <- max(abs(zlim - middle))
    zlim <- c(-zmax, zmax) + middle
  }

  grid <- expand.grid(x = x, y = y)
  grid$z <- as.vector(z)

  pl <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["z"]])) +
    layer_raster(...) +
    colortheme::scale_fill_theme(theme = ct, middle = middle, limits = zlim)

  if (!is.null(points) && nrow(points) > 0L) {
    pl <- pl + layer_point(
      mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      data = points, inherit.aes = FALSE, ...
    )
  }
  if (!is.null(xticks)) {
    pl <- pl + ggplot2::scale_x_continuous(breaks = seq_along(xticks), labels = xticks)
  }
  if (!is.null(yticks)) {
    pl <- pl + ggplot2::scale_y_continuous(breaks = seq_along(yticks), labels = yticks)
  }
  pl + extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}

#' ggplot2 Matrix Heatmap
#'
#' @inheritParams draw_matrix_base
#' @return a \code{ggplot} object.
#' @export
#'
draw_matrix_gg <- function(
    z, labels = rownames(z), theme = NULL,
    main = NULL, sub = NULL, xlab = "", ylab = "", ...
) {
  check_ggplot2()
  m <- nrow(z)
  labels <- labels %||% as.character(seq_len(m))
  ct <- colortheme::colortheme(theme %||% getOption("colortheme.sequential", "grayscale"))

  grid <- expand.grid(
    x = factor(labels, levels = labels),
    y = factor(labels, levels = labels)
  )
  grid$z <- as.vector(z)

  ggplot2::ggplot(grid, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["z"]])) +
    layer_tile(...) +
    colortheme::scale_fill_theme(theme = ct) +
    ggplot2::coord_fixed() +
    extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}

#' ggplot2 Boxplots
#'
#' @inheritParams draw_boxes_base
#' @return a \code{ggplot} object.
#' @export
#'
draw_boxes_gg <- function(
    x, theme = NULL, theme.by = NULL, horizontal = TRUE,
    main = NULL, sub = NULL, xlab = "", ylab = NULL, ...
) {
  check_ggplot2()
  nms <- names(x) %||% as.character(seq_along(x))
  use_theme <- !is.null(theme)
  cvals <- if (use_theme) (theme.by %||% factor(nms, levels = nms)) else NULL

  dfs <- lapply(seq_along(x), function(i) {
    d <- data.frame(
      category = factor(nms[i], levels = nms),
      value    = x[[i]],
      stringsAsFactors = FALSE
    )
    if (use_theme) d$.cval <- cvals[i]
    d
  })
  df <- do.call(rbind, dfs)

  box_aes <- if (horizontal) {
    ggplot2::aes(x = .data[["value"]], y = .data[["category"]])
  } else {
    ggplot2::aes(x = .data[["category"]], y = .data[["value"]])
  }
  if (use_theme) box_aes$fill <- ggplot2::aes(fill = .data[[".cval"]])[[1L]]

  pl <- ggplot2::ggplot(df, box_aes) + layer_boxplot(...)
  if (use_theme) {
    pl <- pl + colortheme::scale_fill_theme(theme = theme)
  }
  pl + extract_gg_labs(list(...), xlab = xlab, ylab = ylab, main = main, sub = sub)
}
