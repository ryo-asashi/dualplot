# R/utils.R
# Internal utilities, namespace checks, and aesthetic scaling helpers

`%||%` <- function(x, y) if (is.null(x)) y else x

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

check_ggplot2 <- function(call = sys.call(-1L)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "package 'ggplot2' is required for engine = 'ggplot2': ",
      "please install it or use engine = 'graphics'",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

rescale_unit <- function(x) {
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x) || is.logical(x)) x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  d <- rng[2L] - rng[1L]
  if (!is.finite(d) || d == 0) {
    return(ifelse(is.na(x), NA_real_, 0.5))
  }
  pmax(0, pmin(1, (x - rng[1L]) / d))
}

#' Map Data Values to Base Graphics Aesthetics
#'
#' Helper functions to map numeric or discrete vectors to alpha transparency,
#' line types, or line widths for base graphics.
#'
#' @param x a vector of values to map.
#' @param range numeric vector of length 2 specifying the target range.
#'
#' @return a numeric or integer vector of mapped aesthetic values.
#' @keywords internal
#'
map_to_alpha <- function(x, range = c(0.25, 1)) {
  u <- rescale_unit(x)
  u * diff(range) + range[1L]
}

#' @rdname map_to_alpha
#' @keywords internal
#'
map_to_linetype <- function(x) {
  max_lty = 6L
  if (is_discrete(x)) {
    idx <- as.integer(as.factor(x))
    return(((idx - 1L) %% max_lty) + 1L)
  }
  u <- rescale_unit(x)
  as.integer(pmin(floor(u * max_lty) + 1L, max_lty))
}

#' @rdname map_to_alpha
#' @keywords internal
#'
map_to_linewidth <- function(x, range = c(0.5, 3)) {
  u <- rescale_unit(x)
  u * diff(range) + range[1L]
}

# Resolve colors using colortheme::map_to_colors when theme is supplied
resolve_base_colors <- function(
    values, theme = NULL, default = "gray35", discrete = NULL, middle = 0
) {
  if (is.null(theme)) {
    return(default)
  }
  colortheme::map_to_colors(
    x = values, theme = theme, discrete = discrete, middle = middle
  )
}
