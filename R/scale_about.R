#' control plot range about coordinate point
#'
#' Pick a center point and cut your plot to fit exactly what you want to look at.
#' This is actually done in the \code{scale_} functions of \code{ggplot2} which
#' makes sense but may not be immediately obvious.
#'
#' @inheritParams get_x_limits
#' @inheritParams get_y_limits
#' export
#' @noRd
coord_map_about <- function(latitude, longitude, north, east, south=north, west=east, ...) {
  return(coord_map(xlim = get_x_limits(latitude, longitude, east, west),
                   ylim = get_y_limits(latitude, longitude, north, south), ...))
}

#' get limits
#'
#' Given central coordinates and distances, determine the limits
#'
#' @inheritParams get_x_limits
#' @inheritParams get_y_limits
#' @return a \code{list} with x and y limits
#' export
#' @noRd
get_limits <- function(latitude, longitude, north, east, south=north, west=east) {
  return(list(xlims=get_x_limits(latitude, longitude, east, west),
              ylims=get_y_limits(latitude, longitude, north, south)))
}

#' get x limits
#'
#' Given central coordinates and distances, determine the limits of the x-scale
#'
#' @importFrom earthtools compute_projection
#' @param latitude numeric scalar, center point latitude in decimal degrees
#' @param longitude numeric scalar, center point longitude in decimal degrees
#' @param east numeric scalar, distance ease of center point to view plot in nautical miles
#' @param west numeric scalar, distance west of center point to view plot in nautical miles
#' @param xargs list, arguments passed to \code{\link[ggplot2]{scale_x_continuous}}
#' export
#' @noRd
get_x_limits <- function(latitude, longitude, east, west=east) {
  xlims <- compute_projection(latitude, longitude, c(270, 90), c(west, east), output_type="matrix")
  return(sort(xlims[, 2]))
}

#' get y limits
#'
#' Given central coordinates and distances, determine the limits of the y-scale
#'
#' @importFrom earthtools compute_projection
#' @param latitude numeric scalar, center point latitude in decimal degrees
#' @param longitude numeric scalar, center point longitude in decimal degrees
#' @param north numeric scalar, distance north of center point to view plot in nautical miles
#' @param south numeric scalar, distance south of center point to view plot in nautical miles
#' @param yargs list, arguments passed to \code{\link[ggplot2]{scale_y_continuous}}
#' export
#' @noRd
get_y_limits <- function(latitude, longitude, north, south=north) {
  ylims <- compute_projection(latitude, longitude, c(180, 0), c(south, north), output_type="matrix")
  return(sort(ylims[, 1]))
}
