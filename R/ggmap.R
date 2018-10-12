#' convienance function to get ggmap
#'
#' This function wraps \code{\link[ggmap]{get_map}}. The main advantage here is that
#' you can define a map using a center location and a radius from that location.
#'
#' @importFrom ggmap make_bbox get_map
#' @importFrom earthtools compute_projection
#' @param center_latitude latitude of center of map
#' @param center_longitude longitude of center of map
#' @param radius approximate center to edge (via cardinal direction) distance [NM]
#' @param map_type see \code{\link[ggmap]{get_map}}
#' @param map_source see \code{\link[ggmap]{get_map}}
#' @param buffer_fraction see \code{\link[ggmap]{get_map}}
#' @param ... passed to \code{\link[ggmap]{get_map}}
#' @export
get_ggmap <- function(center_latitude, center_longitude, radius,
                      map_type="terrain", map_source="google", buffer_fraction=0, ...) {
  
  # get bounding box
  corners <- compute_projection(center_latitude, center_longitude, c(0,90,180,270), radius)
  box <- make_bbox(corners$longitude, corners$latitude, f=buffer_fraction)
  
  # get map
  map <- get_map(location=box, maptype=map_type, source=map_source, ...)
  return(map)
}
