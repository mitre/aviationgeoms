#' @export
#' @importFrom ggplot2 layer GeomPolygon
#' @rdname rangering
geom_rangering <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatRangering, geom = GeomPolygon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

