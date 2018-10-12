#' @export
#' @rdname procedure
#' @importFrom ggplot2 layer GeomPath
geom_procedure <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatProcedure, geom = GeomPath, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
