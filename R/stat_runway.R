#' @format NULL
#' @usage NULL
#' @export
#' @importFrom earthtools et_bearing_initial et_projection
#' @importFrom ggplot2 aes Stat ggproto
#' @noRd
StatRunway <- ggproto("StatRunway", Stat,
                      required_aes = c("longitude_AER",
                                       "latitude_AER",
                                       "longitude_DER",
                                       "latitude_DER"),
                       default_aes = aes(x = ..x..,
                                         y = ..y..,
                                         colour = "black", fill = "black", size = 0.5,
                                         linetype = 1, alpha = 1
                       ),

                       et_group = function(data, scales){
                         if(!("width" %in% names(data)))
                           data$width <- 150
                         bearing <- et_bearing_initial(data$latitude_AER, data$longitude_AER,
                                                            data$latitude_DER, data$longitude_DER)
                         arrival_corner1 <- et_projection(output_type="data.frame",
                                                               data$latitude_AER, data$longitude_AER, distance=data$width/(2*6076.12), bearing=(bearing+90)%%360)
                         arrival_corner2 <- et_projection(output_type="data.frame",
                                                               data$latitude_AER, data$longitude_AER, distance=data$width/(2*6076.12), bearing=(bearing+270)%%360)
                         departure_corner1 <- et_projection(output_type="data.frame",
                                                                 data$latitude_DER, data$longitude_DER, distance=data$width/(2*6076.12), bearing=(bearing+270)%%360)
                         departure_corner2 <- et_projection(output_type="data.frame",
                                                                 data$latitude_DER, data$longitude_DER, distance=data$width/(2*6076.12), bearing=(bearing+90)%%360)
                          data.frame(x=c(arrival_corner1[,2], arrival_corner2[,2], departure_corner1[,2],
                                                                  departure_corner2[,2], arrival_corner1[,2]),
                                          y=c(arrival_corner1[,1], arrival_corner2[,1], departure_corner1[,1],
                                              departure_corner2[,1], arrival_corner1[,1])
                         )
                       }
)



#' @title plot runways
#'
#' @description Given runway names and end points lay down a polygon plot
#'
#' \code{stat_runway} and\code{stat_runway} understand the following aesthetics (required aesthetics are in bold).  The first five
#' correspond to the endpoints of the runway and it's width in feet.  these names match the column names of the data.frame returned
#' by \code{caasddb::fetch_runways()}
#' \itemize{
#' \item \bold{latitude_AER}
#' \item \bold{longitude_AER}
#' \item \bold{latitude_DER}
#' \item \bold{longitude_DER}
#' \item width
#' \item alpha
#' \item colour
#' \item fill
#' \item linetype
#' \item size
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link{ggplot}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link{fortify}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame.}, and
#'    will be used as the layer data.
#' @param geom The geometric object to use display the data
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param na.rm logical. Should this layer remove NA values
#' @param show.legnd logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param ... Additional parameters to the \code{geom} and \code{stat}.
#' @importFrom ggplot2 layer
#' @export
#'
#' @examples
#' # given some runway data with the latitude/longitude for the arrival/departure ends of the runway (AER/DER)
#' library(ggplot2)
#' ggplot(den_rwys, aes(latitude_AER=latitude_AER, longitude_AER=longitude_AER,latitude_DER=latitude_DER, longitude_DER=longitude_DER, group=runway))+stat_runway()
#'
#' @rdname runway
stat_runway <- function(mapping=NULL, data=NULL, geom="polygon",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatRunway, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
