# Copyright 2016-18 Seth Wenchel

#' @format NULL
#' @usage NULL
#' @importFrom geohash gh_decode
#' @export
#' @noRd
StatGeohash <- ggproto("StatGeohash", Stat,
                       default_aes = aes(xmin = ..xmin..,
                                         ymin= ..ymin..,
                                         xmax = ..xmax..,
                                         ymax= ..ymax..,
                                         geohash= NA,
                                         colour = NA, fill = "grey20", size = 0.5,
                                         linetype = 1, alpha = 1
                       ),
                       compute_group = function(data, scales){
                         if(!("geohash" %in% names(data)))
                           stop("missing aesthetic \'geohash\' from stat_geohash")
                         lat_lng_errs <- gh_decode(data$geohash)  
                         out <- data.frame(xmin=lat_lng_errs$lng-lat_lng_errs$lng_error,
                                           xmax=lat_lng_errs$lng+lat_lng_errs$lng_error,
                                           ymin=lat_lng_errs$lat-lat_lng_errs$lat_error,
                                           ymax=lat_lng_errs$lat+lat_lng_errs$lat_error, 
                                           geohash=data$geohash,
                                           stringsAsFactors = FALSE)
                         return(out)
                       }
)

#' @title Display rectangles representing each geohash
#' 
#' @details Given a data.frame containing a list of geohash character strings, plot rectangles corresponding to the error boxes for each geohash.
#' 
#' \code{stat_geohash} understands the following aesthetics (required aesthetics are in bold)
#' \itemize{
#' \item \bold{geohash}
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
#' df <- data.frame(geohash=c("SSW","SIW"), stringsAsFactors = FALSE)
#' library(ggplot2)
#' ggplot(df, aes(geohash=geohash))+stat_geohash()
#' ggplot(df, aes(geohash=geohash))+geom_rect(stat="geohash") +coord_quickmap()
#' 
stat_geohash <- function(mapping=NULL, data=NULL, geom="rect",
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatGeohash, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
