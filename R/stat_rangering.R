#' @format NULL
#' @usage NULL
#' @export
#' @importFrom geosphere destPoint
#' @importFrom ggplot2 aes Stat ggproto
#' @noRd
StatRangering <- ggproto("StatRangering", Stat,
                      required_aes = c("x", "y", "radius"),
                      default_aes = aes(colour = "black", fill = NULL, size = 0.5,
                                        linetype = 1, alpha = 1
                      ),
                      # setup_params = function(data, params){
                      #   browser()
                      # },
                      setup_data = function(data, params){
                        nms_data <- names(data)
                        nms_params <- names(params)
                        if(!("radius" %in% nms_data) && ("radius" %in% nms_params)){
                          data$radius <- params$radius
                        }
                        if(!("inner_radius" %in% nms_data) && ("inner_radius" %in% nms_params)){
                          data$inner_radius <- params$inner_radius
                        }
                        data$group <- 1:nrow(data)
                        data
                      },

                      compute_group = function(self, data, scales, radius, inner_radius,...){
                        m<- geosphere::destPoint(c(data$x, data$y),b=0:360, d=data$radius, a=6378137/1852)
                        m2 <- NULL
                        if(!is.null(data$inner_radius)&&!is.na(data$inner_radius))
                          m2<- geosphere::destPoint(c(data$x, data$y),b=360:0, d=data$inner_radius, a=6378137/1852)

                        rbind(data.frame(x=m[,"lon"], y=m[,"lat"]),data.frame(x=m2[,"lon"], y=m2[,"lat"]))
                      }
)



#' @title plot a range ring
#'
#' @description Range rings provide some perspective in plan view plots. This function
#' makes drawing a circle using sperical coordinates on a map simple.  \code{geom_rangering} is backed by \code{geom_polygon}
#' so it is possible to plot "donuts" by specifying and \code{inner_radius} (you will probably also want to specify \code{fill})
#'
#' \code{stat_rangering} and\code{stat_rangering} understand the following aesthetics (required aesthetics are in bold)
#' \itemize{
#' \item \bold{x}
#' \item \bold{y}
#' \item \bold{radius}
#' \item inner_radius
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
#' @rdname rangering
#'
#' @examples
#' # plot range ring about Denver
#' den_rr <- data.frame(x=-104.6732, y= 39.86167, radius=40)
#' ggplot(den_rr, aes(x=x,y=y, radius=radius)) + geom_rangering() + coord_map()
#'
#' # plot range donut about Denver
#' ggplot(den_rr, aes(x=x,y=y, radius=radius)) + geom_rangering(inner_radius=30, fill="blue") + coord_map()
#'
stat_rangering <- function(mapping=NULL, data=NULL, geom="polygon",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatRangering, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
