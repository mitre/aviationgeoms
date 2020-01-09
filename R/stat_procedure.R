#' @importFrom geosphere bearing distHaversine destPoint
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 aes Stat ggproto
#' @importFrom geosphere bearing distHaversine destPoint
#' @noRd
StatProcedure <- ggproto("StatProcedure", Stat,
                         default_aes = aes(x=NA,
                                           y=NA,
                                           leg_type=NA,
                                           rf_center_longitude=NA,
                                           rf_center_latitude=NA,
                                           turn_direction=NA,
                                           colour = "black", fill = NULL, size = 0.5,
                                           linetype = 1, alpha = 1
                         ),

                         compute_group = function(self, data, scales){
                           ggplot2:::check_required_aesthetics(c("x","y","leg_type"),names(data), ggplot2:::snake_class(self))
                           supported_types <- c("IF","TF", "RF")

                           if(!all(data$leg_type %in% supported_types)){ # contains an unsupported type like "FM" or "HM"
                             warning("in stat_geom, unsupported leg type: ",paste(unique(setdiff(data$leg_type, supported_types)),collapse=" "),ifelse(is.null(data$group),"",paste(" in group",data$group[1])))
                             data <- subset(data, leg_type!="HM") # remove all HM legs b/c they can be ignored safely
                             minidx <- which((data$leg_type %in% supported_types))[1] # first leg type we know about
                             if(is.na(minidx))# if we don't know about any leg types, return null
                               return(NULL)
                             if(minidx>1){ #back up one leg to try and get the initial fix for the start of the next leg
                               minidx<-minidx-1
                               data$leg_type[minidx] <- "IF"
                             }
                             maxidx <-min(nrow(data), which(!(data$leg_type %in% supported_types) & (1:nrow(data)>minidx))[1]-1, na.rm=T) # last known leg type which might be all of them
                             data <- data[minidx:maxidx,] # first continuous subset of known legs
                           }
                           if(all(data$leg_type %in% c("IF","TF"))) # contains only linear well defined leg types
                             return(data)
                           if(is.null(data$rf_center_latitude) | is.null(data$rf_center_longitude)| is.null(data$turn_direction)){
                             warning("procedure contains an RF leg but \'rf_center_latitude\' and \'rf_center_longitude\'and \'turn_direction\' aesthetics are not all present")
                             return(NULL)
                           }
                           df <- cbind(data[2:(nrow(data)),], data[1:(nrow(data)-1),c('x','y')])#[(nrow(data)-1):1,]
                           names(df) <- c('xend','yend',names(df[,-c(1,2)])) # change the names of the last two columns which are the leaded x and y
                           df$start_bearing <- (bearing(df[, c('rf_center_longitude','rf_center_latitude')], df[, c('x','y')]))
                           df$stop_bearing <- (bearing(df[, c('rf_center_longitude','rf_center_latitude')], df[, c('xend','yend')]))

                           df$radius_turn <- distHaversine(df[,c('rf_center_longitude', 'rf_center_latitude')],
                                                           df[,c('xend', 'yend')])

                           left_turn <- df$turn_direction=="L"
                           df$inc <- (-1)^left_turn
                           start_before_stop <- sign(df$stop_bearing-df$start_bearing)
                           start_before_stop[is.na(start_before_stop)] <- 0
                           df$start_bearing <- df$start_bearing + ifelse(left_turn, 360*(start_before_stop==1), 360*(start_before_stop==-1) )

                           transposed_df <- as.data.frame(t((df[,c("rf_center_longitude","rf_center_latitude", "radius_turn","start_bearing","stop_bearing", "inc")])))
                           get_points <- function(x){
                             if(!is.na(x[1])){

                               start <- x[4]
                               end <- x[5]
                               inc <- x[6]

                               #gets angles on 0-360 range (instead of -180 to 180)
                               if(start < 0)
                                 start <- 360 + start
                               if(end < 0)
                                 end <- 360 + end

                               #if start more than end for right turn then subtract 360 from start
                               if(start > end & inc == 1){
                                 start <- start - 360
                                 #if start is less than end for left turn subtract 360 from end
                               } else if (start < end & inc == -1){
                                 end <- end - 360
                               }

                               b <- seq(start, end, inc)

                               t(destPoint(x[1:2],d=x[3], b=b))
                             }
                           }


                           list_o_points <- lapply(transposed_df, get_points)

                           m <- vapply(1:(2*nrow(df)), list, list(0))
                           m[seq(1,length(m),2)] <- lapply(as.data.frame(t(df[,c('x','y')])), function(x){x})
                           m[seq(2,length(m),2)] <- lapply(list_o_points, function(x){x})

                           #  browser()
                           out_data <- data.frame(t(matrix(unlist(m), nrow=2)))
                           names(out_data) <- c("x","y")
                           out_data$group <- ifelse(is.null(data$group),runif(1),data$group[1])
                           out_data$PANEL <- data$PANEL[1]
                           return(out_data)
                         }
)


#' @title Display lines representing each procedure
#'
#' @description  Given a data.frame containing a list of SIDs, STARs, Approaches, and Enroute procedures, create a plot.  Currently only IF, TF, and RF legs are supported.
#' HM legs are removed.  If there are still unsupported leg types, the first continous subset of known leg types is plotted.
#'
#' If plotting more than one procedure you will generally want to specify the group aesthetic as \code{group=paste(procedure, transistion)}
#'
#' \code{stat_procedure} and \code{geom_procedure} understand the following aesthetics. Required aesthetics are in bold and if RF legs are present the italicized aesthetics are also required.
#' \itemize{
#' \item \bold{x}
#' \item \bold{y}
#' \item \bold{leg_type}
#' \item \emph{rf_center_longitude}
#' \item \emph{rf_center_latitude}
#' \item \emph{turn_direction}
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
#' \dontrun{
#' # given some procedure data with the latitude, longitude, procedure/transition names, and ARINC424 leg types
#' library(ggplot2)
#' ggplot(procs, aes(y=latitude, x=longitude, group=paste(procedure, transition), leg_type=leg_type)) + geom_procedure()
#' }
#' @rdname procedure
stat_procedure <- function(mapping=NULL, data=NULL, geom="path",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatProcedure, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
