#' Timeline
#'
#' Timeline Visualisation of Earthquake data
#'
#' The timeline geom visualises Earthquakes as points on one or multiple parallel timelines.
#' The size of the points as well as the colour can be used to visualize additional variables.
#'
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following asthetics:
#' \itemize{
#'  \item{"x"}{ Variable (normally date) positioning the earthquake on the timeline}
#'  \item{"y} { Variable for plotting data on multiple parallel timelines}
#'  \item{"size"}{ Radius of the points on the timeline}
#'  \item{"shape"}{ Shape of the points on the timeline}
#'  \item{"Colour"}{ Colour of the points on the timeline}
#' }
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param geom The geometric object to use display the data
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param check.aes,check.param If `TRUE`, the default, will check that
#'   supplied parameters and aesthetics are understood by the `geom` or
#'   `stat`. Use `FALSE` to suppress the checks.
#' @param params Additional parameters to the `geom` and `stat`.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @importFrom grid linesGrob
#' @importFrom grid gTree
#' @importFrom grid pointsGrob
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 .stroke
#' @importFrom scales alpha
#' 
#' @export
#' @examples
#' ggplot(data = df[COUNTRY %in% c("CHINA","USA") & YEAR >= 2000], aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x"),
                        non_missing_aes = c("size", "shape", "colour","y"),
                        default_aes = ggplot2::aes(
                          shape = 19, colour = "black", size = 5, fill = NA,
                          alpha = 0.2, stroke = 0.5, y = 0.2
                        ),
                        
                        draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                          coords <- coord$transform(data, panel_params)
                          points = grid::pointsGrob(
                            coords$x, coords$y,
                            pch = coords$shape,
                            gp = gpar(
                              col = scales::alpha(coords$colour, coords$alpha),
                              fill = scales::alpha(coords$fill, coords$alpha),
                              # Stroke is added around the outside of the point
                              fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                              lwd = coords$stroke * ggplot2::.stroke / 2
                            )
                          )
                          lines = lapply(unique(coords$y), FUN = function(x) grid::linesGrob(y = c(x,x)))
                          grobList = c(list(points), lines)
                          grid::gTree(children = do.call(gList, grobList))
                        },
                        draw_key = ggplot2::draw_key_point
)
#' Timeline Labels
#'
#' Add Labels to Timeline Visualisation
#'
#' The timeline_label geom adds a text variable as lable to the most significant earthquakes.
#'
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} understands the following asthetics:
#' \itemize{
#'  \item{"x"}{ Variable (normally date) positioning the labels on the timeline}
#'  \item{"y} {Variable for adding labels to multiple parallel timelines}
#'  \item{"label"}{Labels to be added to timeline points}
#'  \item{"magnitude"}{Magnitude variable according to which to determine the top nmax earthquakes}
#' }
#' @param nudge_y Amount by which to shift the label on the y axis relative to the point
#' @param nudge_x Amount by which to shift the label on the x axis relative to the point
#' @param nmax Number of earthquakes to be labeled. (Will be top earthquakes according to magnitude aesthetic)
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param geom The geometric object to use display the data
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param check.aes,check.param If `TRUE`, the default, will check that
#'   supplied parameters and aesthetics are understood by the `geom` or
#'   `stat`. Use `FALSE` to suppress the checks.
#' @param params Additional parameters to the `geom` and `stat`.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @importFrom grid linesGrob
#' @importFrom grid gTree
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 position_nudge
#' @importFrom scales alpha
#' 
#' @export
#' @examples
#' ggplot(data = df[COUNTRY %in% c("CHINA","USA") & YEAR >= 2000], aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2) + 
#'     geom_timeline(alpha = 0.2) + geom_timeline_label(aes(magnitude = INTENSITY, label = LOCATION_NAME), nudge_y = 0.2,)
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                parse = FALSE,
                                nudge_x = 0,
                                nudge_y = 0,
                                check_overlap = FALSE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                nmax = 10){
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      nmax = nmax,
      ...
    )
  )
}
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "y", "label","magnitude"),
                             
                             default_aes = ggplot2::aes(
                               fontsize = 2, angle = 45, alpha = NA, family = "",
                               fontface = 1, lineheight = 1.2
                             ),
                             
                             draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                   na.rm = FALSE, check_overlap = FALSE, nmax = 3) {
                               lab <- data$label
                               if (parse) {
                                 lab <- parse(text = as.character(lab))
                               }
                               data <- coord$transform(data, panel_params)
                               levels <- length(unique(data$y))
                               print(levels)
                               data <- data[order(data$magnitude, decreasing = TRUE),]
                               data <- data[1:min(nrow(data), nmax),]
                               texts = grid::textGrob(
                                 data$label,
                                 data$x, data$y, default.units = "native",
                                 just = "left",
                                 rot = data$angle,
                                 gp = grid::gpar(
                                   col = scales::alpha(data$colour, data$alpha),
                                   fontsize = data$fontsize * ggplot2::.pt,
                                   fontfamily = data$family,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight
                                 ),
                                 check.overlap = check_overlap
                               )
                               grobList = list(texts)
                               for(i in 1:nrow(data)){
                                 new_line = grid::linesGrob(x = c(data[i,]$x,data[i,]$x), y = c(data[i,]$y - (0.2/levels),data[i,]$y))
                                 grobList = c(grobList, list(new_line))
                               }
                               grid::gTree(children = do.call(gList, grobList))
                               
                             },
                             
                             draw_key = ggplot2::draw_key_text
)