library(readr)
library(ggplot2)
library(grid)
library(data.table)
df = read_tsv('results')
df = as.data.table(df)


GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c("x"),
                        non_missing_aes = c("size", "shape", "colour","y"),
                        default_aes = aes(
                          shape = 19, colour = "black", size = 5, fill = NA,
                          alpha = 0.2, stroke = 0.5, y = 0.2
                        ),
                        
                        draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                          coords <- coord$transform(data, panel_params)
                          points = pointsGrob(
                            coords$x, coords$y,
                            pch = coords$shape,
                            gp = gpar(
                              col = alpha(coords$colour, coords$alpha),
                              fill = alpha(coords$fill, coords$alpha),
                              # Stroke is added around the outside of the point
                              fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                              lwd = coords$stroke * .stroke / 2
                            )
                          )
                          lines = lapply(unique(coords$y), FUN = function(x) linesGrob(y = c(x,x)))
                          grobList = c(list(points), lines)
                          gTree(children = do.call(gList, grobList))
                        },
                        draw_key = draw_key_point
)



geom_timeline <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
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



GeomText$draw_panel


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
      
      position <- position_nudge(nudge_x, nudge_y)
    }
    layer(
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


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <- ggproto("GeomText", Geom,
                    required_aes = c("x", "y", "label","magnitude"),
                    
                    default_aes = aes(
                      colour = "black", fontsize = 2, angle = 45, alpha = NA, family = "",
                      fontface = 1, lineheight = 1.2
                    ),
                    
                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE, nmax = 3) {
                      lab <- data$label
                      if (parse) {
                        lab <- parse(text = as.character(lab))
                      }
                      print(str(data))
                      print(table(data$label))
                      data <- coord$transform(data, panel_params)
                      print(table(data$y))
                      print(nrow(data))
                      levels <- length(unique(data$y))
                      print(levels)
                      data <- data[order(data$magnitude, decreasing = TRUE),]
                      data <- data[1:min(nrow(data), nmax),]
                      texts = textGrob(
                        data$label,
                        data$x, data$y, default.units = "native",
                        just = "left",
                        rot = data$angle,
                        gp = gpar(
                          col = alpha(data$colour, data$alpha),
                          fontsize = data$fontsize * .pt,
                          fontfamily = data$family,
                          fontface = data$fontface,
                          lineheight = data$lineheight
                        ),
                        check.overlap = check_overlap
                      )
                      grobList = list(texts)
                      for(i in 1:nrow(data)){
                        new_line = linesGrob(x = c(data[i,]$x,data[i,]$x), y = c(data[i,]$y - (0.2/levels),data[i,]$y))
                        grobList = c(grobList, list(new_line))
                      }
                      gTree(children = do.call(gList, grobList))
                      
                    },
                    
                    draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]
  
  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}


test1 <- ggplot(data = df, aes(x = YEAR)) + geom_timeline()
test2 <- ggplot(data = df[COUNTRY %in% c("CHINA","USA") & YEAR >= 2000], aes(x = YEAR, y = INTENSITY)) + geom_timeline(alpha = 0.2) + geom_timeline_label(aes(magnitude = INTENSITY, label = LOCATION_NAME), nudge_y = 0.2,)
test3 <- ggplot(data = df[COUNTRY %in% c("SUDAN", "UKRAINE", "ITALY")], aes(x = YEAR, y = COUNTRY, label = COUNTRY)) + geom_timeline(alpha = 0.2) + geom_timeline_label(nudge_y = 0.2, aes(magnitude = INTENSITY))


