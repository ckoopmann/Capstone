library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

eq_clean_data <- function(df){
      df$DATE = as_date("01-01-1970",format = "%d-%m-%Y")
      year(df$DATE) = df$YEAR
      month(df$DATE) = df$MONTH
      day(df$DATE) = df$DAY
      df$LATITUDE = as.numeric(df$LATITUDE)
      df$LONGITUDE = as.numeric(df$LONGITUDE)
      return(df)
}

eq_location_clean <- function(df){
      df$LOCATION_NAME <- str_split_fixed(df$LOCATION_NAME, ": ", n = 2)[,2]
      return(df)
}
# df = read_tsv("results.tsv")
# df = eq_clean_data(df)
# gsub(pattern = "^\\s",replacement = "", df$LOCATION_NAME)
# df = eq_location_clean(df)
# 
# ##Visualisation
# library(grid)
# GeomTimeline <- ggproto("GeomMyPoint", Geom,
#                        required_aes = c("x"),
#                        default_aes = aes(size = 1),
#                        draw_key = draw_key_point,
#                        draw_panel = function(data, panel_scales, coord) {
#                              ## Transform the data first
#                              coords <- coord$transform(data, panel_scales)
#                              
#                              ## Let's print out the structure of the 'coords' object
#                              print(str(coords))
#                              ## Construct a grid grob
#                              pointsGrob(
#                                    x = unit(as.numeric(coords$x),"npc"),
#                                    y = unit(rep_len(1,length.out = length(coords$x)),"inches"),
#                                    size = unit(coords$size,"npc")
#                              )
#                        })
# 
# geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
#                          position = "identity", na.rm = FALSE, 
#                          show.legend = NA, inherit.aes = TRUE, ...) {
#       ggplot2::layer(
#             geom = GeomTimeline, mapping = mapping,  
#             data = data, stat = stat, position = position, 
#             show.legend = show.legend, inherit.aes = inherit.aes,
#             params = list(na.rm = na.rm, ...)
#       )
# }
# 
# test <- ggplot(data = df, aes(x = YEAR)) + geom_timeline()
# test <- ggplot(data = df, aes(x = YEAR, size = DEATHS)) + geom_timeline()
