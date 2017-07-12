library(dplyr)
library(ggmap)
source("read_data.R")
##Leaflet Maps
library(leaflet)
eq_map <- function(df,annot_col = "DATE"){
      map = leaflet() %>% addTiles() %>% addCircleMarkers(data = df, lng = ~ LONGITUDE,
                                                          lat = ~ LATITUDE,
                                                          radius = ~ EQ_PRIMARY,
                                                          popup = ~ eval(parse(text = annot_col)))
      return(map)
}

eq_create_label <- function(data){
      data = eq_location_clean(data)
      label = rep("", times = nrow(data))
      label[!is.na(data$LOCATION_NAME)] = paste0(label[!is.na(data$LOCATION_NAME)], "<b>Location:</b>",data$LOCATION_NAME[!is.na(data$LOCATION_NAME)], "<br>")
      label[!is.na(data$EQ_PRIMARY)] = paste0(label[!is.na(data$EQ_PRIMARY)], "<b>Magnitude:</b>",data$EQ_PRIMARY[!is.na(data$EQ_PRIMARY)], "<br>")
      label[!is.na(data$DEATHS)] = paste0(label[!is.na(data$DEATHS)], "<b>Total Deaths:</b>",data$DEATHS[!is.na(data$DEATHS)], "<br>")
      return(label)
}

test = readr::read_delim("results.tsv", delim = "\t") %>% 
      eq_clean_data()

readr::read_delim("results.tsv", delim = "\t") %>% 
      eq_clean_data() %>% 
      dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
      eq_map(annot_col = "DATE")

readr::read_delim("results.tsv", delim = "\t") %>% 
      eq_clean_data() %>% 
      dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
      dplyr::mutate(popup_text = eq_create_label(.)) %>% 
      eq_map(annot_col = "popup_text")
