library(readr)
library(ggplot2)
library(grid)
library(data.table)

create("EQVisualisation")
document("EQVisualisation")

install("EQVisualisation")

rm(list = ls())
library(EQVisualisation)

df = read_tsv('results') %>% as.data.table() %>% eq_clean_data() %>% eq_location_clean()
ggplot(data = df[COUNTRY %in% c("CHINA","USA") & YEAR >= 2000], aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)

ggplot(data = df[COUNTRY %in% c("CHINA","USA") & YEAR >= 2000], aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2) + 
     geom_timeline(alpha = 0.2) + geom_timeline_label(aes(magnitude = INTENSITY, label = LOCATION_NAME), nudge_y = 0.2)