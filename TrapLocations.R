####---
# Title: TrapLocations.R
#
# Author: Mark Myer
#
# Date: 1/8/2020
#
# Purpose: To create maps of our mosquito surveillance locations
#
# R version 3.6.1 Action of the Toes
####---

library(readr)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(rgdal)
library(ggmap)
library(RColorBrewer)
library(viridisLite)

#Import most recent dataset for each trap type
grav <- read_excel("2019/Gravid/121019Mosquito ID sheet_for analysis_Gravid_2019_JWB.xls.xlsx", sheet =1) 
light <- read_excel("2019/CDC Light/121119Mosquito ID sheet_for analysis_CDC_Light.xlsx", sheet = 2)
bg <- read_excel("2019/BG Sentinel/2019_Mosquito ID sheet_for analysis_BG_Sentinel.xls", sheet=1, skip =1)

#Get the unique trap locations for each 
grav <- grav %>% select(FID, Address, long, lat) %>% mutate(type = "Gravid") %>% unique() %>% drop_na()
light <- light %>% select(FID, Address, long, lat) %>% mutate(type = "Light") %>% unique() %>% drop_na()
bg <- bg %>% select(FID, Address, long, lat) %>% mutate(type = "BG Sentinel") %>% unique() %>% drop_na()

#Merge into one dataset
traps <- rbind(grav, light, bg)

#Visualize using ggmaps
#Get the New Orleans bounding box
height <- max(grav$lat) - min(grav$lat)
width <- max(grav$long) - min(grav$long)
borders <- c(bottom  = min(grav$lat)  - 0.2 * height, 
             top     = max(grav$lat)  + 0.2 * height,
             left    = min(grav$long) - 0.2 * width,
             right   = max(grav$long) + 0.2 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 11, maptype = "terrain")
map <- ggmap(nola_stamen)

#Plot locations of traps colored by trap type
pdf(file = "./2019/TrapLocationsLabels.pdf", height = 8, width = 9)
map + geom_point(data = traps, pch=21, fill="lightblue", stroke = 1, aes(x=long, y= lat))  + 
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("New Orleans 2019 Mosquito Trap Locations") +
  geom_text(data = traps, aes(x=long, y=lat, label = Address), nudge_x = 0.001, nudge_y = 0.005, size = 2) +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=18, face = "bold"))
dev.off()