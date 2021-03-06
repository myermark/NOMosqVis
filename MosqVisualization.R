####---
# Title: MosqVisualization.R
#
# Author: Mark Myer
#
# Date: 11/6/2019
#
# Purpose: To format and summarize data from mosquito surveillance for plotting
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

#Import data
grav_19 <- read_excel("Q:/NOLA Projects/Mosquito Surveillance/Gravid_2019_JWB.xlsx") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate_at(c(names(grav_19)[8:(ncol(grav_19)-1)]), as.integer) #Errors are OK, it's just warning that the "." were turned into NAs

grav_19 <- mutate(grav_19, `Culex sp._total` = `Culex sp._m` + `Culex sp._f`, `Aedes sp._total` = `Aedes sp._m` + `Aedes sp._f`) 

bg_19 <- read_excel("BG_2019.xls") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate_at(c(names(bg_19)[7:(ncol(bg_19)-1)]), as.integer) %>% 
  mutate(lat_temp = lat, lat = long, long = lat_temp) %>%  #Lat and Long were reversed so need to fix them here
  select(-lat_temp)

bg_19 <- bg_19 %>% mutate(`Culex sp._total` = rowSums(.[grep("Culex", names(.))], na.rm = TRUE), `Aedes sp._total` = rowSums(.[grep("Aedes", names(.))], na.rm = TRUE))

#Summarize by trap 
summ_grav_19 <- grav_19 %>%
  group_by(FID) %>% 
  summarize(address = median(Address),
            long = median(long),
            lat = median(lat),
            cul_total = sum(`Culex sp._total`, na.rm=T),
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T))

summ_bg_19 <- bg_19 %>%
  group_by(FID) %>% 
  summarize(address = median(Address),
            long = median(long),
            lat = median(lat),
            cul_total = sum(`Culex sp._total`, na.rm=T),
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T)) %>%
  na.omit() #Get rid of superfluous NA row

#See whether totals and means are related
#Culex from gravid traps
plot(cul_total ~ cul_mean, data = summ_grav_19)
#Aedes from BG traps
plot(aed_total ~ aed_mean, data = summ_bg_19)

#Visualize using ggmaps
#Get the New Orleans bounding box
height <- max(summ_bg_19$lat) - min(summ_bg_19$lat)
width <- max(summ_bg_19$long) - min(summ_bg_19$long)
borders <- c(bottom  = min(summ_bg_19$lat)  - 0.2 * height, 
                 top     = max(summ_bg_19$lat)  + 0.2 * height,
                 left    = min(summ_bg_19$long) - 0.2 * width,
                 right   = max(summ_bg_19$long) + 0.2 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 11, maptype = "terrain")
map <- ggmap(nola_stamen)

#Gravid traps with Culex
tiff(filename = "2019_Gravid_Culex_R.tiff", height = 8, width = 9, units = "in", res = 300, compression = "lzw")
map + geom_point(data = summ_grav_19, pch=21, stroke = 1,
                 aes(x=long, y= lat, fill = cul_total, size = cul_total))  + 
                 scale_fill_gradient(name= expression(italic(Culex)~"collected"), low = "green", high = "red") +
                 guides(size = F) +
                 labs(x = "Longitude", y = "Latitude") + 
                 ggtitle("2019 Gravid Traps") +
                 theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
                       axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
                       plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 Gravid Traps") 
dev.off()

#BG traps with Aedes
tiff(filename = "2019_BG_Aedes_R.tiff", height = 8, width = 9, units = "in", res = 300, compression = "lzw")
map + geom_point(data = summ_bg_19, pch=21, stroke = 1,
                 aes(x=long, y= lat, fill = aed_total, size = aed_total))  + 
  scale_fill_gradient(name= expression(italic(Aedes)~"collected"), low = "green", high = "red") +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("2019 BG Sentinel Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 BG Sentinel Traps") 
dev.off()

#Export the summary data as a shapefile 
#Gravid traps
coordinates(summ_grav_19) = ~long + lat
proj4string(summ_grav_19) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
writeOGR(summ_grav_19 , dsn = ".", layer = "2019_GravidSummary", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#BG Traps
coordinates(summ_bg_19) = ~long + lat 
proj4string(summ_bg_19) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
writeOGR(summ_bg_19 , dsn = ".", layer = "2019_BGSummary", driver = "ESRI Shapefile", overwrite_layer = TRUE)
