####---
# Title: MosqWeeklyVisualization.R
#
# Author: Mark Myer
#
# Date: 5/21/2020
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
library(emoGG)


#Import data
#Gravid traps
grav <- read_excel("2020/Gravid/2020Gravid and pools.xlsx", sheet = 1) 

grav <- grav %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_at(c(names(grav)[9:(ncol(grav)-1)]), as.integer) #Errors are OK, it's just warning that the "." were turned into NAs

grav <- mutate(grav, `Culex sp._total` = rowSums(grav[grep("Culex", names(grav))], na.rm = TRUE), `Aedes sp._total` = rowSums(grav[grep("Aedes", names(grav))], na.rm = TRUE))

#Subset to only the most recent week
grav_subset <- grav %>% filter(week(Date) == max(week(Date)))

#Light traps 
light <- rbind(read_excel("2020/CDC Light/2020CDCLTdata.xlsx", sheet = 1), read_excel("2020/CDC Light/2020CDCLTdata.xlsx", sheet = 2))

light <- light %>%
  mutate(Date = as.Date(Date)) %>%
  mutate_at(c(names(light)[7:(ncol(light)-1)]), as.integer) 

light <- mutate(light, `Culex sp._total` = rowSums(light[grep("Culex", names(light))], na.rm = TRUE), `Aedes sp._total` = rowSums(light[grep("Aedes", names(light))], na.rm = TRUE), `Anopheles sp._total` = rowSums(light[grep("Anopheles", names(light))], na.rm = TRUE))

#Subset to only the most recent week 
light_subset <- light %>% filter(week(Date) == (max(week(Date)))) 

#BG Sentinel traps
bg <- read_excel("2020/BG Sentinel/Mosquito ID sheet_for analysis_BG_Sentinel_2020.xls")

bg <- bg %>%
      mutate(Date = as.Date(Date)) %>%
      mutate_at(c(names(bg)[9:(ncol(bg)-1)]), as.integer) #%>%
      #mutate(lat_temp = lat, lat = long, long = lat_temp) %>%  #Lat and Long were reversed so need to fix them here
      #select(-lat_temp)

bg <- bg %>% mutate(`Culex sp._total` = rowSums(.[grep("Culex", names(.))], na.rm = TRUE), `Aedes sp._total` = rowSums(.[grep("Aedes", names(.))], na.rm = TRUE))

#Subset to only the most recent week 
bg_subset <- bg %>% filter(week(Date) == (max(week(Date)))) 

#Summarize by trap
#Custom function for the most frequently appearing string
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

summ_grav <- grav_subset %>%
  group_by(FID) %>% 
  summarize(address = calculate_mode(Address),
            long = median(Long),
            lat = median(Lat),
            cul_total = sum(`Culex sp._total`, na.rm=T), #In case there are more than one observation per week, this calculate the total
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T))

summ_light <- light_subset %>%
  group_by(Address) %>% 
  summarize(FID = mean(FID, na.rm = T),
            long = median(long),
            lat = median(lat),
            cul_total = sum(`Culex sp._total`, na.rm=T),
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T),
            ano_total = sum(`Anopheles sp._total`, na.rm=T), 
            ano_mean = mean(`Anopheles sp._total`, na.rm=T))

summ_bg <- bg_subset %>%
  group_by(FID) %>%
  summarize(address = calculate_mode(Address),
            long = median(Long),
            lat = median(Lat),
            cul_total = sum(`Culex sp._total`, na.rm=T),
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T)) %>%
  na.omit() #Get rid of superfluous NA row

#See whether totals and means are related
#Culex from gravid traps
plot(cul_total ~ cul_mean, data = summ_grav)
# #Aedes from BG traps
# plot(aed_total ~ aed_mean, data = summ_bg)

#Visualize using ggmaps
#Get the New Orleans bounding box
height <- max(summ_grav$lat) - min(summ_grav$lat)
width <- max(summ_grav$long) - min(summ_grav$long)
borders <- c(bottom  = min(summ_grav$lat)  - 0.3 * height, 
                 top     = max(summ_grav$lat)  + 0.3 * height,
                 left    = min(summ_grav$long) - 0.3 * width,
                 right   = max(summ_grav$long) + 0.3 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 11, maptype = "terrain")
map <- ggmap(nola_stamen)

#Gravid traps with Aedes
tiff(filename = "./2020/Gravid/Maps/Aedes/052820_Gravid_Aedes_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = summ_grav, pch=21, stroke = 1, aes(x=long, y= lat, fill = aed_total), size = 3)  + 
  scale_fill_gradient(name= expression(italic("Aedes")~"collected"), low = "green", high = "red", limits = c(0, max(grav$`Aedes sp._total`, na.rm=T))) +
  #scale_size_continuous(limits = c(0, max(summ_grav$aed_mean, na.rm=T))) +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("05/28/2020 Gravid Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 Gravid Traps") 
dev.off()

#Gravid traps with Culex
tiff(filename = "./2020/Gravid/Maps/Culex/052820_Gravid_Culex_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type="cairo")
map + geom_point(data = summ_grav, pch=21, stroke = 1, aes(x=long, y= lat, fill = cul_total), size = 3)  + 
                 scale_fill_gradient(name= expression(italic("Culex")~"collected"), low = "green", high = "red", limits = c(0, max(grav$`Culex sp._total`, na.rm=T))) +
                 #scale_size_continuous(limits = c(0, max(summ_grav$cul_mean, na.rm=T))) +
                 guides(size = F) +
                 labs(x = "Longitude", y = "Latitude") + 
                 ggtitle("05/28/2020 Gravid Traps") +
                 theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
                       axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
                       plot.title = element_text(size=20, face = "bold")) 
dev.off()

# #For fun, try an emoji plot
# #Search for emojis
# emoji_search("smile")
# emoji_search("symbols_over_mouth")
# 
# tiff(filename = "./2019/Gravid/emoji_110619_Gravid_Culex.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
# map + 
#   geom_emoji(aes(x = long, y = lat), data= filter(summ_grav, cul_total < 15),  emoji="1f603", size = 0.0275) + 
#   geom_emoji(aes(x = long, y = lat), data= filter(summ_grav, cul_total >= 15),  emoji="1f92c", size = 0.0275) +
#   ggtitle("11/06/2019 Gravid Traps") 
# dev.off()

#Light traps with Aedes
tiff(filename = "./2020/CDC Light/Maps/052820_Light_Aedes_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = summ_light, pch=21, stroke = 1, aes(x=long, y= lat, fill = aed_total), size = 3)  + 
                  scale_fill_gradient(name= expression(italic("Aedes")~"collected"), low = "green", high = "red", limits = c(0, max(light$`Aedes sp._total`, na.rm=T))) +
                  #scale_size_continuous(limits = c(0, max(summ_light$aed_total, na.rm=T))) +
                  guides(size = F) +
                  labs(x = "Longitude", y = "Latitude") + 
                  ggtitle("05/28/2020 Light Traps") +
                  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
                        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
                        plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 Gravid Traps") 
dev.off()

#Light traps with Culex
tiff(filename = "./2020/CDC Light/Maps/052820_Light_Culex_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = summ_light, pch=21, stroke = 1, aes(x=long, y= lat, fill = cul_total), size = 3)  + 
  scale_fill_gradient(name= expression(italic("Culex")~"collected"), low = "green", high = "red", limits = c(0, max(light$`Culex sp._total`, na.rm=T))) +
  #scale_size_continuous(limits = c(0, max(summ_light$cul_total, na.rm=T))) +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("05/28/2020 Light Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 Gravid Traps") 
dev.off()

#Light traps with Anopheles
tiff(filename = "./2020/CDC Light/Maps/052820_Light_Anopheles_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type = "cairo")
map + geom_point(data = summ_light, pch=21, stroke = 1, aes(x=long, y= lat, fill = ano_total), size = 3)  + 
  scale_fill_gradient(name= expression(italic("Anopheles")~"collected"), low = "green", high = "red", limits = c(0, max(light$`Anopheles sp._total`, na.rm=T))) +
  #scale_size_continuous(limits = c(0, max(summ_light$ano_total, na.rm=T))) +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("05/28/2020 Light Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
#+ geom_text(x=(borders[3] + (newwidth * 0.5)), y=(borders[2] - (newheight * 0.05)), label = "2019 Gravid Traps") 
dev.off()

#BG traps with Aedes
tiff(filename = "./2020/BG Sentinel/Maps/Aedes/052820_BG_Aedes_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type="cairo")
map + geom_point(data = summ_bg, pch=21, stroke = 1, aes(x=long, y= lat, fill = aed_total), size = 3)  + 
  scale_fill_gradient(name= expression(italic("Aedes")~"collected"), low = "green", high = "red", limits = c(0, max(bg$`Aedes sp._total`, na.rm=T))) +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("05/28/2020 BG Sentinel Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold")) 
dev.off()

#BG traps with Culex
tiff(filename = "./2020/BG Sentinel/Maps/Culex/052820_BG_Culex_R.tiff", height = 8, width = 9, units = "in", res = 120, compression = "lzw", type="cairo")
map + geom_point(data = summ_bg, pch=21, stroke = 1, aes(x=long, y= lat, fill = cul_total), size = 3)  + 
  scale_fill_gradient(name= expression(italic("Culex")~"collected"), low = "green", high = "red", limits = c(0, max(bg$`Culex sp._total`, na.rm=T))) +
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("05/28/2020 BG Sentinel Traps") +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold")) 
dev.off()


# #Export the summary data as a shapefile 
# #Gravid traps
# coordinates(summ_grav) = ~long + lat
# proj4string(summ_grav) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
# writeOGR(summ_grav , dsn = ".", layer = "2019_GravidSummary", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# #BG Traps
# coordinates(summ_bg) = ~long + lat 
# proj4string(summ_bg) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
# writeOGR(summ_bg , dsn = ".", layer = "2019_BGSummary", driver = "ESRI Shapefile", overwrite_layer = TRUE)
