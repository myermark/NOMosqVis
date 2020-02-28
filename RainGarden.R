####---
# Title: RainGarden.R
#
# Author: Mark Myer
#
# Date: 2/10/2019
#
# Purpose: To format and summarize data from rain garden surveillance with CDC lights for 2019
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
library(reshape2)

setwd("/Volumes/Mark Drive/NOLA/Mosquito Surveillance")

#Rain garden traps 
light <- read_excel("Rain Gardens/2019_RainGarden_CDCLT.xlsx", sheet = 1)

light <- light %>%
  filter(!is.na(`Culex quinquefasciatus_f`)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate_at(c(names(light)[6:(ncol(light)-1)]), as.integer) 


#Graph the types of mosquitoes found in traps
sums <- light %>% 
          summarize_if(is.numeric, sum, na.rm= TRUE)
sums <- sums[,3:length(sums)]
sums <- arrange(sums)
View(sums)

total_mosq <- rowSums(sums)

#Make a pie chart
lbls = c("Cx. salinarius","Ae. vexans","An. crucians","Cx. restuans", "Cx. nigripalpus","Cx. quinquefasciatus","Other")
slices = c(7187, 3842, 700, 550, 497, 338, rowSums(select(sums, -c(`Culex quinquefasciatus_m`,
                                                                   `Culex quinquefasciatus_f`,
                                                                   `Aedes vexans_m`,
                                                                   `Aedes vexans_f`,
                                                                   `Anopheles crucians_m`,
                                                                   `Anopheles crucians_f`,
                                                                   `Culex nigripalpus_m`,
                                                                   `Culex nigripalpus_f`,
                                                                   `Culex restuans_m`,
                                                                   `Culex restuans_f`,
                                                                   `Culex salinarius_m`,
                                                                   `Culex salinarius_f`))))

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

tiff(filename = "./Rain Gardens/MosqSpecies.tiff", width = 5.5, height = 4, units = "in", pointsize = 10, compression = "lzw", res = 300, type = "cairo")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Mosquito Species Found in Rain Gardens") 
dev.off()


#Summarize totals and plot 
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

light <- mutate(light, `Culex sp._total` = rowSums(light[grep("Culex", names(light))], na.rm = TRUE), `Aedes sp._total` = rowSums(light[grep("Aedes", names(light))], na.rm = TRUE))

summ_light <- light %>%
  group_by(Address) %>% 
  summarize(long = median(long),
            lat = median(lat),
            cul_total = sum(`Culex sp._total`, na.rm=T),
            cul_mean = mean(`Culex sp._total`, na.rm=T),
            aed_total = sum(`Aedes sp._total`, na.rm=T),
            aed_mean = mean(`Aedes sp._total`, na.rm=T), 
            total_mosq = sum(select_if(light[,4:length(light)], is.numeric)))

#Visualize using ggmaps

#Get the New Orleans bounding box
height <- max(summ_light$lat) - min(summ_light$lat)
width <- max(summ_light$long) - min(summ_light$long)
borders <- c(bottom  = min(summ_light$lat)  - 0.3 * height, 
             top     = max(summ_light$lat)  + 0.3 * height,
             left    = min(summ_light$long) - 0.3 * width,
             right   = max(summ_light$long) + 0.3 * width)

newheight = borders[2] - borders[1]
newwidth = borders[4] - borders[3]

nola_stamen <- get_stamenmap(bbox = borders, zoom = 13, maptype = "terrain")
map <- ggmap(nola_stamen)

#Light traps with Culex

tiff(filename="Rain Gardens/Plots/RainGardenMap.tiff", width = 10, height = 8, units = "in", res = 90, compression = "lzw", type = "cairo")
map + geom_point(data = summ_light, pch=21, stroke = 1, aes(x=long, y= lat), fill = "red", size = 2)  + 
  guides(size = F) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Rain Garden Monitoring Traps") +
  geom_text(data = summ_light, aes(x=long, y=lat, label = Address), nudge_x = 0.001, nudge_y = 0.0015, size = 3) +
  theme(axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"), 
        plot.title = element_text(size=20, face = "bold"))
dev.off()
#Line graphs of month to month abundance by species
light_sub <- light %>%
  mutate(Week = week(Date),
         `Culex quinquefasciatus` = `Culex quinquefasciatus_m` + `Culex quinquefasciatus_f`, 
         `Aedes aegypti` = `Aedes aegypti_m` + `Aedes aegypti_f`, 
         `Aedes albopictus` = `Aedes albopictus_m` + `Aedes albopictus_f`, 
         `Coquilletidia perturbans` = `Coquillettidia perturbans_m` + `Coquillettidia perturbans_f`, 
         `Culiseta spp.` = rowSums(light[grep("Culiseta", names(light))], na.rm = TRUE)) %>%
  select(Address, Date, Week, `Culex quinquefasciatus`, `Aedes aegypti`, `Aedes albopictus`, `Coquilletidia perturbans`,  `Culiseta spp.`)

light_gather <- pivot_longer(data = light_sub, cols = 4:8, names_to = "Species", values_to = "N")

for(address in (unique(light_gather$Address))) {
  plot_name = paste0(address,"_2019Mosquito.tiff")
  tiff(filename=paste0("Rain Gardens/Plots/", plot_name), width = 6, height = 5, units = "in", res = 150, compression = "lzw", type = "cairo")
  plot <- ggplot(data = filter(light_gather, Address == address), aes(x=Week, y = N)) + 
    geom_line(aes(color = Species)) +
    labs(x = "Week", y = "Number of Mosquitoes") + 
    ggtitle(address) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
  print(plot)
  dev.off()
}
