library(readxl)
library(lubridate)
library(dplyr)

setwd("/Volumes/Mark Drive/NOLA/Mosquito Surveillance")

dat <- read_excel("MasterRecord_Clean.xlsx", sheet = 1)

dat <- filter(dat, year(dat$Date) > 2009)


temp <- data.frame()
for(i in 2010:2019) {
  assign(paste0("temp",i), select(filter(dat, year(dat$Date) == i), Address))
  #write(unique(temp$Address), paste0(i,"_GravidTrapList.txt"))
}

View(intersect(temp2010, temp2011, temp2012, temp2013, temp2014, temp2015, temp2016, temp2017, temp2018, temp2019))


