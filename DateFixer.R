library(readxl)
library(lubridate)
library(dplyr)

setwd("/Volumes/Mark Drive/NOLA/Mosquito Surveillance/")
dat <- read_excel("./Master Records/CDCLT_MasterRecord_Refined.xlsx")

dat$Date <- paste0(
  substr(dat$Date, 5,10), 
  ",",
  substr(dat$Date, 24,29)
)

dat$Date <- mdy(dat$Date)

write.xlsx(dat, file ="./Master Records/CDCLT_MasterRecord_Refined.xlsx")


