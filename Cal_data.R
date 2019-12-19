setwd("C://Users//taiebat//Box//Public-Shared//Research-MT-SS//CAV Rebound Effect//Code & Data//NHTS2017")
setwd("/Users/taiebat/Box/Public-Shared/Research-MT-SS/CAV Rebound Effect/Code & Data/NHTS2017")

library(tidyverse)
TData <- read_csv("trippub.csv", col_names =T)

table(TData$HHSTATE)
table(TData$VEHID)
table(TData$TRPTRANS)


## VMT_MILE & TRPMILES
TData <- subset(TData, HHSTATE == "CA")


TData$TRPMILES[TData$TRPMILES < 0 ] <- NA
TData$TRPMILES[is.na(TData$TRPMILES) ] <- 0.00001

TData <- TData %>% 
  group_by(HOUSEID) %>% 
  mutate(active = sum(TRPMILES[TRPTRANS %in% c("01", "02")]),
         car = sum(TRPMILES[TRPTRANS %in% c("03", "04", "05", "06")]),
         car1 = sum(TRPMILES[TRPTRANS %in% c("03", "04", "05", "06")
                             && VEHID %in% c("01") ]),
         car2 = sum(TRPMILES[TRPTRANS %in% c("03", "04", "05", "06")
                             && VEHID %in% c("02") ]),
         car3plus = sum(TRPMILES[TRPTRANS %in% c("03", "04", "05", "06")
                             && VEHID %in% c("03", "04", "05", "06", "08", "09", "10", "11", "12") ]),
         pubtransp = sum(TRPMILES[TRPTRANS %in% c("10", "11", "12", "15", "16")]),
         ridehail = sum(TRPMILES[TRPTRANS %in% c("17", "18")]))

summary(TData$active)
summary(TData$car)
summary(TData$car1)
summary(TData$car2)
summary(TData$car3plus)
summary(TData$pubtransp)
summary(TData$ridehail)

# a <- TData %>% 
#   filter(active > 0 )
# length(unique(TData$HOUSEID))


####### find unique hosueholds

CAHHData <- TData[!duplicated(TData$HOUSEID), ]

write_csv(CAHHData, path = "/Users/taiebat/Box/Apollo Package/NHTS/CAHHData.csv")



