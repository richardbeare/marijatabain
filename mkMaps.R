## ---- Setup ----
library(tidyverse)
source("maputils.R")
ipapubs <- read.csv("ipa_details.csv", stringsAsFactors=FALSE)
ipaill.raw <- read.csv("IPA_Illustrations.csv", stringsAsFactors=FALSE, encoding="latin1")
ipaill <- gather(ipaill.raw, key=AddCol, value="Address", starts_with("Address"))
ipaill <- subset(ipaill, Address != "")
lcount <- summarise(group_by(ipaill, Language), pins=n())

ipaill <- merge(ipaill, lcount, by.x="Language", by.y="Language")
ipaill <- mutate(ipaill, pinstr=ifelse(pins==1, "", "*"))
if (file.exists("ipastuff.Rda")) {
  load("ipastuff.Rda")
} else {
  ipapubs <- geocodeL(ipapubs)
  ipapubs$popup <- createPopupText(ipapubs$Language, ipapubs$Publication)
  ipaillnew <- geocodeL(ipaill)
  ipaillnew$popup <- createPopupText2(ipaill$Language, ipaill$Publication, ipaill$Recording, 
                                      ipaill$pinstr, ipaill$Address)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}

## check whether the csv and address structures match
newones <- setdiff(ipaill$Language, ipaillnew$Language)
if (length(newones) > 0) {
  extras.df <- subset(ipaill, Language %in% newones)
  ipaill.extras <- geocodeL(extras.df)
  ipaill.extras$popup <- createPopupText2(extras.df$Language, extras.df$Publication, extras.df$Recording,
                                          extras.df$pinstr, extras.df$Address)
  ipaillnew <- rbind(ipaillnew, ipaill.extras)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}
## ---- CreateMap ----
m1 <- theMap(ipapubs, type="c")
m2 <- theMap(ipaillnew, type="c")

## ---- DisplayMapEveryone ----
m2

