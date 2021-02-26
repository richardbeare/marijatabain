## ---- Setup ----
## need my modified ggmap
library(tidyverse)
source("maputils.R")
ipapubs <- read.csv("ipa_details.csv", stringsAsFactors=FALSE)
#ipaill.raw <- read.csv("IPA_Illustrations.csv", stringsAsFactors=FALSE, encoding="latin1")
# use excel format - a bit easier to deal with different character sets
ipaill.raw <- readxl::read_excel("IPA_Illustrations_enc.xlsx")

ipaill <- gather(ipaill.raw, key=AddCol, value="Address", starts_with("Address"))
ipaill <- subset(ipaill, Address != "")
lcount <- summarise(group_by(ipaill, Language), pins=n())

ipaill <- merge(ipaill, lcount, by.x="Language", by.y="Language")
ipaill <- mutate(ipaill, pinstr=ifelse(pins==1, "", "*"))
if (file.exists("ipastuff.Rda")) {
  j <- load("ipastuff.Rda")
} else {
  ipapubs <- geocodeL(ipapubs)
  ipapubs$popup <- createPopupText(ipapubs$Language, ipapubs$Publication)
  ipaillnew <- geocodeL(ipaill)
  ipaillnew$popup <- createPopupText2(ipaill$Language, ipaill$Publication, ipaill$Recording, 
                                      ipaill$pinstr, ipaill$Address)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}
#
# When some of the files were changed to have upper case
dummy <- function() {
  tofix <- which(tolower(basename(ipaillnew$Recording)) == basename(ipaillnew$Recording))
  N <- basename(ipaillnew$Recording[tofix])
  N <- gsub("^(.)", "\\U\\1", x=N, perl=TRUE)
  N <- gsub("(-.)", "\\U\\1", x=N, perl=TRUE)
  
  ipaillnew$Recording[tofix] <- file.path(dirname(ipaillnew$Recording[tofix]), N)
  ipaillnew$popup <- createPopupText2(ipaillnew$Language, ipaillnew$Publication, ipaillnew$Recording, 
                                      ipaillnew$pinstr, ipaillnew$Address)
  
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}

if (any(is.na(ipaillnew$lat))) {
  message("Need to update list - some failed")
  ipaillnew <- geocodeFailed(ipaillnew)
  ipaillnew$popup <- createPopupText2(ipaill$Language, ipaill$Publication, ipaill$Recording, 
                                      ipaill$pinstr, ipaill$Address)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
  
}
## Check to see whether there are extra updates necessary
newlangs <- setdiff(ipaill$Language, ipaillnew$Language)
if (length(newlangs) > 0) {
  to.update <- subset(ipaill, Language %in% newlangs)
  langs.updated <- geocodeL(to.update)
  langs.updated$popup <- createPopupText2(to.update$Language, to.update$Publication, to.update$Recording, 
                                          to.update$pinstr, to.update$Address)
  ipaillnew <- rbind(ipaillnew, langs.updated)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}
## Small corrections go here
#ii <- updateOne(ipaillnew, ipaill, language="Jamaican Creole")
#ii <- updateOne(ipaillnew, ipaill, language="Galician")
#ii <- updateOne(ipaillnew, ipaill, language="Telugu")
## ii <- updateOne(ipaillnew, ipaill, language="Setswana (South African)")
#ipaillnew <- ii
#  ipaillnew$popup <- createPopupText2(ipaillnew$Language, ipaillnew$Publication, ipaillnew$Recording, ipaillnew$pinstr, ipaillnew$Address)
#save(ipapubs, ipaillnew, file="ipastuff.Rda")

# Some fixes, 22/10/2019
# Extra zip files. Spotted some duplications that weren't necessary and hadn't been fixed
dummy <- function() {
  # remove some duplicates to match the spreadsheet, so we don't need to recode everything
  ii <- ipaill[-c(57,133),]
  # reorder ii to match ipaillnew
  ii <- arrange(ii, Language, Address)
  ipaillnew <- arrange(ipaillnew, Language, Address)
  ipaillnew$popup <- createPopupText2(ii$Language, ii$Publication, ii$Recording, 
                                      ii$pinstr, ii$Address)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")

  #############################
  ## Missing recordings 26/02/2021
 
  ipaillnew[which(ipaillnew$Language == "Tongan"), "Recording"] <- "https://www.internationalphoneticassociation.org/sites/default/files/JIPArecordings/Tongan.zip"
  ipaillnew[which(ipaillnew$Language == "Central Lisu"), "Recording"] <- "https://www.internationalphoneticassociation.org/sites/default/files/JIPArecordings/Lisu.zip"
  ipaillnew$popup <- createPopupText2(ipaillnew$Language, ipaillnew$Publication, ipaillnew$Recording, 
                                      ipaillnew$pinstr, ipaillnew$Address)
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
ipaillnew <- locationJitter(ipaillnew)
linkstuff <- createJS(ipaillnew, zoom=7)

## ---- CreateMap ----
m1 <- theMap(ipapubs, type="c")
m2 <- theMap(ipaillnew, type="c")
m2 <- htmlwidgets::onRender(m2, linkstuff$js)
## ---- DisplayMapEveryone ----
m2

## ---- DisplayLinks ----
cat(links=linkstuff$links)


