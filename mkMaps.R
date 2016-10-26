## ---- Setup ----
source("maputils.R")
ipapubs <- read.csv("ipa_details.csv", stringsAsFactors=FALSE)
ipaill <- read.csv("IPA_Illustrations.csv", stringsAsFactors=FALSE, encoding="latin1")

if (file.exists("ipastuff.Rda")) {
  load("ipastuff.Rda")
} else {
  ipapubs <- geocodeL(ipapubs)
  ipapubs$popup <- createPopupText(ipapubs$Language, ipapubs$Publication)
  ipaillnew <- geocodeL(ipaill)
  ipaillnew$popup <- createPopupText2(ipaill$Language, ipaill$Publication, ipaill$Recording)
  save(ipapubs, ipaillnew, file="ipastuff.Rda")
}
## ---- CreateMap ----
m1 <- theMap(ipapubs, type="c")
m2 <- theMap(ipaillnew, type="c")

## ---- DisplayMapEveryone ----
m2

