## ---- Setup ----
source("maputils.R")
ipapubs <- read.csv("ipa_details.csv", stringsAsFactors=FALSE)

ipapubs <- geocodeL(ipapubs)
ipapubs$popup <- createPopupText(ipapubs$Language, ipapubs$Publication)

## ---- CreateMap ----
m <- theMap(ipapubs, type="c")
## ---- DisplayMap ----
m

