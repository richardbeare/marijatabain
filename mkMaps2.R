## ---- Setup ----

source("maputils.R")
mpubs <- read.csv("Marijas_languages.csv", stringsAsFactors=FALSE)
if (file.exists("mpubs.Rda")) {
  load("mpubs.Rda")
} else {
  mpubs <- geocodeL(mpubs)
  mpubs$popup <- createPopupText(mpubs$Language, mpubs$Publication)
  save(mpubs, file="mpubs.Rda")
}
if (any(is.na(mpubs$lat))) {
  message("Need to update list - some failed")
  mpubs <- geocodeFailed(mpubs)
  mpubs$popup <- createPopupText3(mpubs$Language, mpubs$Publication,
                                  mpubs$Address)
  save(mpubs, file="mpubs.Rda")
  
}

## ---- DisplayMap1 ----

m0 <- theMap(subset(mpubs, Pubtype=="illustration"), type="c")
m0

## ---- DisplayMap2 ----
m <- theMap(mpubs, type="c")
m