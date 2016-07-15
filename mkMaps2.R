## ---- Setup ----

source("maputils.R")
mpubs <- read.csv("Marijas_languages.csv", stringsAsFactors=FALSE)
mpubs <- geocodeL(mpubs)
mpubs$popup <- createPopupText(mpubs$Language, mpubs$Publication)

## ---- DisplayMap1 ----

m0 <- theMap(subset(mpubs, Pubtype=="illustration"), type="c")
m0

## ---- DisplayMap2 ----
m <- theMap(mpubs, type="c")
m