## ---- Setup ----
library(leaflet)
library(ggmap)
ipapubs <- read.csv("ipa_details.csv", stringsAsFactors=FALSE)

locations <- t(sapply(ipapubs$Address, geocode))


ipapubs$lat <- unlist(locations[,"lat"])
ipapubs$lon <- unlist(locations[,"lon"])

createPopupText <- function(language, pub)
{
    ## create a clickable language link, if there is a publication link
    ## Otherwise have a publication title following the language name

    links <- grep("^http", pub)

    popup <- paste(language, "</br>", pub)
    popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], "</a>", sep="")
    return(popup)
}
ipapubs$popup <- createPopupText(ipapubs$Language, ipapubs$Publication)

## ---- CreateMap ----
m <- leaflet()
#m <- addTiles(m, group = "OSM (default)") # uses open street map
#    m <- addProviderTiles(m,"Stamen.Toner", group = "Toner")
#    m <- addProviderTiles(m, "Stamen.TonerLite", group = "Toner Lite")
m <- addProviderTiles(m, "OpenTopoMap", group="MapQuestOpen.Aerial")

m <- addMarkers(m, lng=ipapubs$lon, lat=ipapubs$lat, popup=ipapubs$popup)

## ---- DisplayMap ----
m

