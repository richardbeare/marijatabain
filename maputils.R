library(leaflet)
library(ggmap)

geocodeL <- function(ipapubs)
{
  locations <- t(sapply(ipapubs$Address, geocode))
  ipapubs$lat <- unlist(locations[,"lat"])
  ipapubs$lon <- unlist(locations[,"lon"])
  return(ipapubs)
}

createPopupText <- function(language, pub)
{
  ## create a clickable language link, if there is a publication link
  ## Otherwise have a publication title following the language name
  ## substitute DOI for the appropriate link
  
  pub <- gsub("^DOI:", "doi:", pub)
  pub <- gsub("^doi:", "http://dx.doi.org/", pub)
  links <- grep("^http", pub)
  popup <- paste(language, "</br>", pub)
  popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], "</a>", sep="")
  return(popup)
}

theMap <- function(ipapubs, type="a")
{
  m <- leaflet()
  
  m <- switch(type, a=addTiles(m, group = "OSM (default)"),
              b =addProviderTiles(m, "OpenTopoMap", group="MapQuestOpen.Aerial"),
              c = addProviderTiles(m, "Esri.WorldTopoMap", group="Esri.WorldTopoMap"),
              d = addProviderTiles(m, "Esri.WorldImagery", group="Esri.WorldImagery"),
              e = addProviderTiles(m, "Esri.WorldPhysical", group="Esri.WorldPhysical")
  )
  m <- addMarkers(m, lng=ipapubs$lon, lat=ipapubs$lat, popup=ipapubs$popup)
  return(m)
}
