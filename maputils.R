library(leaflet)
library(ggmap)
library(tidyr)
library(dplyr)
geocodeL <- function(ipapubs)
{
  a <- stringi::stri_trans_general(ipapubs$Address, "latin-ascii")
  locations <- t(sapply(a, geocode))
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
createPopupText2 <- function(language, pub, recording)
{
  ## create a clickable language link, if there is a publication link
  ## Otherwise have a publication title following the language name
  ## substitute DOI for the appropriate link
  
  if (length(pub) != length(recording)) {stop("recording and pub links dont match\n")}
  
  pub <- gsub("^DOI:", "doi:", pub)
  pub <- gsub("^doi:", "http://dx.doi.org/", pub)
  links <- grep("^http", pub)
  popup <- paste(language, "</br>", pub)
  recordings <- (!is.na(recording)) | (nchar(recording)>0)
  popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], "</a>", sep="")
  popup[recordings] <- paste(popup[recordings], "</br><a href=\"", recording[recordings], "\"> Recording</a>", sep="")
  return(popup)
}

## icons
mI <- function(iconfile) {
    makeIcon(
      iconUrl = iconfile,
      shadowUrl = "./icons/marker-shadow.png",
      iconAnchorX=13,
      iconAnchorY=40,
      shadowAnchorX=13,
      shadowAnchorY=40,
      popupAnchorX =1,
      popupAnchorY = -40
    )
}

these.icons <- list(green=mI("icons/green_pin.png"),
                    purple= mI("icons/purple_pin.png"),
                    yellow= mI("icons/yellow_pin.png"),
                    cyan=mI("icons/cyan_pin.png"))


theMap <- function(ipapubs, type="a", icons=these.icons)
{
  ipapubs <- ipapubs[!is.na(ipapubs$lon),]
  m <- leaflet()
  ipapubs <- within(ipapubs, {
      Pubtype[is.na(Pubtype)] <- "standard"
  }
  )
  m <- switch(type, a=addTiles(m, group = "OSM (default)"),
              b =addProviderTiles(m, "OpenTopoMap", group="MapQuestOpen.Aerial"),
              c = addProviderTiles(m, "Esri.WorldTopoMap", group="Esri.WorldTopoMap"),
              d = addProviderTiles(m, "Esri.WorldImagery", group="Esri.WorldImagery"),
              e = addProviderTiles(m, "Esri.WorldPhysical", group="Esri.WorldPhysical")
  )
  ii <- nest(group_by(ipapubs, Pubtype))

  for (k in 1:length(ii$data)) {
      iid <- ii$data[[k]]
      m <- addMarkers(m, lng=iid$lon, lat=iid$lat, popup=iid$popup, icon=these.icons[[k]])
  }
  return(m)
}

