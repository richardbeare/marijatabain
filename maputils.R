library(leaflet)
library(ggmap)
library(tidyr)
library(dplyr)

if (file.exists("google_keys.R")) {
  source("google_keys.R")
}
geocodesleep <- function(location) {
  Sys.sleep(0.25)
  return(geocode(location))
}

geocodeL <- function(ipapubs)
{
  a <- stringi::stri_trans_general(ipapubs$Address, "latin-ascii")
  locations <- t(sapply(a, geocodesleep))
  ipapubs$lat <- unlist(locations[,"lat"])
  ipapubs$lon <- unlist(locations[,"lon"])
  return(ipapubs)
}

geocodeFailed <- function(ipageo) {
  failed <- which(is.na(ipageo$lat))
  a <- stringi::stri_trans_general(ipageo$Address[failed], "latin-ascii")
  locations <- t(sapply(a, geocodesleep))
  ipageo$lat[failed] <- unlist(locations[,"lat"])
  ipageo$lon[failed] <- unlist(locations[,"lon"])
  return(ipageo)
}
updateOne <- function(ipapubs, ipapubs.updated , language)
{
  ii <- subset(ipapubs.updated, Language==language)
  nn <- geocodeL(ii)
  r <- which(ipapubs$Language == language)
  ipapubs[r, "lat"] <- nn[,"lat"]
  ipapubs[r, "lon"] <- nn[,"lon"]
  ipapubs[r, "Address"] <- nn[,"Address"]
  return(ipapubs)
}
createPopupText <- function(language, pub, lcount=NULL)
{
  ## create a clickable language link, if there is a publication link
  ## Otherwise have a publication title following the language name
  ## substitute DOI for the appropriate link
  pub <- gsub("^DOI:", "doi:", pub)
  pub <- gsub("^doi:", "http://dx.doi.org/", pub)
  links <- grep("^http", pub)
  popup <- paste(language, "<sup>", lcount, "</sup></br>", pub)
  popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], "</a>", sep="")
  return(popup)
}
createPopupText2 <- function(language, pub, recording, lcount=NULL, address=NULL)
{
  ## create a clickable language link, if there is a publication link
  ## Otherwise have a publication title following the language name
  ## substitute DOI for the appropriate link
  if (length(pub) != length(recording)) {stop("recording and pub links dont match\n")}
  multitag <- paste("<sup>", lcount, "</sup>")
  multitag[lcount==""] <- ""
  pub <- gsub("^DOI:", "doi:", pub)
  pub <- gsub("^doi:", "http://dx.doi.org/", pub)
  links <- grep("^http", pub)
  popup <- paste0(language, multitag, "</br>", pub, "</br>", address, "</br>")
  recordings <- (!is.na(recording)) | (nchar(recording)>0)
  recordings[is.na(recordings)] <- FALSE
  popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], multitag[links], "</a>", "</br>", address[links], "</br>", sep="")
  popup[recordings] <- paste(popup[recordings], "</br><a href=\"", recording[recordings], "\"> Recording</a>", sep="")
  return(popup)
}
createPopupText3 <- function(language, pub, lcount=NULL, address=NULL)
{
  ## create a clickable language link, if there is a publication link
  ## Otherwise have a publication title following the language name
  ## substitute DOI for the appropriate link
  pub <- gsub("^DOI:", "doi:", pub)
  pub <- gsub("^doi:", "http://dx.doi.org/", pub)
  links <- grep("^http", pub)
  popup <- paste0(language, "</br>", pub, "</br>", address, "</br>")
  popup[links] <- paste("<a href=\"", pub[links], "\">", language[links], "</a>", "</br>", address[links], "</br>", sep="")
  return(popup)
}

locationJitter <- function(df) {
  myjitter <- function(d) {
    scaler <- 0.01
    res <- tibble(x=0, y=0)
    nn <- nrow(d)
    if (nn > 1) {
      res <- tibble(x=runif(nn, min=-1, max=1)*scaler, y=runif(nn, min=-1, max=1)*scaler)
    }
    return(cbind(d, res))
  }
  kk <- nest(group_by(df, lat, lon))
  kk <- mutate(kk, data2 = map(data, myjitter))
  kk <- ungroup(unnest(kk, data2))
  
  kk <- mutate(kk, lat=lat+y, lon=lon+x, x=NULL, y=NULL)
  return(kk)
}
createJS <- function(lang.df, zoom)
{
  lang.df <- arrange(lang.df, Language)
  tt <- 1:nrow(lang.df)
  lang.df <- mutate(lang.df, jsid=paste0("#zoomto", tt))
  ## create the click handler
  oneLink <- function(df, zoom)
  {
    sv <- paste0('map.setView([', as.character(df$lat), ', ',as.character(df$lon), '], ', as.character(zoom), ');')
    a1 <- paste0('$(document).on("click", "', df$jsid)
    a2 <- paste(c('", function(e){',
                 'e.preventDefault();',
                 sv, '});'), collapse = "\n")
    a2 <- paste0(a1, a2, "\n")
    return(a2)
  }
  fbody<- map_chr(1:nrow(lang.df), ~oneLink(lang.df[.x,], zoom))
  entire.function <- paste(c("function(el, x) {",
                             "var map = this;", fbody, "}"), collapse="\n")
  ## Create link text
  lt <- paste0("[", lang.df$Language, "](#){", lang.df$jsid, "}  \n")
  return(list(links=lt, js=entire.function))
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
  ii <- arrange(ii, Pubtype)
  for (k in 1:length(ii$data)) {
      iid <- ii$data[[k]]
      m <- addMarkers(m, lng=iid$lon, lat=iid$lat, popup=iid$popup, icon=these.icons[[k]])
  }
  return(m)
}

