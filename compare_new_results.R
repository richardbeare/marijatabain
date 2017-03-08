## tools for checking whether google maps has changed somehow.
checkDiffs <- function(newdf,olddf)
{
  differing <- (newdf$lat != olddf$lat) | (newdf$lon != olddf$lon)
  return(list(n=newdf[differing, ], o=olddf[differing, ]))
}