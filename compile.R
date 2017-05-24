set.seed(0102)
options(htmlwidgets.TOJSON_ARGS = list(pretty = TRUE))
Sys.setenv(R_KNITR_OPTIONS = 'knitr.chunk.tidy = FALSE', RSTUDIO_PANDOC='/usr/lib/rstudio/bin/pandoc')
##knitr::opts_chunk$set(out.width = '100%')
library(leaflet)
library(knitr)
library(rmarkdown)
f = rmarkdown::render(commandArgs(TRUE))




