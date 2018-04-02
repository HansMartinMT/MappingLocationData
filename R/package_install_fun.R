#function to install and load required packages
ipak <- 
  function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
#packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster","ggplot2","colorRamps","rgeos","leaflet","lubridate", "htmlwidgets","move", "tmap","grid")

#run function to install packages

#ipak(packages)

