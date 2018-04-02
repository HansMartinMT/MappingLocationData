##Create a spatial points dataframe from camera data

Map_UTM_YHT<-function(easting,northing,coordref,locdat){
  #specify the easting and northing in UTMs with the corresponding coordinate reference system
  
  #function to install and load required packages
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  #load or install these packages:
  packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster",
                "ggplot2","colorRamps","rgeos","leaflet","lubridate", "htmlwidgets","move", "tmap","grid")
  
  #run function to install packages
  ipak(packages)
coords<-cbind(easting,northing) #vector of coordinates
#coordref<- CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") #specify the coordinate reference system
sp=SpatialPoints(coords, #vector of coordinates
                 coordref #coordinate reference system
) 

locspdf<-SpatialPointsDataFrame(
  coords=sp, 
  data=locdat #add data to make this a spatial points data frame
)

#Map Layers

#Study Extent
studyextent<-readOGR("YHT Study Extent/YHTstudy_extent.shp")
studyextent<-spTransform(studyextent,CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #reproject using the landcover projection
#Ya Ha Tinda Boundary
yahatinda<-readOGR("YaHaTindaShape")
proj4string(yahatinda)<-CRS("+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
yahatinda<-spTransform(yahatinda,CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#Park Regions
BanffRegions<-readOGR("C:/Users/Hans Martin/Documents/R_Data/AnnualReportFigureData/Basebnp_region/basebnp_region.shp")
BanffRegions<-spTransform(BanffRegions,CRS("+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

locmap<-
  #tm_shape(elevation)+ #loads file (either shape file, raster file, etc...)
  #tm_raster()+#specifies how r should plot the file (either as a raster, polygon, line, symbol)
  tm_shape(BanffRegions)+ # loads banff shape file
  tm_polygons(alpha = .5)+ #plots the shapefile as a polygonalpha =0.5 controls transparency (0-1)
  tm_shape(studyextent)+# loads banff shape file
  tm_borders(col = "Red")+ #plots only the boundary of the shapefile 
  tm_shape(yahatinda)+
  tm_polygons(col="darkgray")+
  
  tm_shape(yhtcamlocspdf[yhtcamlocspdf$park!="YHT Grasslands",])+
  tm_symbols(size=0.07, #plots the spatialpoints dataframe as symbols
             col="locinfo", #color is associated with the column "locinfo" in the spatialpointsdataframe
             labels = unique(locinfo),
             #title.col =  "Camera Ownership")+
  tm_style_col_blind()+ #uses a colorblind cooperative colorscheme for the symbols
  
  tm_compass(position=c("right", "top"))+ #inserts a compass showing north.
  
  tm_add_legend(type="line", col=c("red"), title="Study Area") + #adds a custom legend
  
  tm_add_legend(type="fill", col=c("lightgrey"), title="National Park Boundary") + #adds a custom legend 
  
  tm_add_legend(type="fill", col=c("darkgray"), title="Ya Ha Tinda Ranch") +#adds a custom legend
  
  
  tm_layout(
    main.title = "Camera Traps Operated by Lake Louise, Banff, and the Ya Ha Tinda Research Project",#text for main title
    main.title.size = 0.745, #size of main title
    main.title.position= c("center"),#position of main title ("top","right","bottom")
    #outer.margins = c(0,0,0,0.01), 
    inner.margins = c(0.01,0.01,0.01,0.11), 
    #between.margin = 0.1,
    compass.type = "arrow", #type of compass rose
    legend.outside = T, # places the legend inside the map or outside
    legend.title.size=0.75,
    legend.text.size=0.5,
    legend.just = c("right", "top")#specifies legend location
  ) 

#create an inset map
yahatindamap<-tm_shape(yahatinda)+ #spatial data to use
  tm_polygons(col="darkgrey")+
  tm_shape(yhtcamlocspdf)+
  tm_symbols(col="darkgreen", size=0.15)+
  tm_layout(main.title = "Ya Ha Tinda Ranch",
            main.title.size = 0.6, 
            main.title.position= c("center"))

# save_tmap(tm=camlocmap, #object name
#           filename="cameralocationfigure.tiff", #save map to this filename
#           width=5, #width of saved image
#           height=4, #height of saved image
#           units="in", #units "in", "cm"
#           dpi=600, #dpi
#           insets_tm = yahatindamap, #tmap object to inset
#           insets_vp=viewport(x=0.2, y=0.155, width=0.4, height=0.4) #location of the inset map
# )

}