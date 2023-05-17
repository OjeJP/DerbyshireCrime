#instructions
#1) download shapefiles
#2) install packages
#3) run libraries
#4) change directory location in this code. There are two places where you need to do this.
#5) explore how to make maps (e.g. add colour etc.)




#install packages
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("png")
install.packages("rgeos")
install.package("rio")

#libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(rgdal)
library(png)
library(rgeos)
library(rio)

#download shape files to directory
#change the directory location below
shp2<- shapefile("C:/Users/301866/OneDrive - University of Derby/Teaching/2023/Visualisation/Dataset/shapefiles2/shapefiles2/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx")


#import your data
regions<- rio::import("C:/Users/301866/OneDrive - University of Derby/Teaching/2023/Visualisation/Dataset/AssessmentCrimeData.csv")

#make sure that the column name for LSOA codes  match with the name in the shapefiles. In this case the columns name is "LSOA11CD".
shp_main<-subset(shp2, LSOA11CD %in% regions$LSOA11CD)

#add variables to the shapefile. for instance crime data
shp_main$"LSOA Code"<-regions$LSOA11CD
shp_main$"Region Name"<-regions$Name
shp_main$"Land Area"<- regions$`Land Area (Hectares)`
shp_main$"Population"<-regions$Population
shp_main$"Population Density"<- regions$Population/regions$`Land Area in Hectares`
shp_main$"log10(ASB Density)"<- log10(regions$`Anti-Social Behaviour`/regions$`Land Area in Hectares`)
shp_main$"log10(Shoftlifting Density)"<- log10(regions$Shoplifting/regions$`Land Area in Hectares`)

#example 1
#this is the code to make a map. Currently this will make a map of Derbyshire colored to population density
tm_shape(shp_main) +
  
  tm_polygons ("Population Density", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)

#example 2
#this is the code to make a map. Currently this will make a map of Derbyshire colored to log10(ASB Density)
tm_shape(shp_main) +
  
  tm_polygons ("log10(ASB Density)", palette = "YlGn") +
  tm_layout +
  tm_borders(lwd=1)


#example 3
tm_shape(shp_main) +
  
  tm_polygons ("log10(Shoftlifting Density)") +
  tm_layout +
  tm_borders(lwd=1)





