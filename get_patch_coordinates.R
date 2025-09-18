##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Make polygons just into points
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

pkg <- c("dplyr", "ggplot2", "openxlsx", "raster", "terra", "sf")

lapply(pkg, function(x) {
 if(!require(x, character.only = T))
 {
   install.packages(x)
   library(x, character.only = T)
 }
  else
    library(x, character.only = T)
})

## read the raster 
spatial <- read_sf("C://Users/mandrzej24/Documents/06_Åland_data/07_geographic_information/sampled_patches.shp")

## extract the centroids 
centroids <- st_centroid(spatial)

## Jitter the centroids by 1.1km to north or south and to 500meter west or east
jittered_centroids <- st_geometry(centroids) + c(runif(1, -0.01, 0.01), 
                                                 runif(1, -0.01, 0.01))
st_crs(jittered_centroids) ## check which coordinate system the points are in
st_crs(jittered_centroids) <- 4326 ## set to EPSG:4326
st_crs(jittered_centroids)

## writte the data
st_write(jittered_centroids, 
         "C://Users/mandrzej24/OneDrive - University of Oulu and Oamk/Documents/06_Åland_data/08_Analysis/05_Publication/jittered_points.shp")
