###
# generating seed zone maps for ERUM 
# 20201022 
# carverd@colostate.edu 
###
library(raster)
library(sf)
library(tmap)

baseDir <- "F:/usda/rcJohnson/buckwheat"
pal <- rev(tmaptools::get_brewer_pal("Spectral", n = 12, contrast = c(0, 1)))
## I'll need to change order of the color pallette back to original for the final seed zone map  
proj <- raster::crs("+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77")

ecos <- sf::st_read(paste0(baseDir,
                           "/finalMaps/Omeric_ecoregionsOfInterest.shp"))
states <- sf::st_read(paste0(baseDir,
                             "/finalMaps/statesOfInterest.shp"))
grad <- sf::st_read("F:/genericSpatialData/world/ne_10m_graticules_all/ne_10m_graticules_5.shp")
points <- sf::st_read("F:/usda/rcJohnson/analysisData/ERUMlocationMeansv621Climate 19dec19.csv",
                      options=c("X_POSSIBLE_NAMES=Longitude2","Y_POSSIBLE_NAMES=Latitude2"))%>%
  st_set_crs(st_crs(grad))%>%
  st_transform(crs = crs(proj))

grad <- sf::st_transform(grad, crs(proj)) %>%
  sf::st_crop(states)

saveMap <- function(map, name){
  tmap_save(tm = map, filename = paste0(baseDir,"/projectedOutputs/", name,".png"),
            width = 12, height = 15, units = "in", dpi = 600)
}

createMap <- function(raster, nClass, states,grad,ecos,points){
  m1 <- tm_shape(grad) + 
    tm_lines(lty = "dashed", col = "black") + 
     tm_shape(states) + 
       tm_polygons(col = "white", border.col = "black")+
    tm_shape(raster)+
      tm_raster(palette = "-Paired", style = "cat", n = nClass)+
    tm_shape(ecos)+ 
    tm_borders(col = "black")+
    tm_shape(points)+
      tm_bubbles(size = 0.5,alpha = 0.1, border.col = "black", border.lwd = 1)+
      tm_layout(legend.outside = TRUE) +
    tm_scale_bar(color.dark = "gray60",
                          position = c("left", "bottom"))

  return(m1)
}

### general process 
# reclassify the can1 and can2 variables 
# overlay the new classifications 
# ensure unique values are kept 

# read in can1 and can2 
can1 <- raster::raster(paste0(baseDir,"/projectedOutputs/Can1_Oeco.tif"))
can2 <- raster::raster(paste0(baseDir,"/projectedOutputs/Can2_Oeco.tif"))

#classify based on new values   
can1[can1[] < -2.2464, ] <- NA
can1[can1[] > 1.7621, ] <- NA
c1m <- matrix(nrow = 4 ,ncol =3 ,data = c(-2.2464, -1.2443, -0.2422, 0.7600, 
                                          -1.2443, -0.2422, 0.7600, 1.7621, 
                                          1,2,3,4))
can1_re <- raster::reclassify(x = can1, rcl = c1m, byrow = TRUE)
# raster::writeRaster(x= can1_re,  filename = "F:/usda/rcJohnson/buckwheat/projectedOutputs/Can1_Oreclass.tif")

# tmap_save(tm = m3,
#           filename = "F:/usda/rcJohnson/buckwheat/projectedOutputs/Can1_OreclassMap.jpeg",
#           dpi = 600,  width = 12, height = 15, units = "in")


#### can 2 
can2[can2[] < -3.1078, ] <- NA
can2[can2[] > 1.2934, ] <- NA
c2m <- matrix(nrow = 3 ,ncol =3 ,data = c(-3.1078, -1.6407, -0.1737,  
                                          -1.6407, -0.1737, 1.2934,
                                          10,20,30))
can2_re <- raster::reclassify(x = can2, 
                              rcl = c2m,
                              byrow = TRUE)
# raster::writeRaster(x = can2_re,
#                     filename = "F:/usda/rcJohnson/buckwheat/projectedOutputs/Can2_Oreclass.tif")

c2 <- raster("F:/usda/rcJohnson/buckwheat/projectedOutputs/Can2_Oreclass.tif")

### final seed zone map 
can <- can1_re + can2_re
# 
# raster::writeRaster(x = can,
#                     filename = "F:/usda/rcJohnson/buckwheat/projectedOutputs/combined_OSeedZones.tif")

seedzones <- createMap(raster = can,
                       nClass = 12, 
                       states = states,
                       grad = grad,
                       ecos = ecos,
                       points = points)

tmap_save(tm = seedzones,
          filename = "F:/usda/rcJohnson/buckwheat/projectedOutputs/Combined_OreclassMap_Paired_2.jpeg",
          dpi = 600,  width = 12, height = 15, units = "in")


# convert to shapefile 
sh1 <- raster::rasterToPolygons(x = can)
# write out shape
rgdal::writeOGR(obj = sh1,
                dsn = "F:/usda/rcJohnson/buckwheat/finalMaps",
                layer = "OseedZoneAreas.shp",
                driver="ESRI Shapefile")

m3 <- tm_shape(shp = can)+
  tm_raster(style = "cat", n = 12, palette = "-Spectral",saturation = 1,contrast = 1,)+
  tm_layout(legend.outside = TRUE)
tmap_save(tm = m3,
          "F:/usda/rcJohnson/buckwheat/projectedOutputs/combined_0SeedZones.jpeg",
          dpi = 600)




d1 <-can@data@values
d1 <- d1[!is.na(d1)]
d2 <- data.frame(matrix(data = d1, nrow = length(d1), ncol = 1))
colnames(d2) <- "value"
d3 <- d2 %>%
  dplyr::group_by(value) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percentArea = (count/ nrow(d2))*100) %>%
  dplyr::mutate(hectare = count * 100)
View(d3)
colnames(d3) <- c("seed zone index", "number of 1000m2 cells in seed zone", "Percentage of total area the seed zone occupies", 
                  "area in hectares")
# Each cell is 1000 Square Meters 
# 1000 square meter = 0.1 hectacre. 
# Multiple the count by 1000 to get square meters then by 0.1 to get hectace (or 100)
write.csv(x = d3, file = "F:/usda/rcJohnson/buckwheat/projectedOutputs/percent_OArea.csv")


### 20201201 - not running this content at the moment... 

### extras climate variables 
# generate maps of (MAT, MAP MWMT, MCMT, TD, MSP, AHM, NFFD, PAS, EMT, EXT)
# with ecoregions overlayed. 
### read in ecoregion files  
ecos <- sf::st_read("F:/usda/rcJohnson/buckwheat/finalMaps/ecoregionsOfInterest.shp")
# list all imput feature files 
files <- list.files(path = "F:/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII", pattern = "eco.tif",
                    full.names = TRUE)

for(i in seq_along(files)){
  print(i)
  r1 <- raster::raster(files[i])
  name <- substr(x = files[i], start = nchar(files[i])-10, stop = nchar(files) -8 )
  map <- tm_shape(r1) +
    tm_raster(palette = "-Spectral", style = "cat")+
    tm_layout(legend.outside = TRUE)+
    tm_shape(ecos) +
      tm_borders(col="black", lwd = 1)
  tmap_save(tm = map,
            filename = paste0(baseDir, "/NA_NORM_8110_Bioclim_ASCII/",name, "_basicMap.jpeg"),
            dpi = 600, 
            )
}

### reverse color Schemes on previous single variate maps. 
# add R squared value on the maps 
# done 
# send images to RC and if he likes it rework the map details and include the R squared values 
