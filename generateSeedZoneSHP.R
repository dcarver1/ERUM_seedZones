shp <- sf::read_sf("F:/usda/rcJohnson/buckwheat/finalMaps/seedZoneAreas.shp.shp")
View(shp)
tmap::qtm(shp)

shp2 <- sf::read_sf("F:/usda/rcJohnson/buckwheat/finalMaps/OseedZoneAreas.shp.shp")

vals <- sort(unique(shp2$layer))
shp2$seedZone <- NA

n <- 1
for(i in vals){
  print(i)
  index <- grepl(pattern = i, x = shp2$layer)
  shp2$seedZone[index] <- n
  n = n+1 
}

shp3 <- shp2[,c(3,2)]
proj <- raster::crs("+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77")
sf::st_set_crs(x = shp3, value = proj)

sf::write_sf(shp3,dsn = "F:/usda/rcJohnson/buckwheat/finalMaps",
             layer = "ERUM_seedZone20210608",
             driver="ESRI Shapefile",
             overwrite= TRUE) 
# rgdal::writeOGR(obj = shp3,dsn = "F:/usda/rcJohnson/buckwheat/finalMaps",
#                 layer = "ERUM_seedZone20210608",
#                 driver="ESRI Shapefile")
                
