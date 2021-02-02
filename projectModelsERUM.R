###
# Develop content for buckwheat modeling 
# 20201022
# carverd@colostate.edu
###

library(raster)
library(tmap)
library(dplyr)
library(sp)
library(sf)
tmap_mode("view")

### define CRS taken from https://adaptwest.databasin.org/pages/adaptwest-climatena
proj <- raster::crs("+proj=lcc +lon_0=-95 +lat_1=49 +lat_2=77")

### generate extent from occurrences 
d1 <- read.csv("F:/nrelD/usda/rcJohnson/analysisData/ERUMlocationMeansv621Climate 19dec19.csv")
ex <- raster::extent(min(d1$Longitude.1), max(d1$Longitude.1), min(d1$Latitude), max(d1$Latitude))

# # read in states 
s1 <- raster::shapefile("F:/nrelD/genericSpatialData/US/states/tl_2017_us_state.shp")
# s1 <- s1[s1$STUSPS %in% c("WA", "OR", "CA","AZ", "NV", "ID", "MT", "WY", "UT", "CO", "NM"),]
# s1 <- sp::spTransform(s1, crs(proj))
# st_write(st_as_sf(s1), "F:/nrelD/usda/rcJohnson/buckwheat/finalMaps/statesOfInterest.shp", driver="ESRI Shapefile", delete_dsn = TRUE)

### use the ecoregions in which points were collected instead of the states to bound the model 
# read in eco regions 
e1 <- sf::read_sf("F:/nrelD/genericSpatialData/US/omerikEcoregions/us_eco_l3/us_eco_l3.shp")
# generate point object from lat long values

d1$Latitude.1 <- as.numeric(d1$Latitude.1)
d1$Longitude.1 <- as.numeric(d1$Longitude.1)
p1 <- sp::SpatialPoints(coords = d1[,c(41,40)], proj4string = crs(s1))
p2 <- sp::spTransform(p1, crs(e1))
# need to convert from a sp to sf object in order to run sf funciton 
p2 <- st_as_sf(p2)
#extract ecoregion values 
p3 <- sf::st_intersection(p2, e1)

e2 <- e1[e1$US_L3NAME %in% unique(p3$US_L3NAME),]
e2 <- sf::st_transform(e2, proj) 
#rgdal::writeGDAL(dataset = e2, fname = "F:/nrelD/usda/rcJohnson/buckwheat/finalMaps/ecoregionsOfInterest.shp", drivername = "ESRI Shapefile")
st_write(st_as_sf(e2), "F:/nrelD/usda/rcJohnson/buckwheat/finalMaps/Omeric_ecoregionsOfInterest.shp", driver="ESRI Shapefile")

e3 <- as(e2,"Spatial")

# need to convert this to meters inorder to match the projection. 
rPrep <- function(raster, projection,  mask){
  crs(raster) <- projection
  r <- raster::crop(raster, mask) %>%
    raster::mask(mask)
  return(r)
}
outDir <- "F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/"

### read in relivent rasters 
# AHM (annual heat:moisture index), 
r1 <- raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/AHM.asc")
ahm <- rPrep(r1,
             proj, 
             e3)
raster::writeRaster(ahm, filename = paste0(outDir, "ahm_Oeco.tif"))
# EMT (30 year extreme min. temp.),
emt <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/EMT.asc"), 
             proj,
             e3)
raster::writeRaster(emt, filename = paste0(outDir, "emt_Oeco.tif"))

# EXT (30 year extreme max. temp.).
ext <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/EXT.asc"),
             proj,
             e3)
#raster::writeRaster(ext, filename = paste0(outDir, "ext_Oeco.tif"))

# MAP (mean ave. precip.),
map <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/MAP.asc"),
             proj,
             e3)
#raster::writeRaster(map, filename = paste0(outDir, "map_Oeco.tif"))

# MAT (mean ave. temp.)
mat <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/MAT.asc"), 
             proj, 
             e3)
#raster::writeRaster(mat, filename = paste0(outDir, "mat_Oeco.tif"))

# MCMT (mean coldest month temp.),
mcmt <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/MCMT.asc"), 
              proj, 
              e3)
#raster::writeRaster(mcmt, filename = paste0(outDir, "mcmt_Oeco.tif"))

# MSP (mean ave. summer precp.), 
msp <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/MSP.asc"),
             proj, 
             e3)
#raster::writeRaster(msp, filename = paste0(outDir, "msp_Oeco.tif"))

# MWMT (mean warmest month temp.),
mwmt <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/MWMT.asc"),
              proj,
              e3)
#raster::writeRaster(mwmt, filename = paste0(outDir, "mwmt_Oeco.tif"))

# NFFD, frost free days), 
nffd <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/NFFD.asc"),
              proj, 
              e3)
#raster::writeRaster(nffd, filename = paste0(outDir, "nffd_Oeco.tif"))

# PAS (precipitation as snow), 
pas <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/PAS.asc"),
             proj,
             e3)
#raster::writeRaster(pas, filename = paste0(outDir, "pas_Oeco.tif"))

# TD (continentality, MWMT-MCMT),
td <- rPrep(raster::raster("F:/nrelD/usda/rcJohnson/buckwheat/NA_NORM_8110_Bioclim_ASCII/TD.asc"),
            proj, 
            e3)
#raster::writeRaster(td, filename = paste0(outDir, "td_Oeco.tif"))


outDir <- "F:/nrelD/usda/rcJohnson/buckwheat/projectedOutputs"

# Canonical variate 1
Can1 <- -9.79818 + mat * 0.3032 + td * 0.25038 + map * 0.00327 + msp * -0.00337 + ahm * -0.01356 +  pas * -0.00264 +  emt *-0.05372
raster::writeRaster(Can1, filename = paste0(outDir, "/Can1_Oeco.tif"))

# Canonical variate 2
Can2 <-  -25.8418 + mat *-1.7214 + mcmt * 0.50829 + map * 0.00156 + ahm *0.02009 + nffd *0.05013 + emt * -0.11856 + ext * 0.65382
qtm(Can2)
raster::writeRaster(Can2, filename = paste0(outDir, "/Can2_Oeco.tif"))

# Shoot wt.
Shootwt <- -69.2096 + mat * 38.72942 +  mcmt * -46.7416 +  map * 0.31101 +  pas * -0.37035 + emt * 10.9494
qtm(Shootwt)
raster::writeRaster(Shootwt, filename = paste0(outDir, "/Shootwt_Oeco.tif"))

# Bloom day
Bloomday <-  400.9691 + mat * 28.63729 + mcmt * -14.2239 + ahm * -0.37372 +  nffd * -0.48261 +  emt * 2.59468 +  ext *-7.94837 
qtm(Bloomday)
raster::writeRaster(Bloomday, filename = paste0(outDir, "/Bloomday_Oeco.tif"))

# Umbel width
Umbelwidth <- 26.46658 + mat * 5.26435 +  mwmt * -12.1966 +  mcmt * 9.09411 +  td * 11.323 + 
  ahm * -0.07219 + ext * -1.09539 
qtm(Umbelwidth)
raster::writeRaster(Umbelwidth, filename = paste0(outDir, "/Umbelwidth_Oeco.tif"))

