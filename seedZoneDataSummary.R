###
# Developed addational evaluatation of seed zones for ERUM 
# 20201217
# carverd@colostate.edu 
###
library(dplyr)
library(raster)
library(tmap)
library(sf)
tmap_mode("view")

baseDir <- "D:/usda/rcJohnson/buckwheat/"

# final seed zone map 
sz <- raster::raster(paste0(baseDir, "projectedOutputs/combined_OSeedZones.tif"))


# files for all predictors 
f1 <- list.files(path = paste0(baseDir, "NA_NORM_8110_Bioclim_ASCII"), pattern = ".tif", full.names = TRUE) 
#select all the orignal features 
f2 <- f1[c(1,4,7,9,11,13,15,17,19,21,23)]
## so not all of the omerik ecoregions exists so just use the images cut to states 

# read in orginal occurrence data 
d1 <- read.csv("D:/usda/rcJohnson/analysisData/ERUMlocationMeansv621Climate 19dec19.csv")
coords <- d1[,c("Longitude2","Latitude2")]
sp1 <- sp::SpatialPointsDataFrame(coords = coords, data = d1, proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
sp1 <- sp::spTransform(sp1, CRSobj = sz@crs)
### attach seed zones to all occurrence data 
sp1$seedZone <- raster::extract(x = sz, y = sp1)

# seed zones with no 
sp2 <- sp1[is.na(sp1$seedZone),]
tm_shape(sp2)+
  tm_dots()+
  tm_shape(sz)+
  tm_raster()
### a few of these do not line up with seed zones... not sure why that is and it is something I will need to evaluate. 
tm_shape(sp1)+
  tm_dots(col="seedZone")
write.csv(sp1@data, file = paste0(baseDir, "tabularOutputs/occurrenceDataWithSeedZones.csv"))
### generate a table that shows the average value for all predictors for each seed zone 
vals <- sort(unique(raster::values(sz)))
df <- data.frame(matrix(nrow = length(vals), ncol = 12))
colnames(df) <- c("seedzone", "ahm", "emt", "ext", "map", "mat", "mcmt", "msp", "mwmt","nffd","pas","td")
df$seedzone <- vals
for(i in seq_along(vals)){
  print(i)
  r1 <- sz
  r1[r1[]!= vals[i],] <- NA
  r1[!is.na(r1),] <- 1
  for(j in seq_along(f2)){
    print(j)
    m1 <- raster::raster(f2[j]) *r1
    df[i, j+1] <- raster::cellStats(m1, stat = mean)
  }
}
# write csv 
write.csv(x = df, file = paste0(baseDir, "tabularOutputs/meanValuePerSeedZone.csv"))

### this is a nice little problem, a good one for a lesson possible. 


### generate area measures for the 12 seed zones. 
# list of uniqu sz
szlist <- unique(values(sz))
szlist <- szlist[2:length(szlist)]

# create dataframe for holding area 
df <- data.frame(matrix(nrow = 12, ncol = 3))
names(df) <- c("seed zone", "n cells", "area m2")
df$`seed zone` <- szlist

# determine ncells and area for each seed zone 
for(i in 1:length(szlist)){
  r1 <- sz 
  r1[r1 != szlist[i], ]<- NA
  # area <- raster::area(r1, na.rm = TRUE)
  ncell <- values(r1)
  ncell <- ncell[!is.na(ncell)]
  df$`n cells`[i] <- length(ncell)
}

df$`area m2` <- df$`n cells` *1000000
df$`area hectare` <- df$`area m2`*0.0001

df$newName <- c(12,10,11,9,7,6,5,8,4,3,2,1)
df
write.csv(df, file = paste0("D:/usda/rcJohnson/buckwheat/projectedOutputs/percentArea", Sys.Date(),".csv"))














