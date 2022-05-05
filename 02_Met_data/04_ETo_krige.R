library(ggspatial)
library(gstat)
library(mapview)
library(rdwd)
library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(WindVerification) 
library(xts)
library(MeTo)
#install.packages('bit64')
library('bit64')
library(devtools)

library(raster)

# Benchmarking 
library(tictoc)


load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
berlin.Border <- st_transform(berlin.sf, crs="+proj=longlat +datum=WGS84")
##################################################################################################
# tower points
tower_points <- matrix(c(13.315827, 13.32785, 52.457232, 52.51228),ncol=2)
tower_points <- data.frame(x = tower_points[,1], y = tower_points[,2])
coordinates(tower_points) = c("x", "y") 
proj4string(tower_points) <- CRS("+proj=longlat +datum=WGS84")
tower_points_utm <- spTransform(tower_points, "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

plot(berlin.Border)


#### Load data ####

load("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/06_R_SCRIPTS/20210716_ETo_interpol/station_70km_ETo.RData")
load("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211018_ETo/Halfhourly/ETo_halfhour_ws2.RData")


ETo_stations = data.frame(x = station_70km_ETo$geoLaenge, y = station_70km_ETo$geoBreite ) # 10 stations


coordinates(ETo_stations) = c("x", "y") 
proj4string(ETo_stations) <- CRS("+proj=longlat +datum=WGS84")
ETo_stations <- spTransform(ETo_stations, "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

mapview(ETo_stations) 

grd <- makegrid(ETo_stations, cellsize = 1000)
colnames(grd) <- c("x", "y")
coordinates(grd) <- ~x+y # set coordinates
proj4string(grd) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
gridded(grd) <- TRUE 

r_grd <- raster(grd)
r_grd

st_grid <- rasterToPoints(r_grd, spatial = TRUE)
gridded(st_grid) <- TRUE
st_grid <- as(st_grid, "SpatialPixels")

coords_St_grid <- st_grid@coords
saveRDS(coords_St_grid, file = "coords_St_grid.rds")

Berlin_ext <- extent(st_transform(berlin.Border, "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
Berlin_ext@xmax <- Berlin_ext@xmax + res(r_grd)[1] #resolution
Berlin_ext@ymax <- Berlin_ext@ymax + res(r_grd)[1] #resolution

summary(ETo_halfhour_ws2)

##### function to interpolate and crop Berlin
#Interp_DWD = function(var_vector, station_points, grid, ext_berlin) {
#  station_points$var_i = t(var_vector)[2:(length(station_points@coords[,1])+1),1] # transpose
 # vgm_var = gstat::variogram(object = var_i ~ 1, data = station_points) # set a variogram
#  fit_var = gstat::fit.variogram(object = vgm_var, vgm("Exp")) # fit a variogram
 # krg_var = gstat::krige(var_i ~ 1, locations = station_points, newdata = grid, model = fit_var)
 # krg_var = raster::raster(krg_var)
 # krg_var = raster::crop(x = krg_var, y = ext_berlin)
 # return(krg_var)
#}

#krg_ETo <- lapply(1:10, FUN = function(i) Interp_DWD(
#  var_vector = ETo_halfhour_ws2[i,],
#  station_points = ETo_stations,
#  grid = st_grid,
#  ext_berlin = Berlin_ext)) # year_month_day

#plot(krg_ETo[[1]])

glimpse(ETo_halfhour_ws2)

library(pbapply) # trying this library instead of lapply

n = nrow(ETo_halfhour_ws2)
n

tic("Kriging") # tic - Starts the timer and stores the start time and the message on the stack.

var_i <- pbapply::pblapply(1:n, FUN = function(i) t(ETo_halfhour_ws2[i,])[2:(length(ETo_stations@coords[,1])+1),1])

vgm_var = pbapply::pblapply(1:n, FUN = function(i) gstat::variogram(object = var_i[[i]] ~ 1, data = ETo_stations))

fit_var = pbapply::pblapply(1:n, FUN = function(i) gstat::fit.variogram(object = vgm_var[[i]], vgm("Sph")))

krg_var1 = pbapply::pblapply(1:n, FUN = function(i) gstat::krige(var_i[[i]] ~ 1, locations = ETo_stations, newdata = st_grid, model = fit_var[[i]]))

krg_var2 = pbapply::pblapply(1:n, FUN = function(i) raster::raster(krg_var1[[i]]))

krg_var3 = pbapply::pblapply(1:n, FUN = function(i) raster::crop(x = krg_var2[[i]], y = Berlin_ext) )

toc(log = TRUE) #Notes the current timer and computes elapsed time since the matching call to tic().
# When quiet is FALSE, prints the associated message and the elapsed time.

# Kriging: 3737.125 sec elapsed

plot(krg_var1[[8665]])
plot(krg_var2[[8665]])
plot(krg_var3[[8665]])

## save the dataset

#save(krg_var3, file = "/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211019_halfh_kriged_ETo/krg_var3.RData")

#saveRDS(krg_var3, file = "/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211019_halfh_kriged_ETo/krg_var3.rds")

krg_var3 = readRDS(file = "/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211019_halfh_kriged_ETo/krg_var3.rds")

load("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211019_halfh_kriged_ETo/krg_var3.RData")
