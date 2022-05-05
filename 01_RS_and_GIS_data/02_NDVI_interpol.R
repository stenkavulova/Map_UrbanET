# Libraries 

library(sp)
library(raster)
library(rgdal)
library(lubridate)
library(maptools)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(scales)
library(readr)
library(rgeos)

library(grid)
library(spatstat)
library(sf)

library(zoo)
library(tictoc) # benchmarking

#### Berlin border ####

load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
berlin.Border <- st_transform(berlin.sf, crs="+proj=longlat +datum=WGS84")

plot(berlin.Border)

#### Import NDVI ####

# I am loading 2019 Sentinel-2 NDVI data with 10-m spatial resolution. 

# The total number of files for 2019 is 101.

filenames = list.files(path = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/ndvi_time_series_berlin/final/2019",pattern=".tif$",full.names = TRUE)

l = lapply(filenames, raster) # list of rasters (101 rasters)

l_stack = raster::stack(l)

# crop to Berlin border 

tic("crop raster stack")

NDVI_Border <- raster::mask(l_stack, berlin.Border)

toc(log=TRUE)
# crop raster stack: 8970.36 sec elapsed


plot( NDVI_Border[[88]] )

#### First approxNA ####

tic("approxNA first round") # tic - Starts the timer and stores the start time and the message on the stack.
NDVI_FP_filled <- raster::approxNA(x = NDVI_Border, rule = 2)
toc(log = TRUE) #Notes the current timer and computes elapsed time since the matching call to tic().
# approxNA first round: 1179.3 sec elapsed


plot(NDVI_FP_filled[[101]])
plot(berlin.Border, add=T, col="transparent")

names(NDVI_FP_filled) <- names(l_stack)

# I created a NULL raster
NDVI_raster_null <- NDVI_FP_filled[[66]]
values(NDVI_raster_null) <- NA

#### Create a timestamp ####
#Just 2019
# Generate Regular Sequences of Dates
tsNDVI = seq.Date(as.Date("2018-12-31", tz="UTC"),
                  as.Date("2019-12-31", tz="UTC"),
                  by = "day")[-1]  
# YYYY-MM-DD
head(tsNDVI)
tail(tsNDVI)

#### Timestamp names ####
# I am making timestamp names which are the same as the names of the NDVI images.
# create the timestamp name to be the same of the NDVI images

tsNDVI_names <- sapply(1:length(tsNDVI), FUN=function(i)
  paste0("S2_L2A_GEE_BERLIN_NDVI_",
         substring(tsNDVI[i], 1, 4),".",
         substring(tsNDVI[i], 6, 7),".",
         substring(tsNDVI[i], 9, 10) 
  ))

tsNDVI_names[66]

#### Create a rasterstack with all days ####

# create a raster stack with the 365 days 
# 365 days of NA
NDVI_raster_daily <- c(replicate(length(tsNDVI_names), NDVI_raster_null))
NDVI_raster_daily <- stack(NDVI_raster_daily)
names(NDVI_raster_daily) <- tsNDVI_names

#### Replace real NDVI images in the NA raster stack ####
# Here, I add back the 101 real NDVI images I have for 2019.

# I replaced the raster with the non-null 101 images that I had according to the name
for (i in 1:101) {
  NDVI_raster_daily[[names(NDVI_FP_filled)[i]]] <- NDVI_FP_filled[[i]]
}

# test it worked 
plot(NDVI_raster_daily[[2]]) # yes, real NDVI
plot(NDVI_raster_daily[[3]]) # nothing 
plot(NDVI_raster_daily[[14]])

#### Interpolate ####
# then I interpolate with the function approxNA to have a daily stack raster

tic("approxNA for 2019") # tic - Starts the timer and stores the start time and the message on the stack.
NDVI_Berlin_daily <- raster::approxNA(x = NDVI_raster_daily, rule = 2)
toc(log = TRUE) #Notes the current timer and computes elapsed time since the matching call to tic().

# approxNA for 2019: 2443.93 sec elapsed

# test it worked 
plot(NDVI_Berlin_daily[[2]]) # yes, real NDVI
plot(NDVI_Berlin_daily[[3]]) # yes, interpolated one 

writeRaster(NDVI_Berlin_daily, "D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/NDVI_Berlin_daily_2019")

NDVI_loaded_2019 = raster::stack("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/NDVI_Berlin_daily_2019")

# load sp before plotting 
# https://stackoverflow.com/questions/39911076/r-error-in-as-doubley-cannot-coerce-type-s4-to-vector-of-type-double
sp::plot(NDVI_loaded_2019[[5]])

plot(NDVI_loaded_2019[[77]])

#### Crop to ROTH #### 

# I will use this grid at ROTH to crop the rasters. 

Grid_ROTH  <- readOGR(dsn='D:/Stenka_UWI/Topic_2/04_PROCESSED_DATA/20200721_zd_grids/ROTH',layer='Grid_ROTH') 

plot(Grid_ROTH)

Grid_ROTH

ROTH_lonlat = sp::spTransform(Grid_ROTH, CRS("+proj=longlat +datum=WGS84 +no_defs"))

tic("crop") # tic - Starts the timer and stores the start time and the message on the stack.
ROTH_NDVI_2019 = raster::crop(x = NDVI_Berlin_daily, y = ROTH_lonlat)
toc(log = TRUE) #Notes the current timer and computes elapsed time since the matching call to tic().

plot(ROTH_NDVI_2019[[365]] )

writeRaster(ROTH_NDVI_2019,
            "D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/ROTH/ROTH_NDVI_2019.tif",
            options='COMPRESS=LZW')

ROTH_loaded = raster::stack("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/ROTH/ROTH_NDVI_2019.tif")

plot(ROTH_loaded[[344]])

#### Crop to TUCC #### 

# I will use this grid at TUCC to crop the rasters. 

Grid_TUCC  <- readOGR(dsn='D:/Stenka_UWI/Topic_2/04_PROCESSED_DATA/20200721_zd_grids/TUCC',layer='Grid_TUCC') 

plot(Grid_TUCC)

Grid_TUCC

TUCC_lonlat = sp::spTransform(Grid_TUCC, CRS("+proj=longlat +datum=WGS84 +no_defs"))

tic("crop") # tic - Starts the timer and stores the start time and the message on the stack.
TUCC_NDVI_2019 = raster::crop(x = NDVI_Berlin_daily, y = TUCC_lonlat)
toc(log = TRUE) #Notes the current timer and computes elapsed time since the matching call to tic().

plot(TUCC_NDVI_2019[[365]] )

writeRaster(TUCC_NDVI_2019,
            "D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/TUCC/TUCC_NDVI_2019.tif",
            options='COMPRESS=LZW')

TUCC_loaded = raster::stack("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2019/TUCC/TUCC_NDVI_2019.tif")

plot(TUCC_loaded[[77]])
