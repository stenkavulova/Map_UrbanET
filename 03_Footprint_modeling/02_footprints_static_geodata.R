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

library(pbapply)

devtools::install_github("AlbyDR/rSCOPE")

library(rSCOPE)

#### Load eddy data ####

load("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211014_zd_firstguess/TUCC/20211014_TUCC_final.RData")
# 59857 obs. of 47 variables

## rename columns

names(TUCC_final)[names(TUCC_final) == 'wind_speed'] <- 'ws'
names(TUCC_final)[names(TUCC_final) == 'wind_dir'] <- 'wd'
names(TUCC_final)[names(TUCC_final) == 'friction_velocity'] <- 'u.'
names(TUCC_final)[names(TUCC_final) == 'northward_wind'] <- 'v_var'
names(TUCC_final)[names(TUCC_final) == 'obukhov_length'] <- 'L'

# Subset to 2019 

TUCC_final$timestamp = as.POSIXct(TUCC_final$timestamp, tz = "GMT")

TUCC_final$Year = lubridate::year(TUCC_final$timestamp)

EC_TUCC_2019 = subset(TUCC_final, Year == 2019) # 17520 obs.

# This time, the low quality flag remains.  

# However, I need to remove rows where NAs appear for parameters necessary for footprint modelling.

# Remove NAs from columns needed for footprint calculation
TUCC_2019 = EC_TUCC_2019

TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$ws),]
TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$wd),]
TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$u.),]
TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$L),]
TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$v_var),]

TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$timestamp),]
TUCC_2019 = TUCC_2019[!is.na(TUCC_2019$zd),] # 16332 obs.

#### Load geodata ####

# Load geodata resampled to footprint

BH = raster::raster("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211021_geodata_ftp_resamp/TUCC/BH_TUCC_ftp.tif")

VH = raster::raster("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211021_geodata_ftp_resamp/TUCC/VH_TUCC_ftp.tif" )

BH_BEA = raster::raster("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211021_geodata_ftp_resamp/TUCC/BH_BEA_TUCC_ftp.tif")

ISF = raster::raster("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211021_geodata_ftp_resamp/TUCC/ISF_TUCC_ftp.tif")

VF = raster::raster("/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211021_geodata_ftp_resamp/TUCC/VF_TUCC_ftp.tif")

TUCC_brick = raster::brick(BH, BH_BEA, VH, ISF, VF)
plot(TUCC_brick)

#### Extract footprints ####

n = nrow(TUCC_2019)
n

tic("extract static footprints")

FP_TUCC_GIS_2019 = data.frame(t(pbapply::pbsapply(1:n, function(i)
  extract_fp(
    fetch = 1000, 
    zm = 56,
    grid = 200,
    lon = 386525.1,
    lat = 5819332,
    speed = na.approx(TUCC_2019$ws)[i],
    direction = na.approx(TUCC_2019$wd)[i],
    uStar = na.approx(TUCC_2019$u.)[i],
    zd = na.approx(TUCC_2019$zd)[i],
    v_var = na.approx(TUCC_2019$v_var)[i],
    L = na.approx(TUCC_2019$L)[i],
    timestamp = TUCC_2019$timestamp[i],
    FP_probs = 0.9925,
    fix_time = TRUE,
    df_input = FALSE,
    resample_raster = FALSE ,
    input_raster = TUCC_brick
  )
) ))

toc(log = TRUE)

# reset the tictoc log 
tic.clearlog()

#|++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=35m 55s
#> 
#  > toc(log = TRUE)
#extract static footprints: 2155.03 sec elapsed

# convert timestamp to POSIXct
# https://stackoverflow.com/questions/41204987/r-converting-numeric-timestamp-to-posixct-object-returns-na 

FP_TUCC_GIS_2019$timestamp = as.POSIXct(FP_TUCC_GIS_2019$timestamp, origin="1970-01-01", tz = "GMT")

write.csv(FP_TUCC_GIS_2019, "/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211022_static_ftp/TUCC/FP_TUCC_static_2019.csv", row.names = FALSE)

save(FP_TUCC_GIS_2019, file = "/Users/stenkavulova/Dropbox/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211022_static_ftp/TUCC/FP_TUCC_static_2019.RData")
