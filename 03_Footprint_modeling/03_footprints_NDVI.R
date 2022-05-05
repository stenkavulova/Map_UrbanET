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

load("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211014_zd_firstguess/ROTH/20211014_ROTH_final.RData")
# 52608 obs. of 47 variables

## rename columns 

names(ROTH_final)[names(ROTH_final) == 'wind_speed'] <- 'ws'
names(ROTH_final)[names(ROTH_final) == 'wind_dir'] <- 'wd'
names(ROTH_final)[names(ROTH_final) == 'friction_velocity'] <- 'u.'
names(ROTH_final)[names(ROTH_final) == 'northward_wind'] <- 'v_var'
names(ROTH_final)[names(ROTH_final) == 'obukhov_length'] <- 'L'

# Subset to 2020 

ROTH_final$timestamp = as.POSIXct(ROTH_final$timestamp, tz = "GMT")

ROTH_final$Year = lubridate::year(ROTH_final$timestamp)

# Subset to specific month
EC_ROTH_2020 = subset(ROTH_final, Year == 2020) # 17568 obs. 

# This time, the low quality flag remains.  

# However, I need to remove rows where NAs appear for parameters necessary for footprint modelling.

# Remove NAs from columns needed for footprint calculation
ROTH_2020 = EC_ROTH_2020

ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$ws),]
ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$wd),]
ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$u.),]
ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$L),]
ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$v_var),]

ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$timestamp),]
ROTH_2020 = ROTH_2020[!is.na(ROTH_2020$zd),] # 16469 obs. 

#### Load NDVI ####

# using rasterbrick as it works better 
NDVI_ROTH = raster::brick("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211004_NDVI_interpolated/2020/ROTH/NDVI_ROTH_ftp_proj_2020.tif")

plot(NDVI_ROTH[[144]])

#### Extract footprints ####

n = nrow(ROTH_2020)
n

tic("extract NDVI footprints")

FP_ROTH_NDVI_2020 = pbapply::pbsapply(1:n, function(i)
  extract_fp(
    fetch = 1000, 
    zm = 39.75,
    grid = 200,
    lon = 385566.5,
    lat = 5813229,
    speed = na.approx(ROTH_2020$ws)[i],
    direction = na.approx(ROTH_2020$wd)[i],
    uStar = na.approx(ROTH_2020$u.)[i],
    zd = na.approx(ROTH_2020$zd)[i],
    v_var = na.approx(ROTH_2020$v_var)[i],
    L = na.approx(ROTH_2020$L)[i],
    timestamp = ROTH_2020$timestamp[i],
    FP_probs = 0.9925,
    fix_time = FALSE,
    df_input = FALSE,
    input_raster = NDVI_ROTH[[lubridate::yday(ROTH_2020$timestamp[i]) ]],
    resample_raster = FALSE ,
    extract_list = TRUE, # if TRUE extract also the point, extent and buffer
    buffer = 500 # 500m buffer
  )
)

toc(log = TRUE)

# reset the tictoc log 
tic.clearlog()

#|++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=08h 44m 30s
#> 
#  > toc(log = TRUE)
#extract NDVI footprints: 31470.28 sec elapsed



FP_ROTH_NDVI_2020_df <- tibble("timestamp" = ROTH_2020$timestamp[1:n],
                                 
                                 "NDVI_FP" = unlist(FP_ROTH_NDVI_2020[1,]),
                                 
                                 "NDVI_point" = unlist(FP_ROTH_NDVI_2020[2,]),
                                 
                                 "NDVI_extent" = unlist(FP_ROTH_NDVI_2020[3,]),
                                 
                                 "NDVI_buffer" = unlist(FP_ROTH_NDVI_2020[4,]) )

write.csv(FP_ROTH_NDVI_2020_df, "D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211018_NDVI_ftp/ROTH/2020/FP_ROTH_NDVI_2020.csv", row.names = FALSE)

save(FP_ROTH_NDVI_2020_df, file = "D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211018_NDVI_ftp/ROTH/2020/FP_ROTH_NDVI_2020.RData")

ggplot(FP_ROTH_NDVI_2020_df) + 
  geom_line(aes(x = timestamp, y = NDVI_FP)) +
  geom_line(aes(x = timestamp, y = NDVI_point), color = "green") +
  geom_line(aes(x = timestamp, y = NDVI_buffer), color = "red")



load("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211018_NDVI_ftp/ROTH/2019/FP_ROTH_NDVI_07_122019.RData")

FP_NDVI_2019 = plyr::join_all(list(FP_ROTH_NDVI_07_122019_df,
                                   FP_ROTH_NDVI_2020_df),
                              type = "full")

ggplot(FP_NDVI_2019) + 
  geom_line(aes(x = timestamp, y = NDVI_FP)) +
  geom_line(aes(x = timestamp, y = NDVI_point), color = "green") +
  geom_line(aes(x = timestamp, y = NDVI_buffer), color = "red")
