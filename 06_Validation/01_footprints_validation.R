# 23 Nov. 2021

# I am extracting footprints from the ET maps (TUCC and 2020).

#### Libraries ####

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

load("D:/Stenka_UWI/Topic_3/04_PROCESSED_DATA/20211014_zd_firstguess/TUCC/20211014_TUCC_final.RData")
# 59857obs. of 33 variables

## rename columns to match Alby function

names(TUCC_final)[names(TUCC_final) == 'wind_speed'] <- 'ws'
names(TUCC_final)[names(TUCC_final) == 'wind_dir'] <- 'wd'
names(TUCC_final)[names(TUCC_final) == 'friction_velocity'] <- 'u.'
names(TUCC_final)[names(TUCC_final) == 'northward_wind'] <- 'v_var'
names(TUCC_final)[names(TUCC_final) == 'obukhov_length'] <- 'L'

# Subset to 2020 

TUCC_final$timestamp = as.POSIXct(TUCC_final$timestamp, tz = "GMT")

TUCC_final$Year = lubridate::year(TUCC_final$timestamp)

EC_TUCC_2020 = subset(TUCC_final, Year == 2020) # 17568 obs. 

# This time, I am not removing rows where NAs appear for parameters necessary for footprint modelling.

nrow(EC_TUCC_2020) # 17568

#### Load ET maps ####

# This is my predicted ET, saved as a list of dataframes (xyz format).

map_list = readRDS("D:/Stenka_UWI/Topic_3/05_RESULTS/20211117_ET_maps_towers/2020/TUCC/ETmaps_TUCC_2020.rds")

length(map_list) # 17568

# convert to rasters 

maps_l_rast = pbapply::pblapply(
  1:length(map_list), function(i)
    raster::rasterFromXYZ(map_list[[i]])
)

# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07m 35s

# Large List (17520 elements, 5.8 GB)

plot(maps_l_rast[[6700]]) 
plot(maps_l_rast[[9293]]) 

maps_brick = raster::brick(maps_l_rast) # large RasterBrick (1.6 MB)

plot(maps_brick[[6700]])

#### Run footprints ####

n = nrow(EC_TUCC_2020)
n # 17568

FP_TUCC_ET_20 <- pbapply::pbsapply(1:n, function(i)
  
  extract_fp(
    
    fetch = 1000, zm = 56, grid = 200, lon = 386525.1, lat = 5819332, 
    
    speed = zoo::na.approx(EC_TUCC_2020$ws)[i], # FP input variables
    
    direction = zoo::na.approx(EC_TUCC_2020$wd)[i],
    
    uStar = zoo::na.approx(EC_TUCC_2020$u.)[i],
    
    zd = zoo::na.approx(EC_TUCC_2020$zd)[i],
    
    v_var = zoo::na.approx(EC_TUCC_2020$v_var)[i],
    
    L = zoo::na.approx(EC_TUCC_2020$L)[i],
    
    timestamp = EC_TUCC_2020$timestamp[i],
    
    FP_probs = 0.9925,# FP probability
    
    fix_time = FALSE,
    
    df_input = FALSE,
    
    input_raster = maps_brick[[i]], # is need to include [[i]] after the raster name
    
    resample_raster = FALSE,
    
    extract_list = TRUE, # if TRUE extract also the point, extent and buffer
    
    buffer = 500)) # 500m buffer

#  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=35m 09s

FP_TUCC_ET_20_df <- tibble("timestamp" = EC_TUCC_2020$timestamp[1:n],
                           
                           "ET_FP" = unlist(FP_TUCC_ET_20[1,]),
                           
                           "ET_point" = unlist(FP_TUCC_ET_20[2,]),
                           
                           "ET_extent" = unlist(FP_TUCC_ET_20[3,]),
                           
                           "ET_buffer" = unlist(FP_TUCC_ET_20[4,]) )

ggplot(FP_TUCC_ET_20_df) + 
  geom_line(aes(x = timestamp, y = ET_FP)) + 
 # geom_line(aes(x = timestamp, y = ET_point), color = "green") 
#  geom_line(aes(x = timestamp, y = ET_buffer), color = "red")

#write.csv(FP_TUCC_ET_20_df, "D:/Stenka_UWI/Topic_3/05_RESULTS/20211117_ETmaps_ftp/2020/TUCC/FP_TUCC_ET_20.csv", row.names = FALSE)

#save(FP_TUCC_ET_20_df, file = "D:/Stenka_UWI/Topic_3/05_RESULTS/20211117_ETmaps_ftp/2020/TUCC/FP_TUCC_ET_20_df.RData")