library(rdwd)
library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(WindVerification)
library(xts)
library(MeTo)
library(devtools)
library(sp)

# Credit is given to Alby Duarte Rocha for this code. 
# https://github.com/AlbyDR

####################################################
### Merge the DWD data use as SCOPE input parameters
####################################################
Sys.setenv(TZ='Europe/Berlin')
devtools::session_info()

# create a timestamp
ts <- seq(as.POSIXct("2017-12-31", tz = "UTC"),
          as.POSIXct("2021-01-01", tz = "UTC"),
          by = "hour") 

# now 26329 obs. 
ts = as.data.frame(ts)
names(ts)[names(ts) == 'ts'] <- 'MESS_DATUM'


head(ts)
tail(ts)
#unique(diff((ts)))

#######################
data(fileIndex)
data(metaIndex)
data(geoIndex)

head(fileIndex)
head(metaIndex)
head(geoIndex)

unique(fileIndex$var)
# [1] "precipitation"       "air_temperature"     "extreme_temperature" "extreme_wind"       
# [5] "solar"               "wind"                "wind_test"           "kl"                 
# [9] "more_precip"         "weather_phenomena"   "soil_temperature"    "water_equiv"        
# [13] "cloud_type"          "cloudiness"          "dew_point"           "moisture"           
# [17] "pressure"            "sun"                 "visibility"          "wind_synop"         
# [21] ""                    "soil"                "standard_format" 

#### Berlin border ####

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

##################################################################################################
# air_temperature and RH
##################################################################################################
# Berlin and sunrounds before Poland border
station_70km_Ta <- nearbyStations(52.4537, 13.3017,
                                  radius = 70,
                                  res = c("hourly"),
                                  per = c("historical"),
                                  var = c("air_temperature"),
                                  mindate=as.Date("2019-12-31"))

station_70km_Ta <- station_70km_Ta[-1,]

station_70km_Ta[,1:3]
station_70km_Ta[16,1:3] # start 04/2019

station_70km_Ta <- station_70km_Ta[-16,]
station_70km_Ta <- station_70km_Ta[-17,] # too high 187m

station_70km_Ta$col <- "blue"

# save it 

write.csv(station_70km_Ta, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_Ta.csv", row.names = FALSE)

save(station_70km_Ta, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_Ta.RData")

leaflet() %>% addTiles() %>%
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_Ta, ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

link_Ta <- selectDWD(station_70km_Ta$Stationsname,
                     outvec =  TRUE,
                     #remove_dupli = TRUE,   
                     res = c("hourly"), 
                     per = c("historical"),
                     var = c("air_temperature")) 
# download file:
Ta_name <- dataDWD(link_Ta, dir = tempdir(), read = FALSE) 

# read and plot file:
TaData <- readDWD(Ta_name, varnames = FALSE, tz = "UTC") #, format = NULL

for (i in 1:16) {
  filter(TaData[[i]], year(MESS_DATUM)==2019)$TT_TU %>%
    summary()  %>%
    print()
}

for (i in 1:16) {
  filter(TaData[[i]], year(MESS_DATUM)==2019)$RF_TU %>%
    summary()  %>%
    print()
}

TaData[[8]] %>%
  filter(year(MESS_DATUM)==2019) %>%
  summary()

#TaData_check = TaData[[8]]

station_70km_Ta <- station_70km_Ta[-8,]
TaData <- TaData[-8]

station_70km_Ta$col <- "blue"

leaflet() %>% addTiles() %>%
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_Ta, ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

unique(diff(filter(TaData[[1]], year(MESS_DATUM)==2019)[,2]))
which(diff(filter(TaData[[1]], year(MESS_DATUM)==2019)[,2])==2) # 6529 8186
filter(TaData[[1]], year(MESS_DATUM)==2019)[6528:6530,2]

Ta_Set19 <- NULL
Ta_19 <- data.frame("MESS_DATUM"= ts)

for (i in 1:15) {
  Ta_Set19[[i]] <- dplyr::left_join(ts, TaData[[i]][c(2,4,5)], by = "MESS_DATUM")[2:3]
  Ta_19[i+1] <- as.numeric(zoo::na.fill(Ta_Set19[[i]][1], fill = "extend") )
  colnames(Ta_19)[i+1] <- paste0("ID_",TaData[[i]][1,1])
}

#Ta_Set19_check = Ta_Set19[[1]]
#Ta_19_i1 <- as.numeric(zoo::na.fill(Ta_Set19[[1]][1], fill = "extend") ) # 26328

colnames(Ta_19)[1] <- "timestamp"

summary(Ta_19)

# Save Ta

write.csv(Ta_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Ta.csv", row.names = FALSE)

save(Ta_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Ta.RData")

#### RH ####

rh_19 <- data.frame(MESS_DATUM=ts)

for (i in 1:15) {
  rh_19[i+1] <- as.numeric(zoo::na.fill(Ta_Set19[[i]][2], fill = "extend"))
  colnames(rh_19)[i+1] <- paste0("ID_",TaData[[i]][1,1])
}

colnames(rh_19)[1] <- "timestamp"

summary(rh_19)

### quality
for (i in 1:15) {
  tibble("ID" = i, 
         "QN" = unique(filter(TaData[[i]], year(MESS_DATUM)==2019)[3])) %>%
    print()
}

### metadata
metafiles_Ta <- readMeta(Ta_name)

metafiles_Ta_1 = metafiles_Ta[1]

for (i in 1:15) {
  tibble("ID" = i, 
         "start" = head(TaData[[i]]$MESS_DATUM, n=1),
         "Ta"  = head(TaData[[i]]$TT_TU, n=1),
         "rh"  = head(TaData[[i]]$RF_TU, n=1),
         "end" = tail(TaData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}

# height_Ta_ground - all 2m

for (i in 1:15) {
  tail(metafiles_Ta[[i]][[5]]$Geberhoehe.ueber.Grund..m., n = 1) %>% print()
}

tail(metafiles_Ta[[3]][[3]]$Geberhoehe.ueber.Grund..m., n = 1)

# save RH

write.csv(rh_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/rh.csv", row.names = FALSE)

save(rh_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/rh.RData")


##################################################################################################
# pressure
##################################################################################################
station_70km_pressure <- nearbyStations(52.4537, 13.3017,
                                         radius = 70,
                                         res = c("hourly"),
                                         per = c("historical"),
                                         var = c("pressure"),
                                         mindate=as.Date("2019-12-31"))

station_70km_pressure <- station_70km_pressure[-1,]

link_pressure <- selectDWD(station_70km_pressure$Stationsname,
                           outvec =  TRUE,
                           #remove_dupli = TRUE,   
                           res = c("hourly"), 
                           per = c("historical"),
                           var = c("pressure")) 
# download file:
pressure_name <- dataDWD(link_pressure, dir = tempdir(), read = FALSE) 

# read and plot file:
pressureData <- readDWD(pressure_name, varnames = FALSE, tz = "UTC") #, format = NULL
head(pressureData[[9]])

summary(pressureData[[9]])
table(diff(pressureData[[9]]$MESS_DATUM))

for (i in 1:9) {
  tibble("ID" = i, 
         "start" = head(pressureData[[i]]$MESS_DATUM, n=1), 
         "end" = tail(pressureData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}

metafiles_pressure <- readMeta(pressure_name)

for (i in 1:9) {
  tail(metafiles_pressure[[i]][[6]]$Stationshoehe..m., n = 1) %>% print()
}

for (i in 1:9) {
  tibble("ID" = i, 
         "start" = head(pressureData[[i]]$MESS_DATUM, n=1), 
         "end" = tail(pressureData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}

pressureData <- pressureData[-8]
station_70km_pressure <- station_70km_pressure[-8,]

station_70km_pressure$col <- "red"
station_70km_pressure$col[1] <- "blue"

leaflet() %>% addTiles() %>% 
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_pressure[-13], ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

pressure_Set19 <- NULL
pressure_19 <- data.frame(MESS_DATUM=ts)

for (i in 1:8) {
  pressure_Set19[[i]] <- dplyr::left_join(ts, pressureData[[i]][c(2,4,5)], by="MESS_DATUM")[2]
  pressure_19[i+1] <- as.numeric( zoo::na.fill(pressure_Set19[[i]][1], fill = "extend"))
  colnames(pressure_19)[i+1] <- paste0("ID_",pressureData[[i]][1,1])
}

colnames(pressure_19)[1] <- "timestamp"

summary(pressure_19)

head(pressure_19[2])
tail(pressure_19[2])
summary(pressure_19)
summary(filter(pressureData[[1]], year(MESS_DATUM)==2019)[,4])

unique(diff(filter(pressureData[[1]], year(MESS_DATUM)==2019)[,2]))
which(diff(filter(pressureData[[1]], year(MESS_DATUM)==2019)[,2])==2) # 6529 8186
filter(pressureData[[1]], year(MESS_DATUM)==2019)[6528:6530,2]

# save P

write.csv(pressure_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/pressure.csv", row.names = FALSE)

save(pressure_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/pressure.RData")

#############################################################################


#############################################################################
###### # wind
station_70km_Wind <- nearbyStations(52.4537, 13.3017, 
                                    radius = 70,
                                    res = c("hourly"),
                                    per = c("historical"),
                                    var = c("wind_synop"),
                                    mindate=as.Date("2019-12-31"))

station_70km_Wind <- station_70km_Wind[-1,]

# save station metadata

write.csv(station_70km_Wind, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_Wind.csv", row.names = FALSE)

save(station_70km_Wind, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_Wind.RData")

link_wind <- selectDWD(station_70km_Wind$Stationsname,
                       outvec =  TRUE,
                       #remove_dupli = TRUE,   
                       res = c("hourly"), 
                       per = c("historical"),
                       var = c("wind_synop")) 
# download file:
Wind_name <- dataDWD(link_wind, dir = tempdir(), read = FALSE) 

# read and plot file:
WindData <- readDWD(Wind_name, varnames = FALSE, tz = "UTC") #, format = NULL
head(WindData)

summary(WindData[[13]])
table(diff(WindData[[13]]$MESS_DATUM))

metafiles <- readMeta(Wind_name)

height_wind_ground <- NULL

for (i in 1:12) {
  tail(metafiles[[i]][[5]]$Geberhoehe.ueber.Grund..m., n=1) -> height_wind_ground[i]
}

tail(metafiles[[13]][[2]]$Geberhoehe.ueber.Grund..m., n=1) -> height_wind_ground[13]
tail(metafiles[[14]][[5]]$Geberhoehe.ueber.Grund..m., n=1) -> height_wind_ground[14]

# 
# for (i in 14:22) {
#   tail(metafiles[[i]][[5]]$Geberhoehe.ueber.Grund..m., n=1) -> height_wind_ground[i]
# }

station_70km_Wind$Geberhoehe <- height_wind_ground

for (i in 1:14) {
  tibble("ID" = i, 
         "start" = head(WindData[[i]]$MESS_DATUM, n=1), 
         "end" = tail(WindData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}


WindData <- WindData[-13]
station_70km_Wind <- station_70km_Wind[-13,]

leaflet() %>% addTiles() %>% 
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_Wind, ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

### creating an DWD data.frame
ts <- data.frame(MESS_DATUM=ts)

Wind_Set19 <- NULL
ws_19 <- data.frame(MESS_DATUM=ts)

for (i in 1:13) {
  Wind_Set19[[i]] <- left_join(ts, WindData[[i]][c(2,4,5)], by="MESS_DATUM")[2:3]
  ws_19[i+1] <- as.numeric(zoo::na.fill(Wind_Set19[[i]][1], fill = "extend"))
  colnames(ws_19)[i+1] <- paste0("ID_",WindData[[i]][1,1])
}

colnames(ws_19)[1] <- "timestamp"

summary(ws_19)

head(ws_19[2])
tail(ws_19[2])
summary(ws_19)
summary(filter(WindData[[1]], year(MESS_DATUM)==2019)[,4])

unique(diff(filter(WindData[[1]], year(MESS_DATUM)==2019)[,2]))
which(diff(filter(WindData[[1]], year(MESS_DATUM)==2019)[,2])==2) # 6529 8186
filter(WindData[[1]], year(MESS_DATUM)==2019)[6528:6530,2]

# save wind speed

write.csv(ws_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/ws.csv", row.names = FALSE)

save(ws_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/ws.RData")

# convertion to 10m height
summary(ws_19$ID_403)
Wind_height <- station_70km_Wind$Geberhoehe

ws_19_10m <- data.frame(timestamp=ts)

for (i in 2:14) {
  ws_19_10m[i] <- ndbc10m(ws_19[i], zm = Wind_height[i-1], 
                          zref = 10, inunits = "m/s", outunits = "m/s",
                          to.na = TRUE, missing = NA)
}

# winds10m() uses wind direction and roughness z to convert the height
colnames(ws_19_10m) <- colnames(ws_19)

summary(ws_19_10m)

#wind direction
wd_19 <- data.frame(MESS_DATUM=ts)

for (i in 1:13) {
  wd_19[i+1] <- as.numeric( zoo::na.fill(Wind_Set19[[i]][2], fill = "extend"))
  colnames(wd_19)[i+1] <- paste0("ID_",WindData[[i]][1,1])
}

colnames(wd_19)[1] <- "timestamp"

for (i in 1:13) {
  tibble("ID" = i, 
         "QN" = unique(filter(WindData[[i]], year(MESS_DATUM)==2019)[3])) %>%
    print()
}


summary(wd_19)

# save wind speed 10 m 

write.csv(ws_19_10m, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/ws_10m.csv", row.names = FALSE)

save(ws_19_10m, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/ws_10m.RData")

#############################################################################
#############################################################################
# sun
#############################################################################
#############################################################################
station_70km_sun <- nearbyStations(52.4537, 13.3017, 
                                    radius = 70,
                                    res = c("hourly"),
                                    per = c("historical"),
                                    var = c("sun"),
                                    mindate=as.Date("2019-12-31"))

station_70km_sun <- station_70km_sun[-1,]

write.csv(station_70km_sun, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_sun.csv", row.names = FALSE)

save(station_70km_sun, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/metadata/station_70km_sun.RData")

table(station_70km_sun$Stationshoehe)
# max 187 - min 12 - n = 21

link_sun <- selectDWD(station_70km_sun$Stationsname,
                      outvec =  TRUE,
                      #remove_dupli = TRUE,   
                      res = c("hourly"), 
                      per = c("historical"),
                      var = c("sun")) 
# download file:
sun_name <- dataDWD(link_sun, dir = tempdir(), read = FALSE) 

# read and plot file:
sunData <- readDWD(sun_name, varnames = FALSE, tz = "UTC") #, format = NULL
head(sunData)

summary(sunData[[13]])
table(diff(sunData[[13]]$MESS_DATUM))

metafiles_sun <- readMeta(sun_name)

height_sun_ground <- NULL

for (i in 1:13) {
  tail(metafiles_sun[[i]][[5]]$Geberhoehe.ueber.Grund..m., n=1) -> height_sun_ground[i]
}

station_70km_sun$Geberhoehe <- height_sun_ground

for (i in 1:13) {
  tibble("ID" = i, 
         "start" = head(sunData[[i]]$MESS_DATUM, n=1), 
         "end" = tail(sunData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}

leaflet() %>% addTiles() %>% 
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_sun, ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

sun_Set19 <- NULL
sun_19 <- data.frame(MESS_DATUM=ts)

summary(sunData[[1]])

for (i in 1:13) {
  sun_Set19[[i]] <- left_join(ts, sunData[[i]][c(2,4)], by="MESS_DATUM")[2]
  sun_19[i+1] <- sun_Set19[[i]][1]
  colnames(sun_19)[i+1] <- paste0("ID_", sunData[[i]][1,1])
}

colnames(sun_19)[1] <- "timestamp"

summary(sun_19)

head(sun_19[2])
tail(sun_19[2])
summary(sun_19[2])
summary(filter(sunData[[1]], year(MESS_DATUM)==2019)[,4])
unique(diff(filter(sunData[[1]], year(MESS_DATUM)==2019)[,2]))
which(diff(filter(sunData[[1]], year(MESS_DATUM)==2019)[,2])==7)

# convert night NA in zeros
#sun_19_C <- data.frame(MESS_DATUM=ts)

# replacement has 8760 rows, data has 17568
#for (i in 2:14) {
#  sun_19_C[[i]] <- c(c(0,0,0), na.approx(sun_19[[i]][1:17566]) ,c(0,0,0))
#}

##colnames(sun_19_C) <- colnames(sun_19)
#summary(sun_19_C)

#head(sun_19_C[2], 24)
#tail(sun_19_C[2], 24)
#summary(sun_19_C[2])

sun_19_C = sun_19
sun_19_C[is.na(sun_19_C)] <- 0

for (i in 1:13) {
  tibble("ID" = i, 
         "QN" = unique(filter(sunData[[i]], year(MESS_DATUM)==2019)[3])) %>%
    print()
}

# save sun

write.csv(sun_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/sun.csv", row.names = FALSE)

save(sun_19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/sun.RData")

write.csv(sun_19_C, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/sun_C.csv", row.names = FALSE)

save(sun_19_C, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/sun_C.RData")


# extraterrestrial radiation
Ra_sun19 <- data.frame(timestamp=ts)

for (i in 2:14) {
  Ra_sun19[i] <- MeTo::Ra(x = sun_19_C$timestamp, tl = 1, # 1 = hourly
                    lat.deg = station_70km_sun$geoBreite[i-1], 
                    long.deg = station_70km_sun$geoLaenge[i-1], control = list(Lz = 345))
}

tl <- as.numeric(difftime(sun_19_C$timestamp[2:length(sun_19_C$timestamp)], sun_19_C$timestamp[1:(length(sun_19_C$timestamp)-1)] , units = 'hour'))
tl_df = as.data.frame(tl)
tl <- unique(tl)

colnames(Ra_sun19) <- colnames(sun_19)
summary(Ra_sun19)

# radiation
Rs_sun19j <- data.frame(timestamp=ts)

for (i in 2:14) {
  Rs_sun19j[i] <- (0.25 + 0.5*sun_19_C[i]/60)*Ra_sun19[i]*100
}

Rs_sun19 <- data.frame(timestamp=ts)

# convert to w m-2
for (i in 2:14) {
  Rs_sun19[i] <- round((Rs_sun19j[i]*100*100)/(60*60),2)
}

colnames(Rs_sun19) <- colnames(sun_19)
summary(Rs_sun19)

summary(Rs_sun19$ID_3987)

# save solar radiation

write.csv(Rs_sun19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Rs_sun.csv", row.names = FALSE)

save(Rs_sun19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Rs_sun.RData")

write.csv(Rs_sun19j, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Rs_sunj.csv", row.names = FALSE)

save(Rs_sun19j, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Rs_sunj.RData")

write.csv(Ra_sun19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Ra_sun.csv", row.names = FALSE)

save(Ra_sun19, file = "D:/Stenka_UWI/Topic_3/03_RAW_DATA/20210830_DWDdata/Ra_sun.RData")
###########################

# ggplot() +
#   geom_jitter(aes(y = Rs_sun19$ID_3987, x = solar_19[[1]]$Rin))
# 
# cor.test(Rs_sun19$ID_3987, solar_19[[1]]$Rin)


#############################################################################
#############################################################################
# solar
#############################################################################
#############################################################################
station_70km_solar <- nearbyStations(52.4537, 13.3017, 
                                   radius = 70,
                                   res = c("hourly"),
                                   #per = c("historical"),
                                   var = c("solar"),
                                   mindate=as.Date("2019-12-31"))

station_70km_solar <- station_70km_solar[-1,]

table(station_70km_solar$Stationshoehe)
# max 187 - min 12 - n = 21

link_solar <- selectDWD(station_70km_solar$Stationsname,
                      outvec =  TRUE,
                      #remove_dupli = TRUE,   
                      res = c("hourly"), 
                      #per = c("historical"),
                      var = c("solar")) 
# download file:
solar_name <- dataDWD(link_solar, dir = tempdir(), read = FALSE) 

# read and plot file:
solarData <- readDWD(solar_name, varnames = FALSE, tz = "UTC") #, format = NULL
head(solarData)

summary(solarData[[1]])
table(diff(solarData[[1]]$MESS_DATUM))

for (i in 1:2) {
  tibble("ID" = i, 
         "start" = head(solarData[[i]]$MESS_DATUM, n=1), 
         "end" = tail(solarData[[i]]$MESS_DATUM, n=1)) %>%
    print()
}

leaflet() %>% addTiles() %>% 
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_solar, ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

summary(solarData[[1]])
#solar$MESS_DATUM
ymd_hm(solarData[[1]]$MESS_DATUM_WOZ)

solar_Data <- NULL

for (i in 1:2) {
  solarData[[i]] %>%
    filter(year(MESS_DATUM)>= 2018)  %>%
    mutate(FG_LBERG = round((FG_LBERG*100*100)/(60*60),2), 
           ATMO_LBERG = round((ATMO_LBERG*100*100)/(60*60),2),
           #MESS_DATUM_WOZ = with_tz(force_tz(ymd_hm(MESS_DATUM_WOZ),"CEST"), tz="UTC")
           MESS_DATUM_WOZ = ymd_hm(MESS_DATUM_WOZ)
    ) %>%
    na_if(-999) -> solar_Data[[i]]
  
  colnames(solar_Data[[i]]) <- c("id","MESS_DATUM", "QN_592" , "Rli",
                              "diffuse_radiation","Rin",
                              "sunshine_duration" ,"zenith_angle",
                              "timestamp","eor") 
}  

summary(solar_Data[[1]])
head(solar_Data[[1]]$MESS_DATUM)
tail(solar_Data[[1]]$MESS_DATUM)

solar_Data[[1]] %>%
    filter(year(timestamp)== 2019) -> solar_19

unique(diff(solar_19$timestamp))

######################################################
# soil moisture
######################################################
dbase <- "ftp://opendata.dwd.de/climate_environment/CDC/derived_germany"
soilIndex <- indexFTP(folder="soil/daily", base=dbase)
soilIndex <- createIndex(soilIndex, base=dbase)

#  "res" and "var" are inverted in the derived_germany folder!
colnames(soilIndex)[1:2] <- c("var", "res")
# non-standard column order, but rdwd should always use names (not positions)

head(soilIndex)

station_70km_soil <- nearbyStations(52.4537, 13.3017, 
                                     radius = 70,
                                     #res = "daily",
                                     var = "moisture",
                                     per = c("historical"),
                                     mindate=as.Date("2019-12-31"))

station_70km_soil <- station_70km_soil[-1,]

# select URL:
soil_link <- selectDWD(unique(station_70km_soil$Stationsname), 
                       res = "daily", var = "soil", per = "historical",
                       outvec =  TRUE,
                       base = dbase, findex = soilIndex)

unique(soil_link)

soil_name <- dataDWD(unique(soil_link)[-5], base = dbase, dir = tempdir(), read = FALSE) 
metafiles_soil <- readMeta(soil_name)

# download and read files:
soil_data <- dataDWD(unique(soil_link)[-5], base = dbase)

summary(soil_data[[13]])
table(diff(soil_data[[13]]$Datum))

soil_data[[1]]$Stationsindex

soil_station_id <- NULL

for (i in 1:17) {
  unlist(head(soil_data[[i]]$Stationsindex, n = 1)) %>%
    print() -> soil_station_id[i]
}

station_70km_soil <- station_70km_soil[!duplicated(station_70km_soil$Stations_id), ]
station_70km_soil$Stations_id
soil_station_id

station_70km_soil <- station_70km_soil[-5,]
station_70km_soil <- station_70km_soil[-18,]

station_70km_soil$Stations_id == soil_station_id

for (i in 1:17) {
  tibble("ID" = i,
         "station" = head(soil_data[[i]]$Stationsindex, n = 1), 
         "start" = head(soil_data[[i]]$Datum, n = 1), 
         "end" = tail(soil_data[[i]]$Datum, n = 1)) %>%
    print()
}

soil_data <- soil_data[-16]
station_70km_soil <- station_70km_soil[-16,]

SMC_19 <- NULL

for (i in 1:17) {
SMC_19[[i]] <- data.frame(Datum = filter(soil_data[[i]], year(Datum) == 2019)$Datum,
                         BF10 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF10),
                         BF20 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF20),
                         BF30 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF30),
                         BF40 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF40),
                         BF50 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF50),
                         BF60 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BF60),
                         BFGLS60 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BFGLS),
                         BFGSL60 = na.approx(filter(soil_data[[i]], year(Datum) == 2019)$BFGSL)
                         )
}

names(SMC_19) <- paste0("ID_", soil_station_id)

summary(SMC_19[[1]])

SMC_19[[1]] %>%
  filter(year(Datum)==2019) %>%
  ggplot() +
  #geom_line(aes(y = BF10, x = Datum), color = "yellow") + # 10cm
  geom_line(aes(y = BF20, x = Datum), color = "gold") +   # 20cm
  #geom_line(aes(y = BF30, x = Datum), color = "orange") + # 30cm
  geom_line(aes(y = BF40, x = Datum), color = "pink") +   # 40cm
  geom_line(aes(y = BF50, x = Datum), color = "red") +    # 50cm
  #geom_line(aes(y = BF60, x = Datum), color = "darkred") + # 60cm
  geom_line(aes(y = BFGSL60, x = Datum), color = "blue") +   # 60cm
  #geom_line(aes(y = BFGLS60, x = Datum), color = "black") +   # 60cm
  scale_y_continuous(limits = c(0,110), breaks = seq(0,100,20)) +
  geom_hline(yintercept = 100, linetype = "dotted")


SMC_20cm_19 <- data.frame(Datum=SMC_19[[1]]$Datum)
SMC_40cm_19 <- data.frame(Datum=SMC_19[[1]]$Datum)
SMC_60cm_19 <- data.frame(Datum=SMC_19[[1]]$Datum)

for (i in 1:17) {
  SMC_20cm_19[i+1] <- na.approx(SMC_19[[i]]$BF20)
  SMC_40cm_19[i+1] <- na.approx(SMC_19[[i]]$BF40)
  SMC_60cm_19[i+1] <- na.approx(SMC_19[[i]]$BFGSL60)
}

names(SMC_20cm_19) <- c("timestamp", paste0("ID_", soil_station_id))
summary(SMC_20cm_19)

names(SMC_40cm_19) <- c("timestamp", paste0("ID_", soil_station_id))
summary(SMC_40cm_19)

names(SMC_60cm_19) <- c("timestamp", paste0("ID_", soil_station_id))
summary(SMC_60cm_19)


ggplot(soil_data[[13]]) +
  geom_line(aes(y = TS05, x = Datum))

soil_data[[13]] %>%
  filter(year(Datum)>=2019) %>%
  ggplot() +
  geom_line(aes(y = VGSL, x = Datum), color = "red") + #real evapotranspiration over grass and sandy loam (AMBAV)
  geom_line(aes(y = VPGB, x = Datum), color = "blue") + #potention evapotranspiration over grass (AMBAV)
  geom_line(aes(y = VPGH, x = Datum), color = "black") #potention evapotranspiration over grass (Haude)

soil_data[[13]] %>%
  filter(year(Datum)>=2019) %>%
  ggplot() +
  geom_line(aes(y = VGLS, x = Datum), color = "red") + #real evapotranspiration over grass and sandy loam (AMBAV)
  geom_line(aes(y = VPGPM, x = Datum), color = "blue") #potential evaotranspiration over grass (Penman Monteith, FAO formula)

# soil moisture
# BF - soil moisture under grass and sandy loam between 0 and 10 cm depth in % plant useable water
# soil moisture under grass and sandy loam up to 60 cm depth (AMBAV) BFGSL %nFK
# soil moisture under grass and loamy sand up to 60 cm depth (AMBAV) BFGLS %nFK
soil_data[[1]] %>%
  filter(year(Datum)==2019) %>%
  ggplot() +
  geom_line(aes(y = BF10, x = Datum), color = "yellow") + # 10cm
  geom_line(aes(y = BF20, x = Datum), color = "gold") +   # 20cm
  geom_line(aes(y = BF30, x = Datum), color = "orange") + # 30cm
  geom_line(aes(y = BF40, x = Datum), color = "pink") +   # 40cm
  geom_line(aes(y = BF50, x = Datum), color = "red") +    # 50cm
  geom_line(aes(y = BF60, x = Datum), color = "darkred") + # 60cm
  geom_line(aes(y = BFGSL, x = Datum), color = "blue") +   # 60cm
  geom_line(aes(y = BFGLS, x = Datum), color = "black")    # 60cm




######################################################
# atm moisture
######################################################
station_70km_moisture <- nearbyStations(52.4537, 13.3017, 
                                         radius = 70,
                                         res = c("hourly"),
                                         per = c("historical"),
                                         var = c("moisture"),
                                         mindate=as.Date("2019-12-31"))

station_70km_moisture <- station_70km_moisture[-1,]

table(station_70km_moisture$Stationshoehe)
# max 187 - min 12 - n = 31

link_moisture <- selectDWD(station_70km_moisture$Stationsname,
                           outvec =  TRUE,
                           #remove_dupli = TRUE,   
                           res = c("hourly"), 
                           per = c("historical"),
                           var = c("moisture")) 
# download file:
moisture_name <- dataDWD(link_moisture, dir = tempdir(), read = FALSE) 

# read and plot file:
moistureData <- readDWD(moisture_name, varnames = FALSE, tz = "UTC") #, format = NULL
head(moistureData)

summary(moistureData[[16]])
table(diff(moistureData[[17]]$MESS_DATUM))

metafiles_moisture <- readMeta(moisture_name)

for (i in 1:17) {
  tibble("ID" = i,
         "station" = head(moistureData[[i]]$STATIONS_ID, n = 1), 
         "start" = head(moistureData[[i]]$MESS_DATUM, n = 1), 
         "end" = tail(moistureData[[i]]$MESS_DATUM, n = 1)) %>%
    print()
}

moistureData <- moistureData[-16]
station_70km_moisture <- station_70km_moisture[-16,]


leaflet() %>% addTiles() %>% 
  addPolygons(data = berlin.Border, fillColor = "green", fillOpacity = 0.2, color = "black", weight = 1) %>%
  addCircles(lng=13.3017, lat=52.4537, radius = 70000, col = "black") %>%
  addCircleMarkers(data = station_70km_moisture[-16], ~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)

# P_STD	  Hourly air pressure	[hpa]
# RF_STD	Hourly values of relative humidity	[%]
# TD_STD	Dew point temperature at 2m height	[°C]
# TF_STD	Calculated hourly values of the wet temperature	[°C]
# TT_STD	Air temperature at 2m height	[°C]
# VP_STD	calculated hourly values of the vapour pressure [hpa]

moisture_Set19 <- NULL
VP_19 <- data.frame(MESS_DATUM=ts)

summary(moistureData[[1]])
summary(moistureData[[1]])

for (i in 1:17) {
  moisture_Set19[[i]] <- left_join(ts, moistureData[[i]][c(2,5)], by="MESS_DATUM")[2]
  VP_19[i+1] <- na.approx(moisture_Set19[[i]][1])
  colnames(VP_19)[i+1] <- paste0("ID_", moistureData[[i]][1,1])
}

colnames(VP_19)[1] <- "timestamp"

summary(VP_19)
VP_19 <- VP_19[-9]
station_70km_moisture <- station_70km_moisture[-8,]
summary(VP_19)

