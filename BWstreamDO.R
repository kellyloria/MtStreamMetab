# Blackwood stream DO

#=========== Preliminaries
rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
library(nrlmetab)
library(zoo)
library(suncalc)
library(readxl)
library(patchwork)
library(gridExtra)
library(dplyr)
library(reshape)
library(lubridate)

#=========================================== 
# Get and process high frequency sensor data
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/Dodat/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() 

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved.Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

do.ts <- subset(do.ts, 
                datetime > "2021-04-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold
#do.ts$datetime <- do.ts$datetime - hours(4)
range(do.ts$datetime)

tempcheck <- subset(do.ts, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))
# do.ts$datetime <- do.ts$datetime - c(60*60*(9))


###
wtr.ts <- rawdat %>% select("UTC_Date_&_Time", "Temperature") %>% 
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

wtr.ts <- subset(wtr.ts, 
                 datetime > "2021-04-30 00:00:00") # sensor quality minimum threshold


## Check the dates:
tempcheck <- subset(wtr.ts, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

# DO data might be 7 hours off so need a UTC correction...
#tempcheck$datetime <- tempcheck$datetime - c(60*60*(9))
# wtr.ts$datetime <- wtr.ts$datetime - c(60*60*(9))


###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 8,sd.dev = 6) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)


######
###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 12, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

# add in climate:
climate <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/D9413.2022-10-14.csv", skip=) %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro='pressure_set_1d')%>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         baro,
         wspeed,
         hour,
         yday) # datetime in UTC

climate$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(climate$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

#climate$datetime <- climate$datetime - c(60*60*9)
## Check the dates:
tempcheck <- subset(climate, 
                    datetime > '2021-08-15 00:00:00' & datetime < '2021-08-16 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))
# DO data might be 7 hours off so need a UTC correction...
#  tempcheck$datetime <- tempcheck$datetime + hours(6)
#  climate$datetime <- climate$datetime + hours(6)

climate <- climate %>%
  select(datetime,
         baro) #

ggplot(data = ,aes(x=datetime,y=climate)) + geom_line()
describe.ts(climate)
baro.ts.clean <- trim.outliers(climate,width = 10,sd.dev = 50) 
ggplot(data = climate,aes(x=datetime,y=baro)) + geom_line() + 
  geom_line(data=baro.ts.clean,aes(x=datetime,y=baro),col="red")
baro.ts.avg <- aggregate.data(data = baro.ts.clean,time.step = 15)

###


clim2 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/HMDC1.2022-10-14.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                par='solar_radiation_set_1',
                wspeed='wind_speed_set_1')%>%
 # mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         par,
         wspeed,
         hour,
         yday) # datetime in UTC

#clim2$datetime <- clim2$datetime - c(60*60*8)
## Check the dates:
tempcheck <- subset(clim2, 
                    datetime > '2021-07-10 00:00:00' & datetime < '2021-07-12 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))
# DO data might be 7 hours off so need a UTC correction...

#clim2$datetime <- clim2$datetime - hours(9)

clim2 <- clim2 %>%
  select(datetime,
         par) #


ggplot(data = ,aes(x=datetime,y=par)) + geom_line()
describe.ts(clim2)
par.ts.clean <- trim.outliers(clim2,width = 10,sd.dev = 50) 
ggplot(data = clim2,aes(x=datetime,y=par)) + geom_line() + 
  geom_line(data=par.ts.clean,aes(x=datetime,y=par),col="red")
par.ts.avg <- aggregate.data(data = par.ts.clean,time.step = 15)


# 
dat <- do.ts.avg %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.avg) %>% 
  full_join(par.ts.avg) %>%
  full_join(baro.ts.avg) 

summary(dat)

## Add in flow
library(dataRetrieval)

siteNo <- "10336660"
pCode <- c("00060", "00065")
start.date <- "2021-04-29"
end.date <- "2022-10-15"

BWflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)


flow.ts <- BWflow %>% select("dateTime", "X_00060_00000", "X_00065_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000", gageHF= "X_00065_00000") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "dischargeCFS", "gageHF")


dat <- dat %>%
  full_join(flow.ts) 


dat1<- dat[!is.na(dat$do.obs), ]
dat2<- dat1[!is.na(dat1$wtr), ]

summary(dat2)

# infill some missing data:
dat2$baroM <- rollapply(dat2$baro, width=50,
                        FUN=function(x) mean(x, na.rm=TRUE), by=1,
                        by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat2$baro <- as.numeric(ifelse(is.na(dat2$baro), 
                               (dat2$baroM),
                               (dat2$baro)))
summary(dat2)



dat2$PARM <- rollapply(dat2$par, width=40,
                       FUN=function(x) mean(x, na.rm=TRUE), by=1,
                       by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat2$par <- as.numeric(ifelse(is.na(dat2$par), 
                              (dat2$PARM),
                              (dat2$par)))
summary(dat2)


dat2$dischargeM <- rollapply(dat2$dischargeCFS, width=40,
                             FUN=function(x) mean(x, na.rm=TRUE), by=1,
                             by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat2$dischargeCFS <- as.numeric(ifelse(is.na(dat2$dischargeCFS), 
                                       (dat2$dischargeM),
                                       (dat2$dischargeCFS)))
summary(dat2)



dat2$gageM <- rollapply(dat2$gageHF, width=10,
                        FUN=function(x) mean(x, na.rm=TRUE), by=1,
                        by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat2$gageHF <- as.numeric(ifelse(is.na(dat2$gageHF), 
                                 (dat2$gageM),
                                 (dat2$gageHF)))
summary(dat2)



#dat1 <- na.omit(dat, dat$do.obs)

qplot(datetime,  do.obs  , data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# write.csv(x = dat2, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWLInputs.csv", row.names = TRUE)

##
###
#=========================
# UPPER site?
#=========================
###
##

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/DodatL/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() 


do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

do.ts <- subset(do.ts, 
                datetime > "2021-04-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold
#do.ts$datetime <- do.ts$datetime - hours(4)
range(do.ts$datetime)

tempcheck <- subset(do.ts, 
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))
# do.ts$datetime <- do.ts$datetime - c(60*60*(5))

####
#### break for 2021
# 2021-06-29 to 2021-09-20
do.ts21 <- subset(do.ts, 
                  datetime > '2021-07-01 00:00:00' & datetime < '2021-09-12 00:00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = do.ts21, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("72 hours"))

####
#### break for 2022
#### break for 2022-07-08 to 2022-10-12
do.ts22 <- subset(do.ts, 
                  datetime > '2022-07-09 14:00:00' & datetime < '2022-10-12 14:00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = do.ts22, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("72 hours"))



###
wtr.ts <- rawdat %>% select("UTC_Date_&_Time", "Temperature") %>% 
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

## Check the dates:
tempcheck <- subset(wtr.ts, 
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

# DO data might be 7 hours off so need a UTC correction...
#  tempcheck$datetime <- tempcheck$datetime - c(60*60*(6))
#  wtr.ts$datetime <- wtr.ts$datetime - c(60*60*(6))

wtr.ts21 <- subset(wtr.ts, 
                  datetime > '2021-07-01 00:00:00' & datetime < '2021-09-12 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = wtr.ts21, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("72 hours"))
# do.ts$datetime <- do.ts$datetime - c(60*60*(5))


####
#### break for 2022
#### break for 2022-07-08 to 2022-10-12
wtr.ts22 <- subset(wtr.ts, 
                  datetime > '2022-07-09 14:00:00' & datetime < '2022-10-12 14:00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = wtr.ts22, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("72 hours"))


###clean DO data
ggplot(data = do.ts21,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts21)
do.ts.clean21 <- trim.outliers(do.ts21,width = 8,sd.dev = 6) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts21,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean21,aes(x=datetime,y=do.obs),col="red")
do.ts.avg21 <- aggregate.data(data = do.ts.clean21,time.step = 15)

ggplot(data = do.ts22,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts22)
do.ts.clean22 <- trim.outliers(do.ts22,width = 8,sd.dev = 6) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts22,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean22,aes(x=datetime,y=do.obs),col="red")
do.ts.avg22 <- aggregate.data(data = do.ts.clean22,time.step = 15)



######
###clean wtr data
ggplot(data = wtr.ts21,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts21)
wtr.ts.clean21 <- trim.outliers(wtr.ts21,width = 12, sd.dev = 3) 
ggplot(data = wtr.ts21,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean21,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg21 <- aggregate.data(data = wtr.ts.clean21,time.step = 15)


ggplot(data = wtr.ts22,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts22)
wtr.ts.clean22 <- trim.outliers(wtr.ts22,width = 12, sd.dev = 3) 
ggplot(data = wtr.ts22,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean22,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg22 <- aggregate.data(data = wtr.ts.clean22,time.step = 15)


# add in climate:
climate <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/D9413.2022-10-14.csv", skip=) %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro='pressure_set_1d')%>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         baro,
         wspeed,
         hour,
         yday) # datetime in UTC

climate$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(climate$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

#climate$datetime <- climate$datetime - c(60*60*9)
## Check the dates:
tempcheck <- subset(climate, 
                    datetime > '2021-08-15 00:00:00' & datetime < '2021-08-16 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))
# DO data might be 7 hours off so need a UTC correction...
#  tempcheck$datetime <- tempcheck$datetime + hours(6)
#  climate$datetime <- climate$datetime + hours(6)

climate <- climate %>%
  select(datetime,
         baro) #

ggplot(data = ,aes(x=datetime,y=climate)) + geom_line()
describe.ts(climate)
baro.ts.clean <- trim.outliers(climate,width = 10,sd.dev = 50) 
ggplot(data = climate,aes(x=datetime,y=baro)) + geom_line() + 
  geom_line(data=baro.ts.clean,aes(x=datetime,y=baro),col="red")
baro.ts.avg <- aggregate.data(data = baro.ts.clean,time.step = 15)

###


clim2 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/HMDC1.2022-10-14.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                par='solar_radiation_set_1',
                wspeed='wind_speed_set_1')%>%
  # mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         Atemp,
         par,
         wspeed,
         hour,
         yday) # datetime in UTC

#clim2$datetime <- clim2$datetime - c(60*60*8)
## Check the dates:
tempcheck <- subset(clim2, 
                    datetime > '2021-07-10 00:00:00' & datetime < '2021-07-12 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))
# DO data might be 7 hours off so need a UTC correction...

#clim2$datetime <- clim2$datetime - hours(9)

clim2 <- clim2 %>%
  select(datetime,
         par) #


ggplot(data = ,aes(x=datetime,y=par)) + geom_line()
describe.ts(clim2)
par.ts.clean <- trim.outliers(clim2,width = 10,sd.dev = 50) 
ggplot(data = clim2,aes(x=datetime,y=par)) + geom_line() + 
  geom_line(data=par.ts.clean,aes(x=datetime,y=par),col="red")
par.ts.avg <- aggregate.data(data = par.ts.clean,time.step = 15)


## Add in flow
library(dataRetrieval)

siteNo <- "10336660"
pCode <- c("00060", "00065")
start.date <- "2021-04-29"
end.date <- "2022-10-15"

BWflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)


flow.ts <- BWflow %>% select("dateTime", "X_00060_00000", "X_00065_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000", gageHF= "X_00065_00000") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "dischargeCFS", "gageHF")

### 2021 ###
dat21 <- do.ts.avg21 %>%
  full_join(wtr.ts.avg21) %>%
  full_join(par.ts.avg) %>%
  full_join(baro.ts.avg) %>%
  full_join(flow.ts) 

### 2022 ###
dat22 <- do.ts.avg22 %>%
  full_join(wtr.ts.avg22) %>%
  full_join(par.ts.avg) %>%
  full_join(baro.ts.avg) %>%
  full_join(flow.ts) 



dat21_1<- dat21[!is.na(dat21$do.obs), ]
dat21_2<- dat21_1[!is.na(dat21_1$wtr), ]
summary(dat21_2)

# infill some missing data:
dat21_2$baroM <- rollapply(dat21_2$baro, width=50,
                        FUN=function(x) mean(x, na.rm=TRUE), by=1,
                        by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat21_2$baro <- as.numeric(ifelse(is.na(dat21_2$baro), 
                               (dat21_2$baroM),
                               (dat21_2$baro)))
summary(dat21_2)



dat21_2$PARM <- rollapply(dat21_2$par, width=40,
                       FUN=function(x) mean(x, na.rm=TRUE), by=1,
                       by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat21_2$par <- as.numeric(ifelse(is.na(dat21_2$par), 
                              (dat21_2$PARM),
                              (dat21_2$par)))
summary(dat21_2)


dat21_2$dischargeM <- rollapply(dat21_2$dischargeCFS, width=40,
                             FUN=function(x) mean(x, na.rm=TRUE), by=1,
                             by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat21_2$dischargeCFS <- as.numeric(ifelse(is.na(dat21_2$dischargeCFS), 
                                       (dat21_2$dischargeM),
                                       (dat21_2$dischargeCFS)))
summary(dat21_2)



dat21_2$gageM <- rollapply(dat21_2$gageHF, width=10,
                        FUN=function(x) mean(x, na.rm=TRUE), by=1,
                        by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat21_2$gageHF <- as.numeric(ifelse(is.na(dat21_2$gageHF), 
                                 (dat21_2$gageM),
                                 (dat21_2$gageHF)))
summary(dat21_2)



#dat1 <- na.omit(dat, dat$do.obs)

qplot(datetime,  do.obs  , data = dat21_2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# write.csv(x = dat21_2, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/21_BWUInputs.csv", row.names = TRUE)


##

dat22_1<- dat22[!is.na(dat22$do.obs), ]
dat22_2<- dat22_1[!is.na(dat22_1$wtr), ]
summary(dat22_2)

# infill some missing data:
dat22_2$baroM <- rollapply(dat22_2$baro, width=50,
                           FUN=function(x) mean(x, na.rm=TRUE), by=1,
                           by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat22_2$baro <- as.numeric(ifelse(is.na(dat22_2$baro), 
                                  (dat22_2$baroM),
                                  (dat22_2$baro)))
summary(dat22_2)



dat22_2$PARM <- rollapply(dat22_2$par, width=40,
                          FUN=function(x) mean(x, na.rm=TRUE), by=1,
                          by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat22_2$par <- as.numeric(ifelse(is.na(dat22_2$par), 
                                 (dat22_2$PARM),
                                 (dat22_2$par)))
summary(dat22_2)

# 
# dat21_2$dischargeM <- rollapply(dat21_2$dischargeCFS, width=40,
#                                 FUN=function(x) mean(x, na.rm=TRUE), by=1,
#                                 by.column=TRUE, partial=TRUE, fill=NA, align="center")
# 
# dat21_2$dischargeCFS <- as.numeric(ifelse(is.na(dat21_2$dischargeCFS), 
#                                           (dat21_2$dischargeM),
#                                           (dat21_2$dischargeCFS)))
# summary(dat21_2)
# 
# 
# 
# dat21_2$gageM <- rollapply(dat21_2$gageHF, width=10,
#                            FUN=function(x) mean(x, na.rm=TRUE), by=1,
#                            by.column=TRUE, partial=TRUE, fill=NA, align="center")
# 
# dat21_2$gageHF <- as.numeric(ifelse(is.na(dat21_2$gageHF), 
#                                     (dat21_2$gageM),
#                                     (dat21_2$gageHF)))
# summary(dat21_2)



#dat1 <- na.omit(dat, dat$do.obs)

qplot(datetime,  do.obs  , data = dat22_2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# write.csv(x = dat22_2, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWUInputs.csv", row.names = TRUE)





