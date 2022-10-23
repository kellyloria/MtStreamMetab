# Glenbrook stream DO agg

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
library(scales)

#=========================================== 
# Get and process high frequency sensor data
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Glenbrook\ Lower/DOdat/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved.Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% #, format ="%Y-%m-%d %H:%M:%S"))%>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

do.ts <- subset(do.ts,
                 datetime > "2021-03-15 00:00:00",
                Q>0.7) # sensor quality minimum threshold

## Look through timestamp patterns for do.ts
# do.ts1 <- subset(do.ts, 
#                  datetime > "2021-03-15 00:00:00" & datetime < '2022-10-20 14:00:00',
#                 Q>0.7) # sensor quality minimum threshold
# do.ts1$datetime <- do.ts1$datetime - c(60*60*(12))
# do.ts2 <- subset(do.ts, 
#                  datetime > "2021-10-20 14:00:00",
#                  Q>0.7) # sensor quality minimum threshold
# do.ts2$datetime <- do.ts2$datetime - c(60*60*(12))
# 
# do.ts <- rbind(do.ts1, do.ts2)

tempcheck <- subset(do.ts,
                    datetime > '2022-09-16 00:00:00' & datetime < '2022-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

# tempcheck$datetime <- tempcheck$datetime - c(60*60*(6)) #? maybe 6

wtr.ts <- rawdat %>%
dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

wtr.ts <- subset(wtr.ts,
                datetime > "2021-03-15 00:00:00") 

## Look through timestamp patterns for wtr.ts
# wtr.ts1 <- subset(wtr.ts, 
#                  datetime > "2021-03-15 00:00:00" & datetime < '2022-10-20 14:00:00')
# wtr.ts1$datetime <- wtr.ts1$datetime - c(60*60*(12))
# 
# wtr.ts2 <- subset(wtr.ts, 
#                  datetime > "2021-10-20 14:00:00") 
# wtr.ts2$datetime <- wtr.ts2$datetime - c(60*60*(12))
# 
# wtr.ts <- rbind(wtr.ts1,wtr.ts2)

tempcheck2 <- subset(wtr.ts, 
                    datetime > '2022-09-16 00:00:00' & datetime < '2022-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))


###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 8,sd.dev = 3) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)


###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 8, sd.dev = 6) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

# add in climate:
# *st F9917 installed 2021-06-01

climate <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/F9917.2022-10-21.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                par="solar_radiation_set_1",
                wspeed='wind_speed_set_1',
                baro="pressure_set_1d")%>%
  #mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  select(datetime,
         baro,
         par,
         wspeed,
         hour,
         yday) # datetime in UTC

climate$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(climate$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

ggplot(data = ,aes(x=datetime,y=baro)) + geom_line()
describe.ts(climate)
baro.ts.clean <- trim.outliers(climate,width = 50,sd.dev = 7) 
ggplot(data = climate,aes(x=datetime,y=baro)) + geom_line() + 
  geom_line(data=baro.ts.clean,aes(x=datetime,y=baro),col="red")
baro.ts.avg <- aggregate.data(data = baro.ts.clean,time.step = 15)


clim2 <-na.omit(climate) 
clim2<- climate %>%
  select(datetime,
         par)

ggplot(data = ,aes(x=datetime,y=par)) + geom_line()
describe.ts(clim2)
par.ts.clean <- trim.outliers(clim2,width = 50,sd.dev = 7) 
ggplot(data = clim2,aes(x=datetime,y=par)) + geom_line() + 
  geom_line(data=par.ts.clean,aes(x=datetime,y=par),col="red")
par.ts.avg <- aggregate.data(data = par.ts.clean,time.step = 15)


# Merge by smaller time interval...
datx <- do.ts.avg %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.avg) %>% 
  full_join(par.ts.avg) %>%
  full_join(baro.ts.avg)

# highlight some 24 hour checks to see what shape the data is in... 
tempcheck <- subset(datx, 
                datetime > '2021-09-10 00:00:00' & datetime < '2021-09-12 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
  scale_x_datetime(breaks = date_breaks("4 hours"))

summary(datx)


## Add in flow data

library(dataRetrieval)

siteNo <- "10336730"
pCode <- c("00060", "00065")
start.date <- "2021-03-14"
end.date <- "2022-10-05"

GBflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

flow.ts <- GBflow %>% select("dateTime", "X_00060_00000", "X_00065_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000", gageHF= "X_00065_00000") %>%
  select("datetime", "dischargeCFS", "gageHF")

dat <- datx %>% #note in this case I am not using the drift correction -KL
  full_join(flow.ts)

dat<- dat[!is.na(dat$do.obs), ]
dat1<- dat[!is.na(dat$wtr), ]

summary(dat1)
dat1 <- subset(dat1, datetime > '2021-06-01 11:30:00') # sensor quality minimum threshold

qplot(datetime, baro , data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))


dat1$baroM <- rollapply(dat1$baro, width=40,
                     FUN=function(x) mean(x, na.rm=TRUE), by=1,
                     by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat1$baro <- as.numeric(ifelse(is.na(dat1$baro), 
                                       (dat1$baroM),
                                       (dat1$baro)))
summary(dat1)


dat1$PARM <- rollapply(dat1$par, width=40,
                        FUN=function(x) mean(x, na.rm=TRUE), by=1,
                        by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat1$par <- as.numeric(ifelse(is.na(dat1$par), 
                               (dat1$PARM),
                               (dat1$par)))
summary(dat1)


dat1$dischargeM <- rollapply(dat1$dischargeCFS, width=40,
                       FUN=function(x) mean(x, na.rm=TRUE), by=1,
                       by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat1$dischargeCFS <- as.numeric(ifelse(is.na(dat1$dischargeCFS), 
                              (dat1$dischargeM),
                              (dat1$dischargeCFS)))
summary(dat1)



dat1$gageM <- rollapply(dat1$gageHF, width=10,
                             FUN=function(x) mean(x, na.rm=TRUE), by=1,
                             by.column=TRUE, partial=TRUE, fill=NA, align="center")

dat1$gageHF <- as.numeric(ifelse(is.na(dat1$gageHF), 
                                       (dat1$gageM),
                                       (dat1$gageHF)))
summary(dat1)

#  write.csv(x = dat1, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBLInputs.csv", row.names = TRUE)
