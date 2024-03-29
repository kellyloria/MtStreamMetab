# Glenbrook stream DO agg

#=========== Preliminaries
rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
library(nrlmetab)
#remotes::install_github("nrlottig/nrlmetab", force = TRUE)
library(zoo)
library(suncalc)
library(readxl)
library(patchwork)
library(gridExtra)
library(dplyr)
library(reshape)
library(lubridate)
library(ggplot2)
library(scales)
#=========================================== 
# Get and process high frequency sensor data
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Glenbrook\ Lower/DOdat/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved.Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

do.ts <- subset(do.ts, 
                datetime > "2021-04-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold
#do.ts$datetime <- do.ts$datetime - hours(4)
range(do.ts$datetime)

tempcheck <- subset(do.ts, 
                    datetime > '2023-05-16 00:00:00' & datetime < '2023-05-17 :00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2023-05-16 00:00:00", 
                                       "2023-05-16 12:00:00", 
                                       "2023-05-16 24:00:00")), color = "yellow")

# do.ts$datetime <- do.ts$datetime - c(60*60*(9))


###
wtr.ts <- rawdat %>% 
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

wtr.ts <- subset(wtr.ts, 
                 datetime > "2021-04-30 00:00:00") # sensor quality minimum threshold


## Check the dates:
tempcheck <- subset(wtr.ts, 
                    datetime > '2023-05-16 00:00:00' & datetime < '2023-05-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2023-05-16 00:00:00", 
                                       "2023-05-16 12:00:00", 
                                       "2023-05-16 24:00:00")), color = "yellow")


# DO data might be 7 hours off so need a UTC correction...
#tempcheck$datetime <- tempcheck$datetime - c(60*60*(9))
# wtr.ts$datetime <- wtr.ts$datetime - c(60*60*(9))

do.ts <- do.ts %>%
  dplyr::select("datetime", "do.obs")

###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts, width = 7,sd.dev = 3)  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)

######
###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts, width = 7, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

# add in climate:
# *st F9917 installed 2021-06-01
climate <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/F9917.2023-09-01.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                par="solar_radiation_set_1",
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro="pressure_set_1d")%>%
  #mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
         baro,
         Atemp,
         par,
         wspeed,
         hour,
         yday) # datetime in UTC

climate$datetime <- (as.POSIXct(round_date(
  as.POSIXct(climate$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

tempcheck <- subset(climate, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")



### FROM BW #####
################

# add in climate:
clim_infil1 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/D9413.2023-09-01.csv", skip=) %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro='pressure_set_1d')%>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
                Atemp,
                baro,
                wspeed,
                hour,
                yday) # datetime in UTC

clim_infil1$datetime <- (as.POSIXct(round_date(
  as.POSIXct(clim_infil1$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

tempcheck <- subset(clim_infil1, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")


###
clim_infil2 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/HMDC1.2023-09-01.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                par='solar_radiation_set_1',
                wspeed='wind_speed_set_1')%>%
  # mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
                Atemp,
                par,
                wspeed,
                hour,
                yday) # datetime in UTC

clim_infil2$datetime <- (as.POSIXct(round_date(
  as.POSIXct(clim_infil2$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))



## Check the dates:
tempcheck <- subset(clim_infil2, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")
# 
dat <- do.ts.clean%>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.clean) %>% 
  full_join(climate) 

summary(dat)

## Add in flow data

library(dataRetrieval)

siteNo <- "10336730"
pCode <- c("00060", "00065")
start.date <- "2021-03-14"
end.date <- "2023-09-01"

GBflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

flow.ts <- GBflow %>%
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000", gageHF= "X_00065_00000") %>%
  dplyr::select("datetime", "dischargeCFS", "gageHF")

datv1 <- dat %>%
  full_join(flow.ts) 

dat_unique <- datv1 %>%
  group_by(datetime) %>%
  summarize(do.obs = mean(do.obs, na.rm=T),
            wtr = mean(wtr, na.rm=T),
            par = mean(par, na.rm=T),
            baro = mean(baro, na.rm=T),
            dischargeCFS = mean(dischargeCFS, na.rm=T),
            gageHF = mean(gageHF, na.rm=T))

dat1<- dat_unique[!is.na(dat_unique$do.obs), ]
dat2<- dat1[!is.na(dat1$wtr), ]

summary(dat2)

# Calculate the rolling average and store it in 'gageM'
dat2$gageM <- rollapply(dat2$gageHF, width = 5,
                        FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                        by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$gageHF[i])) {
    dat2$gageHF[i] <- dat2$gageM[i]
  }
}
# Summary of the updated 'dat21_2' dataset
summary(dat2)

qplot(datetime, gageHF, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# Calculate the rolling average 
dat2$dischargeM <- rollapply(dat2$dischargeCFS, width = 10,
                             FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                             by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$dischargeCFS[i])) {
    dat2$dischargeCFS[i] <- dat2$dischargeM[i]
  }
}
# Summary of the updated 'dat21_2' dataset
summary(dat2)

qplot(datetime, dischargeCFS, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

datsave<- dat2
# Calculate the rolling average and store it in 'gageM'
dat2$baroM <- rollapply(dat2$baro, width = 5,
                        FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                        by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$baro[i])) {
    dat2$baro[i] <- dat2$baroM[i]
  }
}
summary(dat2)


qplot(datetime, baro, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# Summary of the updated 'dat21_2' dataset
summary(dat2)

baro_infil <-left_join(clim_infil1, climate, 
                       by=c('datetime'))

bar_reg <- lm(baro_infil$baro.x ~ baro_infil$baro.y, data=baro_infil)
summary(bar_reg)

baro_infil$baroM<- ((baro_infil$baro.x) * (summary(bar_reg)$coef[2,1]))
hist(baro_infil$baroM) 
hist(baro_infil$baro.x) 
hist(baro_infil$baro.y) 

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$baro[i])) {
    dat2$baro[i] <- baro_infil$baro.x[i]
  }
}
summary(dat2)

# Calculate the rolling average and store it in 'gageM'
dat2$parM <- rollapply(dat2$par, width = 10,
                       FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                       by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$par[i])) {
    dat2$par[i] <- dat2$parM[i]
  }
}

summary(dat2)

par_infil <-left_join(clim_infil2, climate, 
                      by=c('datetime'))

par_reg <- lm(par_infil$par.x ~ par_infil$par.y, data=par_infil)
summary(par_reg)

par_infil$parM<- ((par_infil$par.x) * summary(par_reg)$coef[2,1])
hist(par_infil$parM) 
hist(par_infil$par.x)
hist(par_infil$par.y)

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$par[i])) {
    dat2$par[i] <- par_infil$parM[i]
  }
}
# Summary of the updated 'dat21_2' dataset
summary(dat2)


qplot(datetime, par, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

dat3 <- dat2 %>%
  dplyr::select(datetime,
                do.obs,
                wtr,
                par, 
                baro,
                dischargeCFS,
                gageHF) #


# write.csv(x = dat3, file = "/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_GBLInputs.csv", row.names = TRUE)






#===============
# Upper sites
#================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/DOdat21/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% #, format ="%Y-%m-%d %H:%M:%S"))%>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

do.ts <- subset(do.ts,
                Q>0.7) # sensor quality minimum threshold

# do.ts$datetime <- do.ts$datetime - c(60*60*(6))

# tempcheck$datetime <- tempcheck$datetime - c(60*60*(6)) #? maybe 6

wtr.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 3,sd.dev = 5) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)


###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 3, sd.dev = 2) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)


# Merge by smaller time interval...
datx <- do.ts.clean %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.clean) 

## 2022
rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/DOdat22/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% #, format ="%Y-%m-%d %H:%M:%S"))%>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

do.ts <- subset(do.ts,
                datetime > "2022-04-07 18:00:00" & datetime < "2022-10-03 10:00:00",
                Q>0.7) # sensor quality minimum threshold

wtr.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

wtr.ts <- subset(wtr.ts,
                 datetime > "2022-04-07 18:00:00" & datetime < "2022-10-03 10:00:00",)


###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 3,sd.dev = 8) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)


###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 3, sd.dev = 8) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)


# Merge by smaller time interval...
datx1 <- do.ts.clean %>%
  full_join(wtr.ts.clean) 

dat2 <- rbind(datx, datx1)

## add in 2023 ###
rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/DOdat23/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

do.ts <- rawdat %>%
  dplyr::rename(datetime = "UTC_Date_&_Time", do.obs = "Dissolved Oxygen") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% #, format ="%Y-%m-%d %H:%M:%S"))%>%
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "do.obs", "Battery", "Q")

do.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(do.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

do.ts <- subset(do.ts, 
                Q>0.7) # sensor quality minimum threshold
#do.ts$datetime <- do.ts$datetime - hours(4)
range(do.ts$datetime)

tempcheck <- subset(do.ts, 
                    datetime > '2023-05-16 00:00:00' & datetime < '2023-05-17 :00:00') # sensor quality minimum threshold

qplot(datetime, do.obs, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2023-05-16 00:00:00", 
                                       "2023-05-16 12:00:00", 
                                       "2023-05-16 24:00:00")), color = "yellow")

###
wtr.ts <- rawdat %>% 
  dplyr::rename(datetime = "UTC_Date_&_Time", wtr = "Temperature") %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  #mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime", "wtr")

wtr.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(wtr.ts$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

## Check the dates:
tempcheck <- subset(wtr.ts, 
                    datetime > '2023-05-16 00:00:00' & datetime < '2023-05-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2023-05-16 00:00:00", 
                                       "2023-05-16 12:00:00", 
                                       "2023-05-16 24:00:00")), color = "yellow")

do.ts <- do.ts %>%
  dplyr::select("datetime", "do.obs")

###clean DO data
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts, width = 5,sd.dev = 3)  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 15)

###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts, width = 5, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

# add in climate:
# *st F9917 installed 2021-06-01
climate_23 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/F9917.2023-09-01.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                par="solar_radiation_set_1",
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro="pressure_set_1d")%>%
  #mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
                baro,
                Atemp,
                par,
                wspeed,
                hour,
                yday) # datetime in UTC

climate_23$datetime <- (as.POSIXct(round_date(
  as.POSIXct(climate_23$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

tempcheck <- subset(climate_23, 
                    datetime > '2022-05-15 10:00:00' & datetime < '2022-05-17 :10:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2022-05-16 00:00:00", 
                                       "2022-05-16 12:00:00", 
                                       "2022-05-16 24:00:00")), color = "yellow")


tempcheck <- subset(climate_23, 
                    datetime > '2022-07-16 00:00:00' & datetime < '2022-07-17 :00:00') # sensor quality minimum threshold

# tempcheck$datetime <- tempcheck$datetime - c(60*60*(7))

climate_23$datetime <- climate_23$datetime - c(60*60*(7))
tempcheck <- subset(climate_23, 
                    datetime > '2021-09-15 10:00:00' & datetime < '2021-09-17 :10:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")


### FROM BW #####
################

# add in climate:
clim_infil1 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/D9413.2023-09-01.csv", skip=) %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                wspeed='wind_speed_set_1',
                baro='pressure_set_1d')%>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
                Atemp,
                baro,
                wspeed,
                hour,
                yday) # datetime in UTC

clim_infil1$datetime <- (as.POSIXct(round_date(
  as.POSIXct(clim_infil1$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

tempcheck <- subset(clim_infil1, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, Atemp, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")


###
clim_infil2 <- read_csv("/Users/kellyloria/Documents/UNR/MSMmetab/HMDC1.2023-09-01.csv") %>% 
  dplyr::rename(datetime='Date_Time',
                Atemp='air_temp_set_1',
                par='solar_radiation_set_1',
                wspeed='wind_speed_set_1')%>%
  # mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S")) %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>% 
  mutate(year=year(datetime),
         yday=yday(datetime),
         hour=hour(datetime)) %>%
  dplyr::select(datetime,
                Atemp,
                par,
                wspeed,
                hour,
                yday) # datetime in UTC

clim_infil2$datetime <- (as.POSIXct(round_date(
  as.POSIXct(clim_infil2$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))



## Check the dates:
tempcheck <- subset(clim_infil2, 
                    datetime > '2021-09-16 00:00:00' & datetime < '2021-09-17 :00:00') # sensor quality minimum threshold

qplot(datetime, par, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours")) +
  geom_vline(xintercept = as.POSIXct(c("2021-09-16 00:00:00", 
                                       "2021-09-16 12:00:00", 
                                       "2021-09-16 24:00:00")), color = "yellow")
# 
datx2 <- do.ts.clean %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.clean)

summary(datx2)

dat3<- rbind(dat2, datx2)
summary(dat3)


## Add in flow data

library(dataRetrieval)

siteNo <- "10336730"
pCode <- c("00060", "00065")
start.date <- "2021-03-14"
end.date <- "2023-09-01"

GBflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

flow.ts <- GBflow %>%
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000", gageHF= "X_00065_00000") %>%
  dplyr::select("datetime", "dischargeCFS", "gageHF")

datv1 <- dat3 %>%
  full_join(flow.ts) %>%
  full_join(climate_23)
summary(datv1)
# 
# dat_unique <- datv1 %>%
#   group_by(datetime) %>%
#   summarize(do.obs = mean(do.obs, na.rm=T),
#             wtr = mean(wtr, na.rm=T),
#             par = mean(par, na.rm=T),
#             baro = mean(baro, na.rm=T),
#             dischargeCFS = mean(dischargeCFS, na.rm=T),
#             gageHF = mean(gageHF, na.rm=T))

dat1<- datv1[!is.na(datv1$do.obs), ]
dat2<- dat1[!is.na(dat1$wtr), ]

summary(dat2)

# Calculate the rolling average and store it in 'gageM'
dat2$gageM <- rollapply(dat2$gageHF, width = 5,
                        FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                        by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$gageHF[i])) {
    dat2$gageHF[i] <- dat2$gageM[i]
  }
}
# Summary of the updated 'dat21_2' dataset
summary(dat2)


gageinfil<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_GBLInputs.csv")
summary(gageinfil)
gageinfil <- gageinfil %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>%
  dplyr::select("datetime","baro", "dischargeCFS", "gageHF", "par")

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$gageHF[i])) {
    dat2$gageHF[i] <- gageinfil$gageHF[i]
  }
}

qplot(datetime, gageHF, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# Calculate the rolling average 
dat2$dischargeM <- rollapply(dat2$dischargeCFS, width = 5,
                             FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                             by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$dischargeCFS[i])) {
    dat2$dischargeCFS[i] <- dat2$dischargeM[i]
  }
}
# Summary of the updated 'dat21_2' dataset
summary(dat2)

qplot(datetime, dischargeCFS, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

datsave<- dat2
# Calculate the rolling average and store it in 'gageM'
dat2$baroM <- rollapply(dat2$baro, width = 5,
                        FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                        by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$baro[i])) {
    dat2$baro[i] <- dat2$baroM[i]
  }
}
summary(dat2)


###
baro_infil <-left_join(clim_infil1, climate_23, 
                       by=c('datetime'))

bar_reg <- lm(baro_infil$baro.x ~ baro_infil$baro.y, data=baro_infil)
summary(bar_reg)

baro_infil$baroM<- ((baro_infil$baro.x) / (summary(bar_reg)$coef[2,1]))
hist(baro_infil$baroM) 
hist(baro_infil$baro.x) 
hist(baro_infil$baro.y) 

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$baro[i])) {
    dat2$baro[i] <- baro_infil$baro.x[i]
  }
}
summary(dat2)
###

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$baro[i])) {
    dat2$baro[i] <- gageinfil$baro[i]
  }
}

qplot(datetime, baro, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# Summary of the updated 'dat21_2' dataset
summary(dat2)


# Calculate the rolling average and store it in 'gageM'
dat2$parM <- rollapply(dat2$par, width = 5,
                       FUN = function(x) mean(x, na.rm = TRUE), by = 1,
                       by.column = TRUE, partial = TRUE, fill = NA, align = "center")
# Loop through each row and fill missing 'gageHF' values with 'gageM'
for (i in 1:nrow(dat2)) {
  if (is.na(dat2$par[i])) {
    dat2$par[i] <- dat2$parM[i]
  }
}


## SM light ##
latitude <- c(39.10740708)
longitude <- c(-120.16213517)
dat2$solar.time <- calc_solar_time(dat2$datetime, longitude)
dat2$light_cal<- calc_light(
  dat2$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(dat2$solar.time)
)

par_reg <- lm(dat2$light_cal ~ dat2$par, data=dat2)
summary(par_reg)

dat2$parIF<- ((dat2$light_cal) * (summary(par_reg)$coef[2,1])*-1)
hist(dat2$parIF) 
hist(dat2$light_cal) 
hist(dat2$par) 

for (i in 1:nrow(dat2)) {
  if (is.na(dat2$par[i])) {
    dat2$par[i] <- dat2$parIF[i]
  }
}

summary(dat2)

qplot(datetime, par, data = dat2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

dat3 <- dat2 %>%
  dplyr::select(datetime,
                do.obs,
                wtr,
                par, 
                baro,
                dischargeCFS,
                gageHF) #

summary(dat3)


# write.csv(x = dat3, file = "/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_GBUInputs.csv", row.names = TRUE)





#  write.csv(x = dat1, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBUInputs.csv", row.names = TRUE)

