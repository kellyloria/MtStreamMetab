# GB stream SPC agg

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

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Glenbrook\ Lower/SPC/20775509",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775509, SEN S/N: 20775509)", 
                wtr="Temp, °C (LGR S/N: 20775509, SEN S/N: 20775509)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

qplot(datetime, wtr, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

qplot(datetime, SPC, data = SPC.ts2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("200 hours"))

SPC.ts2 <- subset(SPC.ts,
                    datetime < '2021-05-29 00:00:00' | datetime > '2021-06-04 :00:00') # sensor quality minimum threshold

tempcheck <- subset(SPC.ts,
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

file <- list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Glenbrook\ Lower/SPC/20775525/",sep=""), full.names = T)
rawdat1 <-  do.call(rbind, lapply
                   (file, read.csv, as.is=T, header = T))

names(rawdat1)

SPC.ts1 <- rawdat1 %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775525..SEN.S.N..20775525.", 
                wtr="Temp...C..LGR.S.N..20775525..SEN.S.N..20775525.") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts1$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts1$datetime, format="%Y-%m-%dT%H:%M:%SZ"), 
  hour, unit="5 minutes")))

qplot(datetime, wtr, data = SPC.ts1, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))


tempcheck <- subset(SPC.ts1,
                    datetime > '2022-07-16 00:00:00' & datetime < '2022-07-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))



SPCdat<- rbind(SPC.ts2, SPC.ts1)
SPCdat<- na.omit(SPCdat)
range((SPCdat$datetime))

qplot(datetime, SPC, data = SPCdat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

SPCdat <- subset(SPCdat,
                  SPC >=100) # sensor quality minimum threshold

wtr.ts <- SPCdat %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 5, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)

SPC.ts <- SPCdat %>%
  select("datetime", "SPC")

ggplot(data = ,aes(x=datetime,y=SPC)) + geom_line()
describe.ts(SPC.ts)
SPC.ts.clean <- trim.outliers(SPC.ts,width = 5, sd.dev = 5) 
ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line() + 
  geom_line(data=SPC.ts.clean,aes(x=datetime,y=SPC),col="red")
SPC.ts.avg <- aggregate.data(data = SPC.ts.clean,time.step = 60)

dat <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)


qplot(datetime, wtr, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))


# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/GBL_SPC.csv", row.names = TRUE)


#=========================================== 
# GBU
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/SPC/20775518/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775518, SEN S/N: 20775518)", 
                wtr="Temp, °C (LGR S/N: 20775518, SEN S/N: 20775518)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))


##
rawdat2 = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/SPC/20775508",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat2)

SPC.ts2 <- rawdat2 %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775508, SEN S/N: 20775508)", 
                wtr="Temp, °C (LGR S/N: 20775508, SEN S/N: 20775508)" ) %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts2$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts2$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))


tempcheck <- subset(SPC.ts2,
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))

# SPC.ts$datetime <- SPC.ts$datetime - c(60*60*(6)) #? maybe 6

wtr.ts <- SPC.ts %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 8, sd.dev = 4) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

SPC.ts <- SPC.ts %>%
  select("datetime", "SPC")

ggplot(data = ,aes(x=datetime,y=SPC)) + geom_line()
describe.ts(SPC.ts)
SPC.ts.clean <- trim.outliers(SPC.ts,width = 12, sd.dev = 4) 
ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line() + 
  geom_line(data=SPC.ts.clean,aes(x=datetime,y=SPC),col="red")
SPC.ts.avg <- aggregate.data(data = SPC.ts.clean,time.step = 15)

datU <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)

qplot(datetime, wtr, data = datU, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("500 hours"))


#=========================================== 
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/GlenbrookUpper/SPC/20775518",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775518, SEN S/N: 20775518)", 
                wtr="Temp, °C (LGR S/N: 20775518, SEN S/N: 20775518)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%m/%d/%y %H:%M"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

qplot(datetime, wtr, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("100 hours"))


tempcheck <- subset(SPC.ts,
                    datetime > '2022-08-16 00:00:00' & datetime < '2022-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))



SPCdat<- rbind(SPC.ts2, SPC.ts)
SPCdat<- na.omit(SPCdat)
range((SPCdat$datetime))

range(SPCdat$datetime)

wtr.ts <- SPCdat %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 5, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)

SPC.ts <- SPCdat %>%
  select("datetime", "SPC")

ggplot(data = ,aes(x=datetime,y=SPC)) + geom_line()
describe.ts(SPC.ts)
SPC.ts.clean <- trim.outliers(SPC.ts,width = 5, sd.dev = 3) 
ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line() + 
  geom_line(data=SPC.ts.clean,aes(x=datetime,y=SPC),col="red")
SPC.ts.avg <- aggregate.data(data = SPC.ts.clean,time.step = 60)

datU1 <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)

qplot(datetime, SPC, data = datU1, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("900 hours"))



# write.csv(x = datU1, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/GBU_SPC.csv", row.names = TRUE)

