# Blackwood stream SPC agg

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

# "2021-04-29 13:00:00 PDT" "2022-03-19 15:05:00 PDT"
files <- list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_lower/SPC_20775510/",sep=""), full.names = T)
rawdat <-  do.call(rbind, lapply
                 (files, read.csv, as.is=T, skip = 1, header = T))

names(rawdat)
SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775510..SEN.S.N..20775510.", 
                wtr="Temp...C..LGR.S.N..20775510..SEN.S.N..20775510.") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  dplyr::select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%dT%H:%M:%SZ"),unit="5 minutes")))

qplot(datetime, wtr, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("500 hours"))


# missing the 2022-04 to 2022-08

## "2022-08-24 01:00:00 PDT" "2023-01-25 13:00:00 PST"
files2 <- list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_lower/SPC_20775511/",sep=""), full.names = T)
rawdat2 <-  do.call(rbind, lapply
                    (files2, read.csv, as.is=T, skip = 1, header = T))
names(rawdat2)

SPC.tsL <- rawdat2 %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775511..SEN.S.N..20775511.", 
                wtr="Temp...C..LGR.S.N..20775511..SEN.S.N..20775511.") %>%
  dplyr::mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  dplyr::select("datetime", "SPC", "wtr")

SPC.tsL$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.tsL$datetime, format="%Y-%m-%dT%H:%M:%SZ"), unit="5 minutes")))

qplot(datetime, wtr, data = SPC.tsL, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("300 hours"))

range(na.omit(SPC.tsL$datetime))


###
## "2022-08-24 01:00:00 PDT" "2023-01-25 13:00:00 PST"
files2 <- list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_lower/SPC_20775515/",sep=""), full.names = T)
rawdat2 <-  do.call(rbind, lapply
                    (files2, read.csv, as.is=T, skip = 1, header = T))
names(rawdat2)

SPC.tsL1 <- rawdat2 %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775515..SEN.S.N..20775515.", 
                wtr="Temp...C..LGR.S.N..20775515..SEN.S.N..20775515.") %>%
  dplyr::mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  dplyr::select("datetime", "SPC", "wtr")

SPC.tsL1$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.tsL1$datetime, format="%Y-%m-%dT%H:%M:%SZ"), unit="5 minutes")))

qplot(datetime, wtr, data = SPC.tsL1, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("300 hours"))

range(na.omit(SPC.tsL$datetime))

###







SPCdat<- rbind(SPC.ts, SPC.tsL, SPC.tsL1)
SPCdat<- na.omit(SPCdat)
range((SPCdat$datetime))

qplot(datetime, wtr, data = SPCdat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))


tempcheck <- subset(SPCdat,
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))


qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("2 hours"))


# SPC.ts$datetime <- SPC.ts$datetime - c(60*60*(6)) #? maybe 6

wtr.ts <- SPCdat %>%
  dplyr::select("datetime", "wtr")

###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 5, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)

SPC.ts <- SPCdat %>%
  dplyr::select("datetime", "SPC")

ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line()
describe.ts(SPC.ts)
SPC.ts.clean <- trim.outliers(SPC.ts,width = 5, sd.dev = 3) 
ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line() + 
  geom_line(data=SPC.ts.clean,aes(x=datetime,y=SPC),col="red")
SPC.ts.avg <- aggregate.data(data = SPC.ts.clean,time.step = 15)

dat <- SPC.ts.clean%>% 
  full_join(wtr.ts.clean)

dat<- SPCdat

qplot(datetime, SPC, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# SPC.ts

# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_BWL_SPCa.csv", row.names = TRUE)



#=========================================== 
# Upper 2021
#===========================================

files = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_upper/2021/",sep=""), full.names = T)

rawdat <-  do.call(rbind, lapply
                    (files, read.csv, as.is=T, header = T))
names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775523..SEN.S.N..20775523.", 
                wtr="Temp...C..LGR.S.N..20775523..SEN.S.N..20775523.") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  dplyr::select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%dT%H:%M:%SZ"), unit="5 minutes")))

qplot(datetime, SPC, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("100 hours"))
 
range(SPC.ts$datetime)


files2 = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_upper/2022/",sep=""), full.names = T)

rawdat2 <-  do.call(rbind, lapply
                   (files2, read.csv, as.is=T, header = T))
names(rawdat2)

SPC.ts2 <- rawdat2 %>%
  dplyr::rename(datetime = "Date.Time..GMT.07.00", 
                SPC = "Full.Range..μS.cm..LGR.S.N..20775508..SEN.S.N..20775508.", 
                wtr= "Temp...C..LGR.S.N..20775508..SEN.S.N..20775508.") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"))%>%
  dplyr::select("datetime", "SPC", "wtr")

SPC.ts2$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts2$datetime, format="%Y-%m-%d %H:%M:%S"), unit="5 minutes")))

qplot(datetime, SPC, data = SPC.ts2, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("500 hours"))


qplot(datetime, SPC, data = SPC.ts2s, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("100 hours"))

SPCdat_up<- rbind(SPC.ts, SPC.ts2)

qplot(datetime, wtr, data = SPCdat_up, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("800 hours"))

wtr.ts <- SPCdat_up %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = wtr.ts, aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 5, sd.dev = 3) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 15)

SPC.ts <- SPCdat_up %>%
  select("datetime", "SPC")

ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line()
describe.ts(SPC.ts)
SPC.ts.clean <- trim.outliers(SPC.ts,width = 5, sd.dev = 3) 
ggplot(data = SPC.ts,aes(x=datetime,y=SPC)) + geom_line() + 
  geom_line(data=SPC.ts.clean,aes(x=datetime,y=SPC),col="red")
SPC.ts.avg <- aggregate.data(data = SPC.ts.clean,time.step = 15)

dat <- SPC.ts.clean %>% 
  full_join(wtr.ts.clean)

qplot(datetime, SPC, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("800 hours"))

# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_BWU_SPC.csv", row.names = TRUE)
