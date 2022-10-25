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

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_lower/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775510, SEN S/N: 20775510)", 
                wtr="Temp, °C (LGR S/N: 20775510, SEN S/N: 20775510)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%m/%d/%y %H:%M"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

range(na.omit(SPC.ts$datetime))

# do.ts <- subset(do.ts,
#                 datetime > "2021-03-15 00:00:00",
#                 Q>0.7) # sensor quality minimum threshold

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

tempcheck <- subset(SPC.ts,
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# SPC.ts$datetime <- SPC.ts$datetime - c(60*60*(6)) #? maybe 6

wtr.ts <- SPC.ts %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 8, sd.dev = 3) 
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

dat <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)

qplot(datetime, SPC, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))

# SPC.ts

# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWL_SPC.csv", row.names = TRUE)



#=========================================== 
# Upper 2021
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_upper/2021/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775523, SEN S/N: 20775523)", 
                wtr="Temp, °C (LGR S/N: 20775523, SEN S/N: 20775523)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%m/%d/%y %H:%M"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

tempcheck <- subset(SPC.ts,
                    datetime > '2021-08-16 00:00:00' & datetime < '2021-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

# SPC.ts$datetime <- SPC.ts$datetime - c(60*60*(6)) #? maybe 6

SPC.ts <- subset(SPC.ts, 
                  datetime > '2021-07-01 00:00:00' & datetime < '2021-09-08 09:00:00') 

qplot(datetime, SPC, data = SPC.ts1, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


wtr.ts <- SPC.ts %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 8, sd.dev = 3) 
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

dat <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)

qplot(datetime, SPC, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("100 hours"))


# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/21_BWU_SPC.csv", row.names = TRUE)


#=========================================== 
# Upper 2022
#===========================================

rawdat = list.files(paste("/Users/kellyloria/Documents/UNR/MSMmetab/Blackwood/SPC/SPC_upper/2022/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()

names(rawdat)

SPC.ts <- rawdat %>%
  dplyr::rename(datetime = "Date Time, GMT-07:00", 
                SPC = "Full Range, μS/cm (LGR S/N: 20775508, SEN S/N: 20775508)", 
                wtr="Temp, °C (LGR S/N: 20775508, SEN S/N: 20775508)") %>%
  mutate(datetime = as.POSIXct((datetime), format ="%m/%d/%y %H:%M"))%>%
  select("datetime", "SPC", "wtr")

SPC.ts$datetime <-  (as.POSIXct(round_date(
  as.POSIXct(SPC.ts$datetime, format="%Y-%m-%d %H:%M:%S"), 
  hour, unit="5 minutes")))

tempcheck <- subset(SPC.ts,
                    datetime > '2022-08-16 00:00:00' & datetime < '2022-08-17 :00:00') # sensor quality minimum threshold

qplot(datetime, wtr, data = tempcheck, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1 hours"))

# SPC.ts$datetime <- SPC.ts$datetime - c(60*60*(6)) #? maybe 6

SPC.ts <- subset(SPC.ts, 
                 datetime > '2022-07-09 14:00:00' & datetime < '2022-10-08 14:00:00') 

qplot(datetime, SPC, data = SPC.ts, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


wtr.ts <- SPC.ts %>%
  select("datetime", "wtr")

###clean wtr data
ggplot(data = ,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 8, sd.dev = 3) 
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

dat <- SPC.ts.avg %>% 
  full_join(wtr.ts.avg)

qplot(datetime, SPC, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("100 hours"))


# write.csv(x = dat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWU_SPC.csv", row.names = TRUE)

