rm(list=ls())

# Load packages 
library(StreamMetabolism)
library(streamMetabolizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(rstan)
library(unitted)
library(zoo)
library(lubridate)
library(dataRetrieval)

# See what data you need:
metab_inputs('bayes','data')

dat<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBLInputs.csv")
summary(dat)

dat <- dat %>%
  mutate(datetime = as_datetime(datetime, "America/Los_Angeles")) %>%
  select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")

str(dat)

# get all units in model form #
dat$pressure_millibar <- c(dat$baro * 0.01)
dat$discharge <- c(dat$dischargeCFS *  28.317)

###
## Depth rating curve: 

date<- as.POSIXct(c("2022-01-21 17:00:00",
                    "2021-06-03 14:00:00",
                    "2021-12-01 10:00:00",
                    "2022-06-09 13:00:00",
                    "2022-07-28 18:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.1437662, 
           0.3125,
           0.2220309,
           0.1529198, 
           0.09045016)

mophDF <- data.frame(date,depth)

DRC <- left_join(dat, mophDF[c("date", "depth")],
                 by = c("datetime" = "date"))
summary(DRC)
DRC$gageHm <-c(DRC$gageHF *0.3048)

# regression between reach depth and gage height
depth.lm<- glm(depth~gageHm, data=DRC)
summary(depth.lm)

#BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
DRC$est.depth<- ((DRC$gageHm) * summary(depth.lm)$coef[2,1])
hist(DRC$est.depth) 

dat$depth <- ((dat$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])

# calc light example
latitude <- c(39.08740806)
longitude <- c(-119.93990567)

dat$solar.time <- calc_solar_time(dat$datetime, longitude)

## SM light ##
dat$light_cal<- calc_light(
  dat$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(dat$solar.time)
)

hist(dat$light_cal)

dat$light_st <- c(dat$par*2.114)
hist(dat$light_st)

dat$DO.sat <- calc_DO_sat(dat$wtr, 
                          dat$pressure_millibar,
                          sal=0) 
hist(dat$DO.sat)


# Get data in correct name and column form
names(dat)

colnames(dat)[3] <- "temp.water"
colnames(dat)[2] <- "DO.obs"
colnames(dat)[13] <- "light"


# New named df
mdat <- subset(dat, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))
# write.csv(x = mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBL_modelInputs.csv", row.names = TRUE)

##
###
#=========================
# UPPER site?
#=========================
###
##

### 2021
##

dat21<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/21_GBUInputs.csv")
summary(dat21)
str(dat21)

dat21 <- dat21 %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")


# get all units in model form #
dat21$pressure_millibar <- c(dat21$baro * 0.01)
dat21$dischargeR <- c(dat21$dischargeCFS *  35.3147)

###
## Depth rating curve: 

date<- as.POSIXct(c("2021-06-12 13:00:00",
                    "2021-07-22 13:00:00",
                    "2021-11-20 18:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(1.6965168, 1.1, 0.9144)
flow <- c(8.768033,4.968033,16)

mophDF <- data.frame(date,depth, flow)

DRC <- left_join(dat21, mophDF[c("date", "depth", "flow")],
                 by = c("datetime" = "date"))
summary(DRC)
DRC$gageHm <-c(DRC$gageHF *0.3048)

# regression between reach depth and gage height
depth.lm<- glm(depth~gageHm, data=DRC)
summary(depth.lm)

flow.lm<- glm(flow~dischargeR, data=DRC)
summary(flow.lm)
#BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
DRC$est.depth<- ((DRC$gageHm) * summary(depth.lm)$coef[2,1])
hist(DRC$est.depth) 

dat21$depth <- ((dat21$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])

DRC$est.flow<- ((DRC$dischargeR) * summary(flow.lm)$coef[2,1])
hist(DRC$est.flow) 

dat21$discharge.est <- ((dat21$dischargeR) * summary(flow.lm)$coef[2,1])
hist(dat21$discharge.est) 

# calc light example
latitude <- c(39.10740708)
longitude <- c(-120.16213517)

dat21$solar.time <- calc_solar_time(dat21$datetime, longitude)

## SM light ##
dat21$light_cal<- calc_light(
  dat21$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(dat21$solar.time)
)

hist(dat21$light_cal)

dat21$light_st <- c(dat21$par*2.114)
hist(dat21$light_st)

dat21$DO.sat <- calc_DO_sat(dat21$wtr, 
                            dat21$pressure_millibar,
                            sal=0) 
hist(dat21$DO.sat)


# Get data in correct name and column form
names(dat21)

colnames(dat21)[3] <- "temp.water"
colnames(dat21)[2] <- "DO.obs"
colnames(dat21)[14] <- "light"
colnames(dat21)[11] <- "discharge"


# New named df
mdat <- subset(dat21, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))
# write.csv(x = mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/21_GBU_modelInputs.csv", row.names = TRUE)


### 2022
##

dat22<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBUInputs.csv")
summary(dat22)
str(dat22)

dat22 <- dat22 %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")


# get all units in model form #
dat22$pressure_millibar <- c(dat22$baro * 0.01)
dat22$dischargeR <- c(dat22$dischargeCFS *  35.3147)

###
## Depth rating curve: 

date<- as.POSIXct(c("2022-07-12 13:00:00",
                    "2022-08-09 13:00:00",
                    "2022-08-20 18:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.136166667, 0.154806,0.1263684)
flow <- c(48.68312,36.68312,28.68312)

mophDF <- data.frame(date,depth, flow)

DRC <- left_join(dat22, mophDF[c("date", "depth", "flow")],
                 by = c("datetime" = "date"))
summary(DRC)
DRC$gageHm <-c(DRC$gageHF *0.3048)

# regression between reach depth and gage height
depth.lm<- glm(depth~gageHm, data=DRC)
summary(depth.lm)

flow.lm<- glm(flow~dischargeR, data=DRC)
summary(flow.lm)
#BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
DRC$est.depth<- ((DRC$gageHm) * summary(depth.lm)$coef[2,1]) 
hist(DRC$est.depth) 

dat22$depth <- ((dat22$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])

DRC$est.flow<- ((DRC$dischargeR) * summary(flow.lm)$coef[2,1])
hist(DRC$est.flow) 

dat22$discharge.est <- ((dat22$dischargeR) * summary(flow.lm)$coef[2,1])


# calc light example
latitude <- c(39.10740708)
longitude <- c(-120.16213517)

dat22$solar.time <- calc_solar_time(dat22$datetime, longitude)

## SM light ##
dat22$light_cal<- calc_light(
  dat22$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(dat22$solar.time)
)

hist(dat22$light_cal)

dat22$light_st <- c(dat22$par*2.114)
hist(dat22$light_st)

dat22$DO.sat <- calc_DO_sat(dat22$wtr, 
                            dat22$pressure_millibar,
                            sal=0) 
hist(dat22$DO.sat)


# Get data in correct name and column form
names(dat22)

colnames(dat22)[3] <- "temp.water"
colnames(dat22)[2] <- "DO.obs"
colnames(dat22)[14] <- "light"
colnames(dat22)[11] <- "discharge"


# New named df
mdat22 <- subset(dat22, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(x = mdat22, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBUmodelInputs.csv", row.names = TRUE)


