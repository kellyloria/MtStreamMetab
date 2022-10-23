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

