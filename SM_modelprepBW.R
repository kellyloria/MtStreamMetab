rm(list=ls())

library(unitted)
library(StreamMetabolism)
library(streamMetabolizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(rstan)
library(zoo)
library(lubridate)
library(dataRetrieval)

# See what data you need:
metab_inputs('bayes','data')

dat<- readRDS("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/24_BWL_Inputs.rds")
summary(dat)
str(dat)

# get all units in model form #
dat$discharge <- c(dat$dischargeCFS *  0.028317) # did you need to cube the flow
hist(dat$discharge)


###
## Depth rating curve: 

date<- as.POSIXct(c(
                    "2021-06-06 10:00:00",
                    "2021-07-28 18:00:00",
                    "2022-05-26 18:00:00",
                    "2022-08-24 18:00:00",
                    "2022-10-11 18:00:00",
                    "2022-11-21 18:00:00",
                    "2022-12-19 16:00:00",
                    "2023-02-15 16:00:00",
                    "2023-04-05 16:00:00",
                    "2023-07-18 14:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.153, 
           0.090, 
           0.428, #
           0.151, 
           0.096, 
           0.086, 
           0.294, 
           0.161, 
           0.388, #
           0.322)

mophDF <- data.frame(date,depth)

DRC <- left_join(dat, mophDF[c("date", "depth")],
                   by = c("datetime" = "date"))
summary(DRC)
DRC$gageHm <-c(DRC$gageHF *0.3048)

# regression between reach depth and gage height
depth.lm<- glm(gageHm~scale(depth), data=DRC)
summary(depth.lm)

plot(DRC$depth~ DRC$gageHm)

#BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
DRC$est.depth<- ((DRC$gageHm) * summary(depth.lm)$coef[2,1])
hist(DRC$est.depth) 
hist(DRC$gageHm)
hist(DRC$depth) 

dat$depth <- ((dat$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])
hist(dat$depth) 
# calc light example
latitude <- c(39.10740708)
longitude <- c(-120.16213517)

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

dat$light_st <- c(dat$light*2.114)
hist(dat$light_st)

dat$DO.sat <- calc_DO_sat(dat$wtr, 
                          dat$baro, #
                            sal=0) 
hist(dat$DO.sat)


# Get data in correct name and column form
names(dat)



colnames(dat)[3] <- "DO.obs"
colnames(dat)[4] <- "temp.water"
colnames(dat)[13] <- "light"


# New named df
mdat <- subset(dat, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(x = mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/24_BWLmodelInputs.csv", row.names = TRUE)

# saveRDS(mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/24_BWL_modelInputs.rds")


mdat_low <- mdat%>%
  filter(discharge<0.79)
hist(mdat_low$discharge)
hist(mdat$discharge)

# saveRDS(mdat_low, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/24_GBL_modelInputs_lowflow.rds")


#####
#####
#####
####

# start with Blackwood
result <- mdat %>%
  #group_by(Site) %>%
  dplyr::summarize(
    total_intervals = n(),
    ranked_discharge = list(sort(discharge, decreasing = TRUE)),
    exceedence_prob = list(seq(1, n(), length.out = n()) / n() * 100)
  ) %>%
  unnest(cols = c(ranked_discharge, exceedence_prob))

# Plotting for each site
result %>%
  ggplot(aes(y = (ranked_discharge), x = exceedence_prob)) +
  geom_line() + theme_bw() +
  scale_color_manual(values=alpha(c("#3283a8"),0.5)) +
  labs(y = "Discharge (cms/km)", x = "Exceedence Probability", title = "Exceedence Probability vs. Discharge by Site") 

# 25% of flow exceeds 0.7872126 m3s 
# 50% of flow exceeds 0.3539625 m3s



##
###
#=========================
# UPPER site?
#=========================
###
##

### 2021
##

dat21<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_BWUInputs.csv")
summary(dat21)
str(dat21)

dat21 <- dat21 %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
  dplyr::select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")


# get all units in model form #
dat21$pressure_millibar <- c(dat21$baro * 0.01)
dat21$dischargeR <- c(dat21$dischargeCFS *  35.3147)

###
## Depth rating curve: 
date<- as.POSIXct(c("2021-07-12 13:00:00",
                    "2021-08-09 13:00:00",
                    "2021-08-20 18:00:00",
                    "2023-07-18 12:00:00",
                    "2023-07-18 13:00:00",
                    "2023-08-10 12:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.136166667, 
           0.154806,
           0.1263684,
           0.301,
           0.248,
           0.1057)
flow <- c(48.68312,
          36.68312,
          28.68312, 
          910.55,
          740.53,
          191.69)

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
colnames(dat21)[13] <- "light"
colnames(dat21)[11] <- "discharge"



# New named df
mdat <- subset(dat21, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(x = BWdat21, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/23_BWUmodelInputs.csv", row.names = TRUE)


### 2022
##

dat22<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWUInputs.csv")
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
colnames(dat22)[13] <- "light"
colnames(dat22)[11] <- "discharge"



# New named df
BWdat22 <- subset(dat22, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(x = BWdat22, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_BWUmodelInputs.csv", row.names = TRUE)



