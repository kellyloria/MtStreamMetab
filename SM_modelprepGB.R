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

dat<- readRDS("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/24_GBLInputs.rds")
summary(dat)
str(dat)

# get all units in model form #
dat$discharge <- c(dat$dischargeCFS *  0.028317) # did you need to cube the flow
hist(dat$discharge)

###
## Depth rating curve: 

date<- as.POSIXct(c("2022-01-21 17:00:00",
                    "2021-06-03 14:00:00",
                    "2021-12-01 10:00:00",
                    "2022-06-09 13:00:00",
                    "2022-06-23 13:00:00",
                    "2022-07-28 18:00:00",
                    "2022-10-03 10:00:00",
                    "2022-11-04 13:00:00",
                    "2022-12-12 13:00:00",
                    "2022-03-27 13:00:00"
                    ), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.144, 
           0.130,
           0.191,
           0.153, 
           0.191,
           0.091,
           0.106, 
           0.142, 
           0.243,
           0.166)

mophDF <- data.frame(date,depth)

DRC <- left_join(dat, mophDF[c("date", "depth")],
                 by = c("datetime" = "date"))
summary(DRC)
DRC$gageHm <-c(DRC$gageHF *0.3048)

# regression between reach depth and gage height
depth.lm<- glm(gageHm~(depth), data=DRC)
summary(depth.lm)

plot(DRC$depth~ DRC$gageHm)

#BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
DRC$est.depth <- ((DRC$gageHm) * summary(depth.lm)$coef[2,1])
hist(DRC$est.depth) 

hist(DRC$gageHm)
hist(DRC$depth) 


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

dat$light_st <- c(dat$light*2.114)
hist(dat$light_st)

# Raise the conductivity (in mS/cm) to the power 1.0878. 
# Multiply the result by 0.4665. 
# This gives you salinity in grams (of salt) per liter (of solution)

dat$DO.sat <- calc_DO_sat(dat$wtr, 
                          dat$baro,
                          sal=10.5635) 
hist(dat$DO.sat)


# Get data in correct name and column form
names(dat)

colnames(dat)[4] <- "temp.water"
colnames(dat)[3] <- "DO.obs"
colnames(dat)[13] <- "light"


# New named df
mdat <- subset(dat, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))
# write.csv(x = mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/24_GBL_modelInputs.csv", row.names = TRUE)
# saveRDS(mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/24_GBL_modelInputs.rds")


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
  labs(y = "Discharge (cms/km)", x = "Exceedence Probability", title = "Exceedence Probability vs. Discharge by Site") 

# 25% of flow exceeds 0.04021014 m3s 
# 50% of flow exceeds 0.200 m3s 

####
####
####
####

library(scales)
qplot(datetime, discharge , data = mdat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("500 hours"))












## DO -- subset to time frame of interest & plot
lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","data.table","xts","dygraphs",
         "nrlmetab","cowplot"), require, character.only=T)

vis_data_DO <- function(x){
  
  x <- subset(x, x$datetime < "2022-10-02 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$DO.sat, order.by = x$datetime)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

vis_data_flow <- function(x){
  
  x <- subset(x, x$datetime < "2022-10-02 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$discharge, order.by = x$datetime)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

vis_data_DO(mdat)
vis_data_flow(mdat)

# might need to remove 2021-10-24/25, 2021-12-23/24 and 2021-12-27/28
# for extremely high flows

Fall2021<- subset(mdat,  datetime< '2021-10-24 10:00:00' | datetime > '2021-10-26 10:00:00')

Dec2021<- subset(Fall2021,  datetime <'2021-12-23 00:00:00' | datetime > '2021-12-24 00:00:00')

Dec20212<- subset(Dec2021,  datetime <'2021-12-27 00:00:00' | datetime > '2021-12-29 00:00:00')

vis_data_flow(Dec2021)

# write.csv(x = Dec2021, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/21S_GBL_modelInputs.csv", row.names = TRUE)


# 2022-06-01 to 2022-10-01
library(ggplot2)
datS22<- subset(dat, datetime > '2022-07-01 00:00:00' & datetime < '2022-10-01 :00:00')

qplot(datetime, discharge , data = datS22, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("500 hours"))

# write.csv(x = datS22, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22S_GBL_modelInputs.csv", row.names = TRUE)


## Look at critical flows?
# install.packages("", dependencies = T)
# library(EcoHydRology)
# 
# 
# bfs<- BaseflowSeparation(dat$discharge, filter_parameter = 0.925, passes = 3)
# hydrograph(input=dat$discharge,streamflow2=bfs[,1])
# 
# 
# devtools::install_github("nickbond/hydrostats")
# library(hydrostats)
# # Coefficient of variation = sd/mean
# baseflows(dat$discharge, a, n.reflected = 30, ts = "mean")
# 
# data(Acheron)
# Acheron<-ts.format(Acheron)
# baseflows(Acheron,a=0.975, ts="mean")
# baseflows(Acheron,a=0.975, ts="annual")
# head(baseflows(Acheron,a=0.975, ts="daily"))
# 
# baseflw <- daily.cv(dat$solar.time, dat$discharge)
# daily.cv(Acheron)

##
###
#=========================
# UPPER site?
#=========================
###
##

dat<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/23_CleanDat/23_GBUInputs.csv")
summary(dat)
str(dat)

dat <- dat %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%dT%H:%M:%SZ"),tz = "UTC")%>%
  dplyr::select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")


# get all units in model form #
dat$pressure_millibar <- c(dat$baro * 0.01)
dat$dischargeR <- c(dat$dischargeCFS *  35.3147)

#vis_data_flow(dat21)

###
## Depth rating curve: 

date<- as.POSIXct(c("2021-03-25 13:00:00",
                    "2021-04-25 13:00:00",
                    "2022-04-07 13:00:00",
                    "2021-07-22 13:00:00",
                    "2022-06-22 13:00:00",
                    "2022-10-02 13:00:00",
                    "2023-03-27 13:00:00"), 
                  tz="America/Los_Angeles",
                  format = c("%Y-%m-%d %H:%M:%OS"))

depth <- c(0.126, 0.117, NA, 0.098, 0.159, 0.067, 0.123)
flow <- c(NA, NA, 48.634, 4.968, 54.630, 4.909, 42.03)

mophDF <- data.frame(date,depth, flow)

DRC <- left_join(dat, mophDF[c("date", "depth", "flow")],
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

dat$depth <- ((dat$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])
hist(dat$depth)

DRC$est.flow<- ((DRC$dischargeR) * summary(flow.lm)$coef[2,1]) * (-1)
hist(DRC$est.flow) 

dat$discharge.est <- ((dat$dischargeR) * summary(flow.lm)$coef[2,1]) * (-1)
hist(dat$discharge.est) 

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

dat$light_st <- c(dat$par*2.114)
hist(dat$light_st)

qplot(datetime, light_st, data = dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(breaks = date_breaks("1000 hours"))


dat$DO.sat <- calc_DO_sat(dat$wtr, 
                            dat$pressure_millibar,
                            sal=6) 
hist(dat$DO.sat)


# Get data in correct name and column form
names(dat)

colnames(dat)[3] <- "temp.water"
colnames(dat)[2] <- "DO.obs"
colnames(dat)[14] <- "light"
colnames(dat)[11] <- "discharge"


# New named df
mdat <- subset(dat, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))
# write.csv(x = mdat, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/23_GBU_modelInputs.csv", row.names = TRUE)


range(mdat$datetime)

vis_data_DO <- function(x){
  
  x <- subset(x, x$datetime < "2022-10-02 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$DO.obs, order.by = x$datetime)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

vis_data_flow <- function(x){
  
  x <- subset(x, x$datetime < "2022-10-02 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$discharge, order.by = x$datetime)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

vis_data_DO(mdat)
vis_data_flow(mdat)

# might need to remove 2021-10-24/25, 2021-12-23/24 and 2021-12-27/28
# for extremely high flows

Fall2021<- subset(mdat,  datetime< '2021-10-24 10:00:00' | datetime > '2021-10-26 10:00:00')

vis_data_flow(Fall2021)

Fall2021_b<- subset(Fall2021,  datetime< '2021-11-30 12:00:00')

vis_data_flow(Fall2021_b)

# write.csv(x = Fall2021_b, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/21_GBU_modelInputs.csv", row.names = TRUE)







### 2022
# ##
# 
# dat22<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBUInputs.csv")
# summary(dat22)
# str(dat22)
# 
# dat22 <- dat22 %>%
#   mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))%>%
#   select("datetime","do.obs", "wtr","par", "baro", "dischargeCFS", "gageHF")
# 
# 
# # get all units in model form #
# dat22$pressure_millibar <- c(dat22$baro * 0.01)
# dat22$dischargeR <- c(dat22$dischargeCFS *  35.3147)
# 
# ###
# ## Depth rating curve: 
# 
# date<- as.POSIXct(c("2022-07-12 13:00:00",
#                     "2022-08-09 13:00:00",
#                     "2022-08-20 18:00:00"), 
#                   tz="America/Los_Angeles",
#                   format = c("%Y-%m-%d %H:%M:%OS"))
# 
# depth <- c(0.136166667, 0.154806,0.1263684)
# flow <- c(48.68312,36.68312,28.68312)
# 
# mophDF <- data.frame(date,depth, flow)
# 
# DRC <- left_join(dat22, mophDF[c("date", "depth", "flow")],
#                  by = c("datetime" = "date"))
# summary(DRC)
# DRC$gageHm <-c(DRC$gageHF *0.3048)
# 
# # regression between reach depth and gage height
# depth.lm<- glm(depth~gageHm, data=DRC)
# summary(depth.lm)
# 
# flow.lm<- glm(flow~dischargeR, data=DRC)
# summary(flow.lm)
# #BWC_Q1$depth <- calc_depth(Q=u(BWC_Q1$discharge, "m^3 s^-1"), f=u(0.36))
# DRC$est.depth<- ((DRC$gageHm) * summary(depth.lm)$coef[2,1]) 
# hist(DRC$est.depth) 
# 
# dat22$depth <- ((dat22$gageHF * 0.3048) * summary(depth.lm)$coef[2,1])
# 
# DRC$est.flow<- ((DRC$dischargeR) * summary(flow.lm)$coef[2,1])
# hist(DRC$est.flow) 
# 
# dat22$discharge.est <- ((dat22$dischargeR) * summary(flow.lm)$coef[2,1])
# 
# 
# # calc light example
# latitude <- c(39.10740708)
# longitude <- c(-120.16213517)
# 
# dat22$solar.time <- calc_solar_time(dat22$datetime, longitude)
# 
# ## SM light ##
# dat22$light_cal<- calc_light(
#   dat22$solar.time,
#   latitude,
#   longitude,
#   max.PAR = u(2326, "umol m^-2 s^-1"),
#   attach.units = is.unitted(dat22$solar.time)
# )
# 
# hist(dat22$light_cal)
# 
# dat22$light_st <- c(dat22$par*2.114)
# hist(dat22$light_st)
# 
# dat22$DO.sat <- calc_DO_sat(dat22$wtr, 
#                             dat22$pressure_millibar,
#                             sal=0) 
# hist(dat22$DO.sat)
# 
# 
# # Get data in correct name and column form
# names(dat22)
# 
# colnames(dat22)[3] <- "temp.water"
# colnames(dat22)[2] <- "DO.obs"
# colnames(dat22)[14] <- "light"
# colnames(dat22)[11] <- "discharge"
# 
# 
# # New named df
# mdat22 <- subset(dat22, select= c(datetime, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(x = mdat22, file = "/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBUmodelInputs.csv", row.names = TRUE)


