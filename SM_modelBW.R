# single stage
# 2021-10-22

## ---------------------------
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
library(scales)
library(StanHeaders)

#  install.packages("remotes")
#  remotes::install_github("appling/unitted")

#remotes::install_github("USGS-R/streamMetabolizer", force = TRUE)

## ---------------------------
## Read in DO data from miniDOT deployments 03/25-10/01
##

dat <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_BWLmodelInputs.csv")
summary(dat)


dat$solar.time <- as.POSIXct(dat$datetime,
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC")

## Check for NAs in time series
which(is.na(dat$solar.time)) 

## Compile Data
metab_inputs('bayes', 'data')

## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(dat)
dat <- dat[,c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]

# originally for the 18th 

# 
short <- subset(dat, solar.time > "2021-06-22 00:00:00" & solar.time < "2021-06-25 20:00:00")


# Check timestamp format with light or temp
qplot(solar.time,  temp.water, data = short, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(labels = date_format("%m/%d %H:%M"),
                   breaks = date_breaks("12 hours"))


#####################################################
#Visualize the data    
#####################################################
dat <- short

dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='Q\n(cms)')
dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


#####################################################
## Model the data 
#####################################################
## Figure out range of log of daily discharge and reset parameters accordingly

## Set bayes specs
bayes_name_new <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_new <- specs(bayes_name_new)
## Based on range of log daily Q (readjust based on range of your discharge, but then remember to reset number of nodes_meanlog and sdlog)
mean(log(dat$discharge))
range(log(dat$discharge))
hist(log(dat$discharge))
sd(log(dat$discharge))


#bayes_specs_new$K600_lnQ_nodes_centers <- c(-0.8, 0, 0.4, 0.9, 1.9, 2.8, 3.5, 4)
## Based on Pete Raymond's data for small headwater streams
## (might leave at default values but make sure to adjust number of nodes)
#bayes_specs_new$K600_lnQ_nodes_meanlog <- c(rep(1.5, 6))
#bayes_specs_new$K600_lnQ_nodes_sdlog <- c(rep(1.6, 6))
## Change sigma if need to constrain var
#bayes_specs_new$K600_daily_sigma_sigma <- 0.05

dat <- dat[!duplicated(dat[c('solar.time')]),]



## Run streamMetabolizer
## Change object name to avoid overwriting


dat_metab_BW <- metab(bayes_specs_new, data=dat)
dat_fit_BW <- get_fit(dat_metab_BW)

## Visualize
plot_DO_preds(predict_DO(dat_metab_BW))
plot_metab_preds(predict_metab(dat_metab_BW))

## Check binning
Binning <- function(fit_Site, Site){
  SM_output <- fit_Site$daily
  SM_day <- get_data_daily(Site)
  SM_KQbin <- fit_Site$KQ_binned
  SM_specs <- get_specs(Site)
  
  day <- data.frame(SM_day$discharge.daily, SM_output$K600_daily_50pct, rep('daily', dim(SM_output)[1]))
  colnames(day)<-c('Q', 'K600', 'Group')
  
  nodes<-data.frame(exp(as.numeric(as.character(SM_specs$K600_lnQ_nodes_centers))), exp(SM_KQbin$lnK600_lnQ_nodes_50pct), rep('node', dim(SM_KQbin)[1]))
  colnames(nodes)<-c('Q', 'K600', 'Group')
  KQ_plot<-rbind(day,nodes)
  
  ggplot(data=KQ_plot, aes(x=log(Q), y=K600, group=Group, colour=Group)) + 
    geom_point(size=3) +
    #geom_line() + 
    scale_color_manual(name="K-Q",
                       breaks = c("daily", "node"),
                       values=c("grey", "purple"),
                       labels=c("Daily","Bin")) +
    ylab("K600") +
    xlab("logQ") +
    theme_bw() +
    theme(legend.position = "top")
}

Binning(dat_fit_BW, dat_metab_BW)


get_fit(dat_metab_BW)$overall %>%
  select(ends_with('Rhat')) # might be best rhat 


## Save info
writefiles <- function(data, data2){
  for (i in seq_along(data)) {
    filename = paste(names(data)[i], ".csv")
    write.csv(data[[i]], filename)
  }
  write.csv(unlist(get_specs(data2)),"specs.csv")
  write.csv(get_data_daily(data2), "datadaily.csv")
  write.csv(get_data(data2),"mod_and_obs_DO.csv")
}


getwd()
## Create new folder for site and write csv info
writefiles(dat_fit_BW, dat_metab_BW)


#saveRDS(k, file = "GEN_PreClean.rds")
