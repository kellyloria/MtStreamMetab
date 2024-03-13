# single stage
# 2024-03-12

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
# PC path  setwd("R:/Users/kloria/Documents/2023_StreamMetab")

site <- "GBL"
dat <- readRDS("./FinalInputs/24_GBL_modelInputs.rds")
summary(dat)

## Check for NAs in time series
which(is.na(dat$solar.time)) 

## Compile Data
metab_inputs('bayes', 'data')

## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(dat)
dat <- dat[,c("solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]

# originally for the 18th 

short <- subset(dat, solar.time >= as.POSIXct("2023-04-29 17:00:00") & solar.time <= as.POSIXct("2023-09-01 00:00:00"))

# Check timestamp format with light or temp
qplot(solar.time,  light, data = short, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_x_datetime(labels = date_format("%m/%d %H:%M"),
                   breaks = date_breaks("4 hours"))


#####################################################
#Visualize the data    
#####################################################
# dat <- short

# dat <- dat2

dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  dplyr::select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='Q\n(cms)')
dat %>% unitted::v() %>%
  dplyr::select(solar.time, depth, temp.water, light, discharge) %>%
  gather(type, value, depth, temp.water, light, discharge) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

str(dat)

## Try to get even data:
dat2 <- dat %>%
  mutate(date_only = as.Date(solar.time)) %>%
  group_by(date_only) %>%
  filter(n() == 96) %>%  # Assuming 96 observations per day for 15-minute intervals
  select(-date_only)

# #####################################################
# ## Model the data 
# #####################################################
# # Function to aggregate data to 15-minute intervals
# aggregate_to_15_minutes <- function(data) {
#   # Convert the 'solar.time' column to a POSIXct format
#   data$solar.time <- as.POSIXct(data$solar.time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#   
#   # Create a new column for 15-minute intervals
#   data$solar.time <- cut(data$solar.time, breaks = "15 min")
#   
#   # Group by the 'interval' column and aggregate data within each interval
#   aggregated_data <- data %>%
#     group_by(solar.time) %>%
#     summarize_at(vars(DO.obs, DO.sat, depth, temp.water, light, discharge), mean, na.rm = TRUE)
#   # Convert 'interval' back to a POSIXct timestamp
#   aggregated_data$solar.time <- as.POSIXct(aggregated_data$solar.time, origin = "1970-01-01", tz = "UTC")
#   
#   
#   return(aggregated_data)
# }
# 
# aggregated_dat <- aggregate_to_15_minutes(dat)

#####################################################
## Model the data 
#####################################################
## Figure out range of log of daily discharge and reset parameters accordingly

## Set bayes specs
bayes_name_new <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_new <- specs(bayes_name_new)
## Based on range of log daily Q (readjust based on range of your discharge, but then remember to reset number of nodes_meanlog and sdlog)
mean(log(dat2$discharge))
range(log(dat2$discharge))
hist(log(dat2$discharge))
sd(log(dat2$discharge))


bayes_specs_new$K600_lnQ_nodes_centers <- c(-7, -6, -5, -4, -3, -2, -1, 0)
## Based on Pete Raymond's data for small headwater streams
## (might leave at default values but make sure to adjust number of nodes)
bayes_specs_new$K600_lnQ_nodes_meanlog <- c(rep(1.4, 6))
#bayes_specs_new$K600_lnQ_nodes_sdlog <- c(rep(1.6, 6))
## Change sigma if need to constrain var
bayes_specs_new$K600_daily_sigma_sigma <- 0.05
bayes_specs_new$GPP_daily_lower <- c(0)
bayes_specs_new$ER_daily_upper <- c(0)

# added stuff
bayes_specs_new$n_chains <- c(6)
bayes_specs_new$n_cores <- c(6)
bayes_specs_new$burnin_steps <- c(2500)
bayes_specs_new$saved_steps <- c(2500)


#summary(aggregated_dat)
## Run streamMetabolizer
#aggregated_dat<- na.omit(aggregated_dat)

# # 
# dat1 <- dat %>%
# filter(solar.time < as.POSIXct("2021-11-05 00:00:00") | solar.time > as.POSIXct("2021-11-07 00:00:00"))
# 
# dat2 <- dat1 %>%
#   filter(solar.time < as.POSIXct("2022-11-05 00:00:00") | solar.time > as.POSIXct("2022-11-07 00:00:00"))
# #
# dat3 <- dat2 %>%
#   filter(solar.time < as.POSIXct("2023-11-03 00:00:00") | solar.time > as.POSIXct("2023-11-05 00:00:00"))
# #
# dat4 <- dat3 %>%
#   filter(solar.time < as.POSIXct("2021-08-05 00:00:00") | solar.time > as.POSIXct("2021-09-15 00:00:00"))
# 
# dat4 <- aggregated_dat %>%
#   filter(solar.time > as.POSIXct("2021-05-10 00:00:00"))

# run model 
dat3 <-  dat2[,c(-1)]
dat_metab_GB <- metab(bayes_specs_new, data=dat3)
dat_fit_GB <- get_fit(dat_metab_GB)

## Visualize
DOplot <-plot_DO_preds(predict_DO(dat_metab_GB))
# ggsave(plot = DOplot, filename = paste("./MtStreamMetab/Figures/diagnostic/plot_DO_preds_GBL2.png",sep=""),width=5,height=4,dpi=300)

metabplot<- plot_metab_preds(predict_metab(dat_metab_GB))
# ggsave(plot = metabplot, filename = paste("./MtStreamMetab/Figures/diagnostic/metabplot_GBL2.png",sep=""),width=5,height=4,dpi=300)


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

binplot<- Binning(dat_fit_GB, dat_metab_GB)
# ggsave(plot = binplot, filename = paste("./MtStreamMetab/Figures/diagnostic/binplot_GBL2.png",sep=""),width=5,height=4,dpi=300)

get_fit(dat_metab_GB)$overall %>%
  dplyr::select(ends_with('Rhat')) # might be best rhat 
# 1.75


## Save info
getwd()

## Save info
writefiles <- function(data, data2, path = "./24_output/") {
  for (i in seq_along(data)) {
    filename = paste(path,site,"_",names(data)[i], "_GBL2.csv", sep = "")
    write.csv(data[[i]], filename)
  }
  
  write.csv(unlist(get_specs(data2)), paste(path,site,"_","specs_GBL2.csv", sep = ""))
  write.csv(get_data_daily(data2), paste(path,site,"_","datadaily_GBL2.csv", sep = ""))
  write.csv(get_data(data2), paste(path,site,"_","mod_and_obs_DO_GBL2.csv", sep = ""))
}

## Create new folder for site and write csv info
writefiles(dat_fit_GB, dat_metab_GB)

# plot for k600
GBL <- read.csv("./24_output/GBL_daily_GBL2.csv")
GBL$date <- as.Date(GBL$date, origin="2021-01-01")
GBL$site <- "GBL"
GBL$shore <- "east"

Odd_plot <- ggplot(GBL, aes(x = K600_daily_mean, color = shore, fill = shore)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = alpha(c("#a67d17"), 0.2)) +
  scale_color_manual(values = alpha(c("#a67d17"), 0.9)) + theme_bw() + 
  geom_vline(data = GBL, aes(xintercept = mean(na.omit(K600_daily_mean))), linetype = "dashed") 

Gplot_sp <- ggplot(GBL, aes(x = K600_daily_mean, y = (ER_mean*-1))) +  
  geom_point(shape= 17, col = alpha(c("#a67d17"),0.5)) +
  geom_smooth(method ="lm", se=F)+  facet_grid(.~site)+
  theme_bw()

GB_lm <- lm(K600_daily_mean~(ER_mean*-1), data=GBL)
summary(GB_lm)

# Extract R-squared value
r_gsquared <- summary(GB_lm)$r.squared
GB_plot <- Gplot_sp + 
  geom_text(aes(x = max((GBL$K600_daily_mean), na.rm=T), y = min((GBL$ER_mean*-1), na.rm=T), 
                label = paste("R-squared =", round(r_gsquared, 2))),
            hjust = 1, vjust = 0, size = 4, col = "blue",
            parse = T, check_overlap = T, na.rm = T)
library(ggpubr)

k_grid <- ggarrange(GB_plot,
                    Odd_plot,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, 
                    legend = "bottom",
                    widths = c(0.6, 0.4))

#ggsave(plot = k_grid, filename = paste("./figures/24_streamMetab_k_GBL2.png",sep=""),width=6,height=3,dpi=300)
