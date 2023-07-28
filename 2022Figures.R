# Do data for metabolism models:

BWU21_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/21_BWUmodelInputs.csv")
BWU21_do$solar.time <- as.POSIXct(BWU21_do$datetime,
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC")
BWU21_do$site<- "BWU"
BWU21_do$shore<- "west"
## Check for NAs in time series
which(is.na(BWU21_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(BWU21_do)
BWU21_do <- BWU21_do[,c("site", "shore", "solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]

BWU22_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_BWUmodelInputs.csv")
BWU22_do$solar.time <- as.POSIXct(BWU22_do$datetime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC")
BWU22_do$site<- "BWU"
BWU22_do$shore<- "west"
## Check for NAs in time series
which(is.na(BWU22_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(BWU22_do)
BWU22_do <- BWU22_do[,c("site", "shore","solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]


BWL22_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_BWLmodelInputs.csv")
BWL22_do$solar.time <- as.POSIXct(BWL22_do$datetime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC")
BWL22_do$site<- "BWL"
BWL22_do$shore<- "west"

## Check for NAs in time series
which(is.na(BWL22_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(BWL22_do)
BWL22_do <- BWL22_do[,c("site","shore", "solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]

## Rbind BW
BW <- rbind(BWL22_do, BWU22_do, BWU21_do)


GBL22_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBL_modelInputs.csv")
GBL22_do$solar.time <- as.POSIXct(GBL22_do$datetime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC")
GBL22_do$site<- "GBL"
GBL22_do$shore<- "east"
## Check for NAs in time series
which(is.na(GBL22_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(GBL22_do)
GBL22_do <- GBL22_do[,c("site","shore", "solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]


GBU22_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/22_GBUmodelInputs.csv")
GBU22_do$solar.time <- as.POSIXct(GBU22_do$datetime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC")
GBU22_do$site<- "GBU"
GBU22_do$shore<- "east"
## Check for NAs in time series
which(is.na(GBU22_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(GBU22_do)
GBU22_do <- GBU22_do[,c("site", "shore","solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]


GBU21_do <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/FinalInputs/21_GBU_modelInputs.csv")
GBU21_do$solar.time <- as.POSIXct(GBU21_do$datetime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC")
GBU21_do$site<- "GBU"
GBU21_do$shore<- "east"
## Check for NAs in time series
which(is.na(GBU21_do$solar.time)) 
## mle model needs: solar.time, DO.obs, DO.sat, depth, temp.water, light
names(GBU21_do)
GBU21_do <- GBU21_do[,c("site","shore", "solar.time","DO.obs","DO.sat","depth","temp.water","light","discharge")]

## Rbind BW
GB <- rbind(GBL22_do, GBU22_do, GBU21_do)

DOdat <- rbind(GB,BW)

DOplot <- ggplot(DOdat, aes(x=solar.time, y=DO.obs, color =(site))) + 
  geom_point(size=0.25) +  
  theme_classic() + 
  labs(x ="Date", y = "Dissloved oxygen (mg/L)") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  scale_x_datetime(labels = date_format("%b-%y"),
                   date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(shore~.)


WRTplot <- ggplot(DOdat, aes(x=solar.time, y=temp.water, color =(site))) + 
  geom_point(size=0.25) +  
  theme_classic() + 
  labs(x ="Date", y = "Water temperature (C)") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  scale_x_datetime(labels = date_format("%b-%y"),
                   date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(shore~.)
#ggsave(plot = WRTplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/UPLOW_wtmp.png",sep=""),width=8,height=4,dpi=300)


DOdatT<-rbind(GBL22_do,BWL22_do)
DOdatT <- DOdatT %>%subset(solar.time>"2021-09-29 23:00:00" & solar.time<"2022-10-01 00:00:00")


Wtrplot <- ggplot(DOdatT, aes(x=solar.time, y=temp.water , color =(site))) + 
  geom_point(size=0.25)  +
  scale_color_manual(values=alpha(c("#3283a8", "#a67d17"),0.25)) +
  scale_x_datetime(labels = date_format("%b-%y"),
                   date_breaks = "1 month") +
  labs(x ="Date", y = "Water Temp (C)") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave(plot = Wtrplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/Wtemp.png",sep=""),width=7,height=2,dpi=300)



Flowplot <- ggplot(DOdat, aes(x=solar.time, y=log(discharge) , color =(site))) + 
  geom_point(size=0.25) +  
  theme_classic() + 
  labs(x ="Date", y = "log(Streamflow (m3/s))") +
  scale_color_manual(values=c("#F94144", "#F8961E",   # "#F9C74F"
                              "#90BE6D", "#43AA8B")) + # 33637d
  facet_grid(shore~.)



# ## SPC
# GBL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBL_SPC.csv")
# GBL22_SPC$solar.time <- as.POSIXct(GBL22_SPC$datetime,
#                                    format = "%Y-%m-%d %H:%M:%S",
#                                    tz = "UTC")
# GBL22_SPC$site<- "GBL"
# GBL22_SPC$shore<- "east"
# 
# 
# GBU22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_GBU_SPC.csv")
# GBU22_SPC$solar.time <- as.POSIXct(GBU22_SPC$datetime,
#                                    format = "%Y-%m-%d %H:%M:%S",
#                                    tz = "UTC")
# GBU22_SPC$site<- "GBU"
# GBU22_SPC$shore<- "east"
# 
# 
# BWU22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWU_SPC.csv")
# BWU22_SPC$solar.time <- as.POSIXct(BWU22_SPC$datetime,
#                                    format = "%Y-%m-%d %H:%M:%S",
#                                    tz = "UTC")
# BWU22_SPC$site<- "BWU"
# BWU22_SPC$shore<- "west"
# 
# 
# BWU21_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/21_BWU_SPC.csv")
# BWU21_SPC$solar.time <- as.POSIXct(BWU21_SPC$datetime,
#                                    format = "%Y-%m-%d %H:%M:%S",
#                                    tz = "UTC")
# BWU21_SPC$site<- "BWU"
# BWU21_SPC$shore<- "west"
# 
# BWL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_BWL_SPC.csv")
# BWL22_SPC$solar.time <- as.POSIXct(BWL22_SPC$datetime,
#                                    format = "%Y-%m-%d %H:%M:%S",
#                                    tz = "UTC")
# BWL22_SPC$site<- "BWL"
# BWL22_SPC$shore<- "west"
# 
# SPCdat<-rbind(GBL22_SPC,GBU22_SPC,BWU22_SPC,BWU21_SPC,BWL22_SPC)
# 
# SPCplot <- ggplot(SPCdat, aes(x=solar.time, y=SPC, color =(site))) + 
#   geom_point(size=0.25) +  
#   theme_classic() + 
#   labs(x ="Date", y = "log(Streamflow (m3/s))") +
#   scale_color_manual(values=c("#F94144", "#F8961E",   # "#F9C74F"
#                               "#90BE6D", "#43AA8B")) + # 33637d
#   facet_grid(shore~.)
# 

## More plots:

## nutrients:
# flow and AFDM
# flow and nutrients
# GPP and nutrients













### ALL sites ###
## nutrients:
# flow and AFDM
# flow and nutrients
# GPP and nutrients

WQdat <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/WQdata_DL.csv")
WQdat <- WQdat %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d")) %>%
  subset(date>"2020-10-01" & date<"2021-09-30")
WQdat$datetime<-(as.POSIXct(paste(WQdat$date, WQdat$time)))

str(WQdat)
unique(WQdat$site)

Alldat_ts <- DOdat %>%
  full_join(WQdat)

# Nutrients plots:


TNplot <- qplot(date, TN, data = WQdat, geom="point", color =site, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

NH3plot <- qplot(date, NH3, data = WQdat, geom="point", color =site, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


NO3plot <- qplot(date, NO3, data = WQdat, geom="point", color =site, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

OPplot <- qplot(date, OrthoP_P, data = WQdat, geom="point", color =site, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))


TPplot <- qplot(date, TP, data = WQdat, geom="point", color =site, size=I(2)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))



OP4plot <- qplot(date, OrthoP_PO4, data = all_dat, geom="point", color =site, size=I(2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#90BE6D", "#43AA8B", "#33637d","#cfbd1d","#d6733a"),0.5)) +
  #scale_x_date(labels = date_format("%b-%d")) +
  theme_bw() + facet_grid(site~.) 


  
SSplot <- qplot(date, SuspSed_mgL, data = all_dat, geom="point", color =site, size=I(2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#90BE6D", "#43AA8B", "#33637d","#cfbd1d","#d6733a"),0.5)) +
  #scale_x_date(labels = date_format("%b-%d")) +
  theme_bw() + facet_grid(site~.) 

TSSplot <- qplot(date, TSS, data = all_dat, geom="point", color =site, size=I(2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#90BE6D", "#43AA8B", "#33637d","#cfbd1d","#d6733a"),0.5)) +
  #scale_x_date(labels = date_format("%b-%d")) +
  theme_bw() + facet_grid(site~.) 



#write.csv (x = all_dat, file = "R:/Users/kloria/Documents/MetabModels/ WQdata_DL.csv", row.names = TRUE)

library(ggpubr)

## total grid:
P_grid <- ggarrange(NO3plot,
                    NH3plot,
                    TPplot,
                    ncol = 1, nrow = 3,
                    common.legend = TRUE, 
                    widths = c(0.8,0.9, 1),
                    legend = "bottom")


DO_plot <- DOdat %>%subset(solar.time>"2021-09-29 23:00:00" & solar.time<"2022-10-01 00:00:00")


lightplot <- ggplot(DO_plot, aes(x=solar.time, y=light , color =(shore))) + 
  geom_point(size=0.25) +  
  theme_classic() + 
  labs(x ="Date", y = "PAR") +
  scale_color_manual(values=alpha(c("#a67d17","#3283a8"),0.25)) + # 33637d
  scale_x_datetime(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave(plot = lightplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/PAR.png",sep=""),width=7,height=2,dpi=300)


# Daily data
AFDM <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22AFDM.csv")
AFDM <- AFDM %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d"))
WQdat$datetime<-(as.POSIXct(paste(WQdat$date, WQdat$time)))
str(WQdat)


##

## FLOW
library(dataRetrieval)
## Add in flow data
siteNo <- "10336730"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

GBflow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

GBflow.ts <- GBflow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

GBflow.ts$Site<- "GB"
GBflow.ts$shore<-"east"

siteNo <- "10336676"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

flow <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

WDflow.ts <- flow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

WDflow.ts$Site<- "WD"
WDflow.ts$shore<-"west"

siteNo <- "10336660"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

flow <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)

BWflow.ts <- flow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

BWflow.ts$Site<- "BW"
BWflow.ts$shore<-"west"



siteNo <- "10336645"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

flow <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)

GNflow.ts <- flow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

GNflow.ts$Site<- "GN"
GNflow.ts$shore<-"west"


siteNo <- "10336700"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

flow <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)

INflow.ts <- flow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

INflow.ts$Site<- "IN"
INflow.ts$shore<-"east"



siteNo <- "10336698"
pCode <- c("00060", "00010")
start.date <- "2021-09-30"
end.date <- "2022-10-01"

flow <- readNWISuv(siteNumbers = siteNo,
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)

TDflow.ts <- flow %>% select("dateTime", "X_00060_00000") %>% 
  dplyr::rename(datetime = "dateTime", dischargeCFS = "X_00060_00000") %>%
  select("datetime", "dischargeCFS")

TDflow.ts$Site<- "TD"
TDflow.ts$shore<-"east"

flowdat<- rbind(TDflow.ts, INflow.ts, WDflow.ts, BWflow.ts, GNflow.ts, GBflow.ts)

FLowplot <- qplot(datetime, log(dischargeCFS), data = flowdat, geom="point", color =Site, size=I(2)) +
  geom_line() + ylim(0,7) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#3283a8", "#a67d17","#226b5b", "#a84e32","#b57633","#164778"),0.8)) +
  scale_x_datetime(labels = date_format("%b-%y"),
               date_breaks = "1 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(shore~.)

#ggsave(plot = FLowplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/Flow.png",sep=""),width=7,height=2,dpi=300)

