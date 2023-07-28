library(ggpubr)

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



## SPC
GBL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/GBL_SPC.csv")
GBL22_SPC$solar.time <- as.POSIXct(GBL22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
GBL22_SPC$site<- "GBL"
GBL22_SPC$shore<- "east"




GBU22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/GBU_SPC.csv")
GBU22_SPC$solar.time <- as.POSIXct(GBU22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
GBU22_SPC$site<- "GBU"
GBU22_SPC$shore<- "east"

#GBU22_SPC <- subset(GBU22_SPC, datetime >="2021-04-22 00:00:00")

BWU22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/BWU_SPC.csv")
BWU22_SPC$solar.time <- as.POSIXct(BWU22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
BWU22_SPC$site<- "BWU"
BWU22_SPC$shore<- "west"

BWL22_SPC<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/BWL_SPC.csv")
BWL22_SPC$solar.time <- as.POSIXct(BWL22_SPC$datetime,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "UTC")
BWL22_SPC$site<- "BWL"
BWL22_SPC$shore<- "west"

SPCdat<-rbind(GBL22_SPC,GBU22_SPC,BWU22_SPC,BWL22_SPC)

SPCplot <- ggplot(SPCdat, aes(x=solar.time, y=SPC, color =(site))) +
  geom_point(size=0.25) +
  theme_classic() +
  labs(x ="Date", y = "SPC") +
  scale_color_manual(values=c("#F94144", "#F8961E",   # "#F9C74F"
                              "#90BE6D", "#43AA8B")) + # 33637d
  facet_grid(shore~.)


# ca <- cabbages %>%
#   group_by(Cult, Date) %>%
#   summarise(
#     Weight = mean(HeadWt),
#     sd = sd(HeadWt),
#     n = n(),
#     se = sd / sqrt(n)
#   )

SPCdat


str(SPCdat)

SPCdat <- SPCdat[-which(SPCdat$datetime >"2021-04-01 00:00:00" & SPCdat$datetime < "2021-04-02 00:00:00"),]
vis_data_flow(dat_adj)

SPCdat_q <- SPCdat %>% 
  mutate(date = as.Date(solar.time)) %>%
  group_by(date, site, shore) %>%
  summarise(
    SPC_m = mean(SPC, na.rm = T),
    SPC_sd = sd(SPC),
    SPC_n=length(SPC),
    SPC_se = SPC_sd / sqrt(SPC_n),
    wtr_m = mean(wtr, na.rm = TRUE),
    wtr_sd = sd(wtr, na.rm = TRUE),
    wtr_n=length(wtr),
    wtr_se = wtr_sd / sqrt(wtr_n)
    )

SPCdat_qn<- na.omit(SPCdat_q)

SPCplot <- ggplot(SPCdat_qn, aes(x=date, y=SPC_m, color =(site))) +
  geom_pointrange(aes(ymin=SPC_m-SPC_se, ymax=SPC_m+SPC_se))+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+ #new
  theme_classic() +
  labs(x ="Date", y = "SPC") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  theme_classic() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
        #axis.text.x=element_text(angle=60, hjust=1), # new
        legend.position = 'bottom', 
        legend.direction = "horizontal") +
  facet_grid(shore~.)

wtrplot <- ggplot(SPCdat_qn, aes(x=date, y=wtr_m, color =(site))) +
  geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")),
             color = "#4c4d4c") +
  geom_pointrange(aes(ymin=wtr_m-wtr_se, ymax=wtr_m+wtr_se))+
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")+ #new
  theme_classic() +
  labs(x ="Date", y = "Water temperature C") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  theme_classic() + 
  theme(axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x=element_text(angle=60, hjust=1), # new
    legend.position = 'bottom', 
    legend.direction = "horizontal") +
  facet_grid(shore~.)

# write.csv (x = all_dat, file = "R:/Users/kloria/Documents/MetabModels/ WQdata_DL.csv", row.names = TRUE)
library(ggpubr)

Up_lowGW_grid <- ggarrange(SPCplot,
                    wtrplot,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, 
                    widths = c(0.8,2.5),
                    heights = c(2.75, 4.5),
                    legend = "bottom")

# ggsave(plot = Up_lowGW_grid, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/Up_lowGW_grid.png",sep=""),width=8,height=8.5,dpi=300)


### SPC 
SPCdat_qnw <- SPCdat_qn %>%
  select(date,site, SPC)
SPCdat_qnw<- as.data.frame(SPCdat_qnw)
SPCdat_qnw2 <- reshape(data=SPCdat_qnw, idvar="date",
                       v.names = "SPC",
                       timevar = "site",
                       direction="wide")

SPCdat_qnw2$GB <- c(SPCdat_qnw2$SPC.GBL- SPCdat_qnw2$SPC.GBU)
SPCdat_qnw2$BW <- c(SPCdat_qnw2$SPC.BWL- SPCdat_qnw2$SPC.BWU)

mean(na.omit(SPCdat_qnw2$GB))
se(na.omit(SPCdat_qnw2$GB))
mean(na.omit(SPCdat_qnw2$BW))
mean(na.omit(SPCdat_qnw2$SPC.GBL))
mean(na.omit(SPCdat_qnw2$SPC.GBU))

se(na.omit(SPCdat_qnw2$BW))

## temp
Tempdat_qnw <- SPCdat_qn %>%
  select(date,site, wtr)
Tempdat_qnw<- as.data.frame(Tempdat_qnw)

Tempdat_qnw2 <- reshape(data=Tempdat_qnw, idvar="date",
                       v.names = "wtr",
                       timevar = "site",
                       direction="wide")

Tempdat_qnw2$GB <- c(Tempdat_qnw2$wtr.GBL- Tempdat_qnw2$wtr.GBU)
Tempdat_qnw2$BW <- c(Tempdat_qnw2$wtr.BWL- Tempdat_qnw2$wtr.BWU)

mean(na.omit(Tempdat_qnw2$GB))
se(na.omit(Tempdat_qnw2$GB))

mean(na.omit(Tempdat_qnw2$BW))
se(na.omit(Tempdat_qnw2$BW))


mean(na.omit(Tempdat_qnw2$wtr.GBL))
mean(na.omit(Tempdat_qnw2$wtr.GBU))
 


(mean(na.omit(Tempdat_qnw2$wtr.BWL)) - mean(na.omit(Tempdat_qnw2$wtr.BWU)))/ mean(na.omit(Tempdat_qnw2$wtr.BWL))

## More plots:

## nutrients:
# flow and AFDM
# flow and nutrients
# GPP and nutrients


Tempdat_qnw3 <- mutate(Tempdat_qnw2, select(wtr.BWL, wtr.BWL))










### ALL sites ###
## nutrients:
# flow and AFDM
# flow and nutrients
# GPP and nutrients

WQdat <- read.csv("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/WQdata_DL.csv")
WQdat <- WQdat %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d")) %>%
  subset(date>"2021-01-01")
WQdat$datetime<-(as.POSIXct(paste(WQdat$date, WQdat$time)))

str(WQdat)
unique(WQdat$site)
range(WQdat$date)

WQdat$site[WQdat$site == "Blackwood"] <- "Blackwood lower"

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


## Look at our NO3, NH4 and PO4 data
AQdat <- read.csv("/Users/kellyloria/Downloads/WaterChemistryRecord.csv")

AQdat2 <- AQdat %>%
  dplyr::rename(NO3="AQ400_NO2_NO3_H2O.in.mg.L",
                NH3="AQ400.NH4.mg.L",
                OrthoP_P="AQ400.o.Phosphate_water.ug.L",
                DOC="Shimadzu.DOC.mgL",
                site="Site")%>%
  mutate(date = as.Date((Date), format ="%m/%d/%y")) %>%
  select("site", "date", "NO3", "NH3", "OrthoP_P")

unique(AQdat2$site)

AQdat <- subset(AQdat2, 
                site== "Blackwood lower" | 
                  site== "Blackwood upper" |
                  site== "Glenbrook lower"|
                  site== "Glenbrook upper" |
                  site== "Third" |
                  site== "General" |
                  site== "Ward" |
                  site== "Incline")

str(AQdat)


## Look at our NO3, NH4 and PO4 data
AQdatPW1 <- read.csv("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/PoreWaterDf.csv")

names(AQdatPW1)
AQdatPW3 <- AQdatPW1 %>%
  dplyr::rename(NO3="NO2_NO3_H2O_mgL",
                NH3="NH4_mgL",
                OrthoP_P="PO4_ugL",
                site="site")%>%
  mutate(date = as.Date((date), format="%m/%d/%y")) %>%
  select("site", "date", "NO3", "NH3", "OrthoP_P")

unique(AQdatPW3$site)

AQdatPW2 <- subset(AQdatPW3, 
                site== "Blackwood lower" | 
                  site== "Blackwood upper" |
                  site== "Glenbrook lower"|
                  site== "Glenbrook upper")

str(AQdatPW2)

AQdatPW2$project <- "KAL_PW"
names(AQdat)

names(WQdat)
WQdat2 <- WQdat %>%
  select("site","date","NO3","NH3","OrthoP_P")
WQdat2$project <- "USGS"

WQdat2$OrthoP_P <- c(WQdat2$OrthoP_P*1000)
# WQdat

WC_dat <- rbind(AQdat,WQdat2,AQdatPW2)

WC_dat <- subset(WC_dat, NO3>0 & NH3<3 & OrthoP_P<40)
WC_dat_sh <- subset(WC_dat,
                    site== "Blackwood lower" | 
                      site== "Blackwood upper" |
                      site== "Glenbrook lower"|
                      site== "Glenbrook upper")

WC_dat_sh

WC_dat<- rbind(WC_dat_sh, AQdatPW2)
unique(WC_dat$site)

NO3plot_aq <- qplot(date, NO3, data = WC_dat, geom="point", color =site, shape=project,size=I(2)) +
  #geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) +
  labs(x ="Date", y = "NO3 (mg/L)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "2 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


NH3plot_aq <- qplot(date, NH3, data = WC_dat, geom="point", color =site, shape=project,size=I(2)) +
  #geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) +
  labs(x ="Date", y = "NH3 (mg/L)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "2 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


OPplot_aq <- qplot(date, OrthoP_P, data = WC_dat, geom="point", color =site, shape=project,size=I(2)) +
  #geom_line() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) +
  labs(x ="Date", y = "O-phos (ug/L)") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "2 month") + 
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

#write.csv (x = all_dat, file = "R:/Users/kloria/Documents/MetabModels/ WQdata_DL.csv", row.names = TRUE)

library(ggpubr)

## total grid:
P_grid <- ggarrange(NO3plot_aq,
                    NH3plot_aq,
                    OPplot_aq,
                    ncol = 1, nrow = 3,
                    common.legend = TRUE, 
                    widths = c(0.8,0.9, 1),
                    legend = "right")



# ggsave(plot = P_grid, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/Waterchem_grid.png",sep=""),width=7,height=7,dpi=300)


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


WC_datsum <- subset(WC_dat,
                    site== "Glenbrook lower" &
                    project == "KAL"
                      # site== "Blackwood upper" |
                      # site== "Glenbrook lower"|
                      # site== "Glenbrook upper"
                      )

mean(na.omit(WC_datsum$NH3))

WC_dat_sh




# Daily data
AFDM <- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22AFDM.csv")
AFDM_W <- AFDM %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d")) %>%
  subset(Site== "BWU" |Site== "BWL") 
AFDM_W$shore<- "west"

AFDM_E <- AFDM %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d")) %>%
  subset(Site== "GBU"|Site== "GBL") 
AFDM_E$shore <- "east"

AFDM_all <- rbind(AFDM_W,AFDM_E)

AFDMplot <- ggplot(AFDM_all, aes(x=date, y=log(AFDMmgcm2), color =(Site))) + 
  geom_point(size=1.25) + geom_line()+ 
  theme_classic() + 
  labs(x ="Date", y = "log(Rock scrape AFDM (mg/cm2))") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  scale_x_date(labels = date_format("%b-%y"),
                   date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(shore~.)
# ggsave(plot = AFDMplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/UPLOW_AFDMRS.png",sep=""),width=8,height=4,dpi=300)

AFDM_alls <- subset(AFDM_all, date>"2022-05-01")

SAFDMplot <- ggplot(AFDM_alls, aes(x=date, y=(AFDMmgmL), color =(Site))) + 
  geom_point(size=1.25) + geom_line()+ 
  theme_classic() + 
  labs(x ="Date", y = "Sediment AFDM (mg/mL)") +
  scale_color_manual(values=c("#3283a8", "#8dcdeb",   # "#F9C74F"
                              "#a67d17", "#e3c476")) + # 33637d
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(shore~.)

# ggsave(plot = SAFDMplot, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/UPLOW_AFDMsed.png",sep=""),width=6,height=4,dpi=300)

BWmetab<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/BW_daily.csv")

BWmetab <- BWmetab %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d")) 


BWMplotGPP <- ggplot(BWmetab, aes(x=date, y=GPP_mean)) + 
  geom_point(size=1.25,  color="#3283a8") + geom_line(color="#3283a8")+ 
  #geom_pointrange(aes(ymin=GPP_mean-GPP_se_mean, ymax=GPP_mean+GPP_se_mean))+
  labs(x ="Date", y = "GPP") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

BWMplotER <- ggplot(BWmetab, aes(x=date, y=ER_mean)) + 
  geom_point(size=1.25,  color="#3283a8") + geom_line(color="#3283a8")+ 
  #geom_pointrange(aes(ymin=GPP_mean-GPP_se_mean, ymax=GPP_mean+GPP_se_mean))+
  labs(x ="Date", y = "ER") +
  scale_x_date(labels = date_format("%b-%y"),
               date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


MetabBW22_grid <- ggarrange(BWMplotGPP,
                    BWMplotER,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, 
                    widths = c(1,1, 1),
                    legend = "bottom")

# ggsave(plot = MetabBW22_grid, filename = paste("/Users/kellyloria/Documents/UNR/Ncycle/MSM_ncycle/figures/MetabBW22_grid.png",sep=""),width=10,height=5,dpi=300)













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

