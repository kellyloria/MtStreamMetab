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
  scale_color_manual(values=c("#F94144", "#F8961E",   # "#F9C74F"
                              "#90BE6D", "#43AA8B")) + # 33637d
  facet_grid(shore~.)


Wtrplot <- ggplot(DOdat, aes(x=solar.time, y=temp.water , color =(site))) + 
  geom_point(size=0.25) +  
  theme_classic() + 
  labs(x ="Date", y = "Water Temp (C)") +
  scale_color_manual(values=c("#F94144", "#F8961E",   # "#F9C74F"
                              "#90BE6D", "#43AA8B")) + # 33637d
  facet_grid(shore~.)

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
