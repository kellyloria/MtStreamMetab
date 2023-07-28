

SAFDM<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/AFDM_raw/SoilAFDM.csv")
names(SAFDM)

Soil_dat_l <- SAFDM %>%
  mutate(date = as.Date((Date), format ="%Y-%m-%d"))%>%
  mutate(AFDMmgmL= as.numeric(AFDM..mg.mL.)) %>%
  select("Site", "date", "AFDMmgmL", "shore", "Type")%>%
  subset(Site=="BWNS1" | Site=="BWNS2" | Site=="BWNS3" | #Site=="BW10m" | 
           #Site=="BWL" | Site=="BWU" | Site=="GBL" | Site=="GBU" | 
           #Site=="BW15m" | Site=="BW20m"| 
           Site=="BWO"| Site=="BW0.5m"| 
           Site=="GBNS1" | Site=="GBNS2" | Site=="GBNS3" | 
           #Site=="GB10m" | Site=="GB15m" | Site=="GB20m"| 
           Site=="GBO"| Site=="GB0.5m")

Soil_dat_s <- SAFDM %>%
  mutate(date = as.Date((Date), format ="%Y-%m-%d"))%>%
  mutate(AFDMmgmL= as.numeric(AFDM..mg.mL.)) %>%
  select("Site", "date", "AFDMmgmL")%>%
  subset(Site=="BWL" | Site=="BWU" | Site=="GBL" | Site=="GBU" | 
           Site=="Third" | Site=="General"| Site=="Ward" | Site=="Incline")


unique(Soil_dat$Site)

Soil_lp <- ggplot(Soil_dat_l, aes(x=date, y=log(AFDMmgmL), color =(shore), shape=(Type))) + 
  geom_point(size=1.25) +
theme_classic() +
  scale_color_manual(values=c("#F94144", 
                              "#90BE6D"))

Soil_lp <- ggplot(Soil_dat_l, aes(x=date, y=log(AFDMmgmL), color =(shore), shape=(sitelab))) + 
  geom_point(size=1.25) +
  theme_classic() +
  scale_color_manual(values=c("#F94144", 
                              "#90BE6D"))



Soil_p <- ggplot(Soil_dat, aes(x=date, y=AFDMmgmL, color =(Site))) + 
  geom_point(size=1.25) + geom_line() 
  theme_classic() + 
  labs(x ="date", y = "Dissloved oxygen (mg/L)") +
  scale_color_manual(values=c("#F94144", "#F8961E","#F9C74F", 
                              "#90BE6D", "#43AA8B", "#33637d")) + facet_wrap(~Site)
  

RSAFDM<- read.csv("/Users/kellyloria/Documents/UNR/MSMmetab/AFDM_raw/RockscrapeAFMD.csv")
names(RSAFDM)
  
RS_dat <- RSAFDM %>%
    mutate(date = as.Date((date), format ="%Y-%m-%d"))%>%
    mutate(AFDMmgcm2 =as.numeric(AFDM..mg.cm..2.)) %>%
    select("Site", "date", "AFDMmgcm2")%>%
    subset(Site=="BWL" | Site=="BWU" | Site=="GBL" | Site=="GBU" | 
             Site=="Third" | Site=="General"| Site=="Ward" | Site=="Incline")

RS_dat_ch2 <- RSAFDM %>%
  mutate(date = as.Date((date), format ="%Y-%m-%d"))%>%
  mutate(AFDMmgcm2 =as.numeric(AFDM..mg.cm..2.)) %>%
  select("Site", "date", "AFDMmgcm2")%>%
  subset(Site=="BWL" | Site=="BWU" | Site=="GBL" | Site=="GBU" | 
           Site=="BWO" | Site=="GBO"| Site=="BW0.5m" | Site=="GB0.5m")

  
str(RS_dat)

RS_plot <- ggplot(RS_dat_ch2, aes(x=date, y=AFDMmgcm2, color =(Site))) + 
  geom_point(size=1.25) + #geom_line() 
theme_classic()  + facet_wrap(~Site)
  labs(x ="Date", y = "Dissloved oxygen (mg/L)") +
  scale_color_manual(values=c("#F94144", "#F8961E","#F9C74F", 
                              "#90BE6D", "#43AA8B", "#33637d")) + facet_wrap(~Site)


AFDM<- RS_dat%>%
full_join(Soil_dat)
 
  
AFDM_plot <- ggplot(AFDM, aes(x=AFDMmgmL, y=log(AFDMmgcm2), color =(Site))) + 
  geom_point(size=1.25) +
theme_classic() + 
  labs(x ="soil AFDM", y = "RS AFDM") 

#write.csv(x = Soil_dat_l, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22_lake_AFDM.csv", row.names = TRUE)


#write.csv(x = AFDM, file = "/Users/kellyloria/Documents/UNR/MSMmetab/CleanDat/22AFDM.csv", row.names = TRUE)


  