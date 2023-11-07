#######################################
## Analysis of Hemlock and Basswood  ##
## sap flux and canopy T with soil   ##
## moisture and weather              ##
#######################################

#### read in libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)
library(rjags)
library(coda)
library(mcmcplots)

#### set up directories ----
dirUser <- 2

dirScriptAll <- c("/Users/hkropp/Documents/GitHub/hemlock_ecohydro",
               "c:/Users/hkropp/Documents/GitHub/hemlock_ecohydro")

dirScript <- dirScriptAll[2]

#### sapflow data ----


source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))

#### calculate gc ---



#### explore basic data patterns  ----
# daily data
T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")

### hourly data
Sap_hemlock <- sapflow.hour %>%
  filter(species == "hemlock")
Sap_basswood <- sapflow.hour %>%
  filter(species == "basswood")

dateAll <- data.frame( doy = rep(seq(160,151), each=24),
                       hour = rep(seq(0,23), times=length(seq(160,151))))

hemDateFill <- full_join(dateAll, Sap_hemlock, by=c("doy","hour"="hour1"))
bassDateFill <- full_join(dateAll, Sap_basswood, by=c("doy","hour"="hour1"))
# join weather
bassWeather <- full_join(weatherHourly, bassDateFill, by=c("doy","hour"))
hemWeather <- full_join(weatherHourly, hemDateFill, by=c("doy","hour"))


sap_all <- rbind(bassWeather, hemWeather)

# join in some daily weather stats for quality filters
dailyPrecip <- data.frame( doy = weatherDaily$doy,
                           dayPrec = weatherDaily$Prec)
weeklyPrecip <- rep(NA,6)
for(i in 7:nrow(dailyPrecip)){
  weeklyPrecip[i] <- sum(dailyPrecip$dayPrec[(i-6):i])
}

dailyPrecip$weekPr <- weeklyPrecip


sap_all <- left_join(sap_all, dailyPrecip, by="doy")

# canopy conductance from sap flow is most reliable 
# during the day and when VPD is greater than 0.6 according to Ewers and Oren
sap_analysis <- sap_all %>%
  filter(dayPrec <= 4) %>% # only take days with trace precip amounts less than or equal to 4 mm
  filter(hour >= 7 & hour <= 18 ) %>%
  filter(VPD_hr > 0.6)%>%
  filter(is.na(species) != TRUE)

dailyAllt1 <- left_join(weatherDaily, dailyPrecip[,c(1,3)], by="doy")
dailyAll <- left_join(dailyAllt1, soilDaily, by=c("doy", "year"))

# get the number of observations in a day

sap_count <- sap_analysis %>%
  filter(is.na(species ) == FALSE) %>%
  group_by(doy,species) %>%
  summarise(ncount = n()) %>%
  filter(ncount >= 6)

sap_model <- inner_join(sap_analysis, sap_count, by=c("doy","species"))


# need to set up species ID, and specDay table with weather data


specDayt <- data.frame(doy=sap_count$doy,
                             species=sap_count$species)
specDay <- left_join(specDayt, dailyAll, by="doy")
specDay$specID <- ifelse(specDay$species == "hemlock", 1,
                  ifelse(specDay$species == "basswood",2,NA))


# calculate canopy conductance from sap flow
# from Ewers and Oren 2000
#Gs=Kg*El/D from Ewers and Oren 2000 in m/s
#function for conductance coefficient (kPa m3 kg-1)

Kg.coeff<-function(T){115.8+(.423*T)}

#convert El should be in kg m-2 s-1 

Gs.convert<-function(Kg,El,D){((Kg*El)/D)}

sap_model$Kg <- Kg.coeff(sap_model$Air_temp)
sap_model$GSc <- Gs.convert(sap_model$Kg,
                            sap_model$El,
                            sap_model$VPD_hr)
#calculate Gs and convert to cm/s from m/s
sap_model$Gc_cm <- sap_model$GSc*100
#convert cm/s to mmol m-2 s using the equation from Pearcy et al
# double check this isn't moles
unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}

sap_model$gc_mmol_m2_s <- unit.conv(sap_model$Gc_cm,
                                    sap_model$Air_temp,
                                    sap_model$AtmosPr)

sap_model$gc_mmol_m2_s
ggplot(sap_model, aes(DD, gc_mmol_m2_s*1000, color=species))+
         geom_point()
####### Model run -----

# data
datalist <- list(Nobs = nrow(sap_analysis),
                 gs = )
                  