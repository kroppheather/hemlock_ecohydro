#######################################
## Analysis of Hemlock and Basswood  ##
## sap flux and canopy T with soil   ##
## moisture and weather              ##
#######################################

#### read in libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### set up directories ----
dirUser <- 2

dirScriptAll <- c("/Users/hkropp/Documents/GitHub/hemlock_ecohydro",
               "c:/Users/hkropp/Documents/GitHub/hemlock_ecohydro")

dirScript <- dirScriptAll[2]

#### sapflow data ----
## T.L.day: average daily transpiration by species in 
## liters per day per tree and per m2 of leaf area
## sapflow.hour: average hourly sap flow by species in
## L per hour per tree and per m2 of leaf area
## liters per day per tree and per m2 of leaf area
## Tot.tree.L.day: average daily transpiration by tree in
## liters per day per tree and per m2 of leaf area

source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))

#### explore basic data patterns  ----


sapAllt1 <- left_join(weatherHourly, sapflow.hour, by=c("doy","hour"="hour1"))
sapAll <- left_join(sapAllt1, soilHourly, by=c("doy","hour"))

T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")



ggplot(sapAll %>% filter(S_Rad > 0), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()
# check for hysteresis in hourly
ggplot(sapAll %>% filter(S_Rad > 0 & doy == 168), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sapAll %>% filter(S_Rad > 0 & doy == 173), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()+
  geom_path()


# evaluate degree of hysteresis
# ensure that all dates and times are present
# create data frame with all possible times and dates

# realized that I need to do this step then join weather so historical VPD can get included properly
Sap_hemlock <- sapAll %>%
  filter(species == "hemlock")

dateAll <- data.frame( doy = rep(seq(160,151), each=24),
                       hour = rep(seq(0,23), times=length(seq(160,151))))

sapDates <- full_join(dateAll, sapAll, by=c("doy","hour","species"))



# daily patterns in data



ggplot(Tc.L.day, aes(maxVPD, L.day, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(maxVPD, L.day.m2, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(SWC, L.day, color=species))+
  geom_point()

ggplot(Tc.L.day, aes(AirT, L.day, color=species))+
  geom_point()


ggplot(Tc.L.day, aes(max_SW, L.day, color=species))+
  geom_point()
