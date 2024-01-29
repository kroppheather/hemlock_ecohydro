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


source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))


#### explore basic data patterns  ----
TreeInfo <- unique(data.frame(species=sensors$Tree.Type,
                  DBH = sensors$DBH..cm., 
                  LA.m2 = sensors$LA.m2))

# daily data
T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")

# join in some daily weather stats for quality filters
dailyPrecip <- data.frame( doy = weatherDaily$doy,
                           dayPrec = weatherDaily$Prec)
weeklyPrecip <- rep(NA,6)
for(i in 7:nrow(dailyPrecip)){
  weeklyPrecip[i] <- sum(dailyPrecip$dayPrec[(i-6):i])
}

dailyPrecip$weekPr <- weeklyPrecip

corTest <- na.omit(left_join(soilDaily,dailyPrecip, by="doy"))

plot(corTest$SWC, corTest$weekPr)
cor(corTest$SWC, corTest$weekPr)
plot(corTest$s_temp, corTest$weekPr)
cor(corTest$s_temp, corTest$weekPr)
plot(corTest$s_temp, corTest$SWC)
cor(corTest$s_temp, corTest$SWC)



T_L_day <- left_join(Tc.L.day, dailyPrecip, by="doy")
# note that L per day works out to mm/day 
ggplot(T_L_day, aes(doy, El_day, color=species))+
         geom_point()+
  geom_line()


ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day %>% filter(species == "hemlock"), aes(aveVPD, El_day))+
  geom_point()

ggplot(T_L_day %>% filter(species == "basswood"), aes(aveVPD, El_day))+
  geom_point()
HemL_day <- T_L_day %>% filter(species == "hemlock")
BassL_day <- T_L_day %>% filter(species == "basswood")

VPDmodH <- lm(HemL_day$El_day ~ HemL_day$aveVPD)
summary(VPDmodH)
VPDmodB <- lm(BassL_day$El_day ~ BassL_day$aveVPD)
summary(VPDmodB)


ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(maxVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(weekPr, El_day, color=species))+
  geom_point()


ggplot(T_L_day, aes(SWC, weekPr))+
  geom_point()

ggplot(T_L_day, aes(SWC, El_day, color=species))+
  geom_point()


ggplot(T_L_day, aes(s_temp, El_day, color=species))+
  geom_point()


# look at patterns in sap flow
sapflow.hour$Js

ggplot(sapflow.hour%>%filter(doy==170), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapflow.hour%>%filter(doy==171), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapflow.hour%>%filter(doy==172), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapflow.hour%>%filter(doy==174), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()


ggplot(sapflow.hour%>%filter(doy==175), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()


ggplot(sapflow.hour%>%filter(doy==226), aes(hour1, Js, color=species))+
  geom_point()+
  geom_line()

