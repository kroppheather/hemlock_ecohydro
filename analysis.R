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
corTest <- left_join(corTest, weatherDaily)

plot(corTest$SWC, corTest$weekPr)
cor(corTest$SWC, corTest$weekPr)
plot(corTest$s_temp, corTest$weekPr)
cor(corTest$s_temp, corTest$weekPr)
plot(corTest$s_temp, corTest$SWC)
cor(corTest$s_temp, corTest$SWC)
plot(corTest$aveVPD, corTest$max_SW)
cor(corTest$aveVPD, corTest$max_SW)
plot(corTest$aveVPD, corTest$ave_SW)
cor(corTest$aveVPD, corTest$ave_SW)
plot(corTest$aveVPD, corTest$ave_SW)
cor(corTest$aveVPD, corTest$ave_SW)

T_L_day <- left_join(Tc.L.day, dailyPrecip, by="doy")
# note that L per day works out to mm/day 
ggplot(T_L_day, aes(doy, El_day, color=species))+
         geom_point()+
  geom_line()


ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day %>% filter(species == "hemlock"&aveVPD >0.6), aes(aveVPD, El_day))+
  geom_point()

ggplot(T_L_day %>% filter(species == "basswood"&aveVPD >0.6), aes(aveVPD, El_day))+
  geom_point()
HemL_day <- T_L_day %>% filter(species == "hemlock")
BassL_day <- T_L_day %>% filter(species == "basswood")

ggplot(T_L_day %>% filter(species == "hemlock"&aveVPD >0.6), aes(minAirT, El_day))+
  geom_point()

ggplot(T_L_day %>% filter(species == "basswood"&aveVPD >0.6), aes(minAirT, El_day))+
  geom_point()

VPDmodH <- lm(HemL_day$El_day ~ HemL_day$aveVPD)
summary(VPDmodH)
VPDmodB <- lm(BassL_day$El_day ~ BassL_day$aveVPD)
summary(VPDmodB)


ggplot(T_L_day, aes(aveVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(ave_SW, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(weekPr, El_day, color=species))+
  geom_point()


ggplot(T_L_day, aes(SWC, El_day, color=species))+
  geom_point()

ggplot(T_L_day %>% filter(species == "hemlock"&aveVPD >0.6), aes(SWC, El_day, color=species))+
  geom_point()

ggplot(T_L_day %>% filter(species == "basswood"&aveVPD >0.6), aes(SWC, El_day, color=species))+
  geom_point()




modSWH <- lm(HemL_day$El_day ~ HemL_day$SWC)
summary(modSWH)

residSWH <- residuals(modSWH)

plot(HemL_day$SWC, residSWH)

qqnorm(residSWH)
qqline(residSWH)


ggplot(T_L_day %>% filter(aveVPD >0.6), aes(doy, El_day, color=species))+
  geom_point()+geom_line()


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

