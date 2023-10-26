#######################################
## Figures for Hemlock and Basswood  ##
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

dirFig <- ("E:/Google Drive/research/projects/kirkland_ecohydro/manuscript/figures")

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

#### organize data  ----
# daily data
T.L.dayW <- left_join(weatherDaily, T.L.day, by=c("doy"))
Tc.L.day <- left_join(T.L.dayW , soilDaily, by="doy")
Tc.L.day$se.L.day <- Tc.L.day$sd.Lday/sqrt(Tc.L.day$n.plant)
Tc.L.day$se.L.m2.day <- Tc.L.day$sd.day.m2/sqrt(Tc.L.day$n.plant)
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

sap_analysis <- sap_all %>%
  filter(dayPrec <= 4) %>% # only take days with trace precip amounts less than or equal to 4 mm
  filter(hour >= 7 & hour <= 18 ) %>%
  filter(VPD_hr > 0.25)

dailyAllt1 <- left_join(weatherDaily, dailyPrecip[,c(1,3)], by="doy")
dailyAll <- left_join(dailyAllt1, soilDaily, by=c("doy", "year"))
dailyAll$SWC_se <- dailyAll$sd_SWC/sqrt(dailyAll$n_SWC)
# get the number of observations in a day
sap_count <- sap_analysis %>%
  filter(is.na(species ) == FALSE) %>%
  group_by(doy,species) %>%
  summarise(ncount = n()) %>%
  filter(ncount >= 6)


bassDay <- Tc.L.day %>%
  filter(species == "basswood")

hemDay <- Tc.L.day %>%
  filter(species == "hemlock")

########### Figure 1 -----------
basscol <- "#4091E5"
basscolt <- "#4091E599"
hemcol <- "#3a5a40"
hemcolt <- "#3a5a4099"

hd <- 2.5
wd <- 4
xl <- 168
xh <- 250
yl1 <- 0
yh1 <- 0.5

yl2 <- 0
yh2 <- 2.5

yl3 <- 0
yh3 <- 36

yl4 <- 0
yh4 <- 0.2



precipScale <- yh1/ceiling(max(dailyAll$Prec))
test <- dailyAll$Prec* precipScale


png(paste0(dirFig, "/fig_1_met_t.png"), width=9, height=6,
    units="in", res=300 )


layout(matrix(seq(1,4),ncol=2, byrow=TRUE), width=lcm(rep(wd*2.54,2)),height=lcm(c(hd)*2.54))

par(mai=c(0.5,0.5,0.5,0.5))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xaxs="i", yaxs="i", xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl1,yh1))
for(i in 1:nrow(dailyAll)){
  polygon(c(dailyAll$doy[i]-0.25,dailyAll$doy[i]-0.25,
            dailyAll$doy[i]+0.25,dailyAll$doy[i]+0.25),
          c(0,dailyAll$Prec[i],dailyAll$Prec[i],0)*precipScale,
          border=NA, col="#C6DAF4")
}
points(dailyAll$doy, dailyAll$SWC, type="l") 



par(mai=c(0.5,0.5,0.5,0.5))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xaxs="i", yaxs="i", xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl2,yh2))

points(dailyAll$doy, dailyAll$maxVPD, type="l")      

par(mai=c(0.5,0.5,0.5,0.5))

plot(c(0,1),c(0,1), type="n", axes=FALSE, xaxs="i", yaxs="i", xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl3,yh3))

points(bassDay$doy, bassDay$L.day, type="b",pch=19, col=basscol)
arrows(bassDay$doy,bassDay$L.day+bassDay$se.L.day,
       bassDay$doy,bassDay$L.day-bassDay$se.L.day,code=0)
points(hemDay$doy, hemDay$L.day, type="b",pch=19, col=hemcol)
arrows(hemDay$doy,hemDay$L.day+hemDay$se.L.day,
       hemDay$doy,hemDay$L.day-hemDay$se.L.day,code=0)

par(mai=c(0.5,0.5,0.5,0.5))

plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl4,yh4))

points(bassDay$doy, bassDay$L.day.m2, type="b",pch=19, col=basscol)
arrows(bassDay$doy,bassDay$L.day.m2+bassDay$se.L.m2.day,
       bassDay$doy,bassDay$L.day.m2-bassDay$se.L.m2.day,code=0, col=basscolt)

points(hemDay$doy, hemDay$L.day.m2, type="b",pch=19, col=hemcol)
arrows(hemDay$doy,hemDay$L.day.m2+hemDay$se.L.m2.day,
       hemDay$doy,hemDay$L.day.m2-hemDay$se.L.m2.day,code=0, col=hemcolt)

dev.off()
