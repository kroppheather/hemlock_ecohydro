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

dateAll <- data.frame( doy = rep(seq(160,252), each=24),
                       hour = rep(seq(0,23), times=length(seq(160,252))))

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


# join in full day list to ensure gaps aren't connected
doyAll <- data.frame( doy = seq(160,252))
                      
bassDay <- Tc.L.day %>%
  filter(species == "basswood")
bassDay <- left_join(doyAll,bassDay, by="doy")

hemDay <- Tc.L.day %>%
  filter(species == "hemlock")
hemDay <- left_join(doyAll,hemDay, by="doy")

########### Figure 1 -----------
basscol <- "#4091E5"
basscolt <- "#4091E599"
hemcol <- "#3a5a40"
hemcolt <- "#3a5a4099"

hd <- 2
wd <- 4.5
xl <- 168
xh <- 250
xseq <- seq(160,260,by=10)
y1seq <- seq(0,0.5,by=0.1)
y1bseq <- seq(0, 35, by=5)
y2seq <- seq(0,2.4,by=0.2)
y2bseq <- seq(10,30, by=5)
y3seq <- seq(0,0.16,by=0.04)
y4seq <- seq(0,0.02,by=0.01)
yl1 <- 0
yh1 <- 0.5

yl2 <- 0
yh2 <- 2.5

yl3 <- 0
yh3 <- 0.17

yl4 <- 0
yh4 <- 0.02

#axis tick label size
cax <- 1.75

# plot lines width
lw <- 1.5

# line number for x axis label
llx <- 3
lly1 <- 6.5
lly2 <- 4
# size of labels
cll <- 1.3
# size of legend
lgc <- 1.5
# size of points
cpt <- 2



precipScale <- yh1/ceiling(max(dailyAll$Prec))
test <- dailyAll$Prec* precipScale
tempScale <- (dailyAll$AirT - 10)*(yh2/20)


png(paste0(dirFig, "/fig_1_met_t.png"), width=7, height=9,
    units="in", res=500 )


layout(matrix(seq(1,4),ncol=1, byrow=TRUE), width=lcm(rep(wd*2.54,1)),height=lcm(rep(hd*2.54,4)))

par(mai=c(0.1,0.1,0.1,0.1))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl1,yh1))
for(i in 1:nrow(dailyAll)){
  polygon(c(dailyAll$doy[i]-0.25,dailyAll$doy[i]-0.25,
            dailyAll$doy[i]+0.25,dailyAll$doy[i]+0.25),
          c(0,dailyAll$Prec[i],dailyAll$Prec[i],0)*precipScale,
          border=NA, col="#C6DAF4")
}
points(dailyAll$doy, dailyAll$SWC, type="l", lwd=lw) 
axis(1, xseq, cex.axis=cax )
axis(2, y1seq, cex.axis=cax, las=2 )
axis(4, y1bseq*precipScale,y1bseq, cex.axis=cax, las=2 )
mtext("Soil moisture", side=2, line=lly1, cex=cll)
mtext(expression(paste("( m"^3, "m"^-3,")")), side=2, line=lly2, cex=cll)
mtext("Precipitation", side=4, line=lly2, cex=cll)
mtext("(mm)", side=4, line=lly1, cex=cll)
legend(200,0.5, c("soil moisture", "precipitation"), lwd=c(lw,NA),
       col=c("black","#C6DAF4"),pch=c(NA,15), cex=lgc, bty="n")


par(mai=c(0.1,0.1,0.1,0.1))
plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl2,yh2))

points(dailyAll$doy, dailyAll$maxVPD, type="l", lwd=lw)   
points(dailyAll$doy, tempScale, type="l", lwd=lw, col="tomato3")  

axis(1, xseq, cex.axis=cax )
axis(2, y2seq, cex.axis=cax, las=2 )
axis(4, (y2bseq-10)*(yh2/20),y2bseq, cex.axis=cax, las=2 )
par(mai=c(0.1,0.1,0.1,0.1))
mtext("Maximum VPD", side=2, line=lly1, cex=cll)
mtext("(KPa)", side=2, line=lly2, cex=cll)
mtext("Air temperature", side=4, line=lly2, cex=cll)
mtext("(C)", side=4, line=lly1, cex=cll)
legend(200,0.8, c("soil moisture", "precipitation"), lwd=c(lw,lw),
       col=c("black","tomato3"), cex=lgc, bty="n")


par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl3,yh3))



points(hemDay$doy, hemDay$L.day.m2, pch=19, col=hemcol, cex=cpt)
arrows(hemDay$doy,hemDay$L.day.m2+hemDay$se.L.m2.day,
       hemDay$doy,hemDay$L.day.m2-hemDay$se.L.m2.day,code=0)
axis(1, xseq, cex.axis=cax )
axis(2, y3seq,  cex.axis=cax, las=2 )
mtext("Daily transpiration", side=2, line=lly1, cex=cll)
mtext(expression(paste("(L m"^-2, "day"^-1, ")")), side=2, line=lly2, cex=cll)

par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl4,yh4))

points(bassDay$doy, bassDay$L.day.m2,pch=19, col=basscol, cex=cpt)
arrows(bassDay$doy,bassDay$L.day.m2+bassDay$se.L.m2.day,
       bassDay$doy,bassDay$L.day.m2-bassDay$se.L.m2.day,code=0)


axis(1, xseq, cex.axis=cax )
axis(2, y4seq,  cex.axis=cax, las=2 )
mtext("Day of year", side=1, line=llx, cex=cll)
mtext("Daily transpiration", side=2, line=lly1, cex=cll)
mtext(expression(paste("(L m"^-2, "day"^-1, ")")), side=2, line=lly2, cex=cll)
dev.off()





