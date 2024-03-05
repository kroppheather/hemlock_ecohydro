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

# figure directories

dirFig <- "G:/My Drive/research/projects/kirkland_ecohydro/manuscript/figures"


#### organize daily data  ----
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

T_L_day <- left_join(Tc.L.day, dailyPrecip, by="doy")
T_L_day$El_se <- T_L_day$sd_El/sqrt(T_L_day$n_plant)
# note that L per day works out to mm/day 

# filter T_L_day to avoid measurments during very low vPD days in accordance with
# Ewers & Clearwater
T_L_day <- T_L_day %>%
  filter(aveVPD >0.6)

# separate out by tree genus
hemDay <- T_L_day %>%
  filter(species == "hemlock")

bassDay <- T_L_day %>%
  filter(species == "basswood")

# met only
dailyAll1 <- left_join(soilDaily,dailyPrecip, by="doy")
dailyAll <- left_join(weatherDaily,dailyAll1, by="doy")


#### check correlations  ----
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




############## Figures ------
basscol <- "#EF5D43"
basscolt <- "#EF5D4399"
hemcol <- "#3E9AF0"
hemcolt <- "#3E9AF099"

############## Figure 1 daily data ------


hd <- 2.5
wd <- 4.5

xl <- 168
xh <- 250
xseq <- seq(160,260,by=10)
y1seq <- seq(0,0.5,by=0.1)
y1bseq <- seq(0, 35, by=5)
y2seq <- seq(0,2.4,by=0.2)
y2bseq <- seq(10,30, by=5)
y3seq <- seq(0,0.12,by=0.02)

yl1 <- 0
yh1 <- 0.5

yl2 <- 0
yh2 <- 2.5

yl3 <- 0
yh3 <- 0.12



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


layout(matrix(seq(1,3),ncol=1, byrow=TRUE), width=lcm(rep(wd*2.54,1)),height=lcm(rep(hd*2.54,3)))

par(mai=c(0.1,0.1,0.1,0.1))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl1,yh1))
for(i in 1:nrow(dailyAll)){
  polygon(c(dailyAll$doy[i]-0.25,dailyAll$doy[i]-0.25,
            dailyAll$doy[i]+0.25,dailyAll$doy[i]+0.25),
          c(0,dailyAll$Prec[i],dailyAll$Prec[i],0)*precipScale,
          border=NA, col="#B4DCFF")
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
       col=c("black","#B4DCFF"),pch=c(NA,15), cex=lgc, bty="n")


par(mai=c(0.1,0.1,0.1,0.1))
plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl2,yh2))

points(dailyAll$doy, dailyAll$maxVPD, type="l", lwd=lw)   
points(dailyAll$doy, tempScale, type="l", lwd=lw, col="#F0C23E")  

axis(1, xseq, cex.axis=cax )
axis(2, y2seq, cex.axis=cax, las=2 )
axis(4, (y2bseq-10)*(yh2/20),y2bseq, cex.axis=cax, las=2 )
par(mai=c(0.1,0.1,0.1,0.1))
mtext("Maximum VPD", side=2, line=lly1, cex=cll)
mtext("(KPa)", side=2, line=lly2, cex=cll)
mtext("Air temperature", side=4, line=lly2, cex=cll)
mtext("(C)", side=4, line=lly1, cex=cll)
legend(200,0.8, c("VPD", "air T"), lwd=c(lw,lw),
       col=c("black","#F0C23E"), cex=lgc, bty="n")


par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl,xh), ylim=c(yl3,yh3))



points(hemDay$doy, hemDay$El_day, pch=19, col=hemcolt, cex=cpt)
arrows(hemDay$doy,hemDay$El_day+hemDay$El_se,
       hemDay$doy,hemDay$El_day-hemDay$El_se,code=0, col=hemcolt)
points(bassDay$doy, bassDay$El_day,pch=19, col=basscolt, cex=cpt)
arrows(bassDay$doy,bassDay$El_day+bassDay$El_se,
       bassDay$doy,bassDay$El_day-bassDay$El_se,code=0, col=basscolt)

axis(1, xseq, cex.axis=cax )
axis(2, y3seq,  cex.axis=cax, las=2 )
mtext("Daily transpiration", side=2, line=lly1, cex=cll)
mtext(expression(paste("(L m"^-2, "day"^-1, ")")), side=2, line=lly2, cex=cll)
legend(200,0.12, c("hemlock", "basswood"), pch=19,
       col=c(hemcolt, basscolt), cex=lgc, bty="n")
mtext("Day of year", side=1, line=llx, cex=cll)

dev.off()

############## Figure 2 daily max Js ------



hd <- 2
wd <- 4.5
xl <- 168
xh <- 250
xseq <- seq(160,260,by=10)
y1seq <- seq(0,0.5,by=0.1)
yl1 <- 0
yh1 <- 0.5


