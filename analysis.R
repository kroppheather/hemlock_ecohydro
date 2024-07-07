#######################################
## Analysis of Hemlock and Basswood  ##
## sap flux and canopy T with soil   ##
## moisture and weather              ##
#######################################

#### read in libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)


#### sapflow data ----

dirUser <- 1

dirScriptAll <- c("/Users/hkropp/Documents/GitHub/hemlock_ecohydro",
                  "c:/Users/hkropp/Documents/GitHub/hemlock_ecohydro")

dirScript <- dirScriptAll[1]
source(paste0(dirScript, "/sapflux.r"))

#### soil and weather data ----
# soilDaily and soilHourly give soil moisture and temperature
# weatherDaily and weatherHourly have hourly and daily stats

source(paste0(dirScript, "/soil_weather.r"))

#### set up directories ----
dirUser <- 1

dirScriptAll <- c("/Users/hkropp/Documents/GitHub/hemlock_ecohydro",
               "c:/Users/hkropp/Documents/GitHub/hemlock_ecohydro")

dirScript <- dirScriptAll[1]
dirData <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro"

# read in canopy density data

canopyInv <- read.csv("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/kirkland_ecohydro/HCEF forest inventory data.csv")

HemCanopy <- canopyInv %>%
  filter(Plot == "RG25")
# allometry
basswoodmeas <- read.csv(paste0(dirData,"/allometry/basswoodmeas.csv"))
hemlockmeas <- read.csv(paste0(dirData,"/allometry/hemlockmeas.csv"))
basswoodlm <- read.csv(paste0(dirData,"/allometry/DettmannMcfarlane.csv"))
# figure directories

dirFigU <- c("/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research//projects/kirkland_ecohydro/manuscript/figures",
  "G:/My Drive/research/projects/kirkland_ecohydro/manuscript/figures")

dirFig <- dirFigU[1]

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


#L or Kg per day == mm day for El

# filter T_L_day to avoid measurments during very low vPD days in accordance with
# Ewers & Clearwater
T_L_day <- T_L_day %>%
  filter(maxVPD >0.6 & Prec < 2)

ggplot(T_L_day, aes(maxVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(aveVPD, Ec_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(doy, El_day, color=species))+
  geom_point()+
  geom_line()
ggplot(T_L_day, aes(doy, Ec_day, color=species))+
  geom_point()+
  geom_line()

ggplot(T_L_day, aes(ave_SW, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(SWC, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(s_temp, El_day, color=species))+
  geom_point()
T_L_day$ave_SWCent <- T_L_day$ave_SW-200
T_L_day$SWCCent <- T_L_day$SWC-0.25
T_L_day$maxVPDCent <- T_L_day$maxVPD-1.5
# separate out by tree genus
hemDay <- T_L_day %>%
  filter(species == "hemlock")

bassDay <- T_L_day %>%
  filter(species == "basswood")



# daily regressions
hemLM <- lm(hemDay$El_day ~ hemDay$maxVPDCent + hemDay$ave_SWCent + hemDay$SWCCent)
summary(hemLM)
qqnorm(hemLM$residuals)
qqline(hemLM$residuals)
shapiro.test(hemLM$residuals)
plot(hemLM$fitted.values, hemLM$residuals)
abline(h=0)

bassLM <- lm(bassDay$El_day ~ bassDay$maxVPDCent + bassDay$ave_SWCent + bassDay$SWCCent)
summary(bassLM)
qqnorm(bassLM$residuals)
qqline(bassLM$residuals)
shapiro.test(bassLM$residuals)
plot(bassLM$fitted.values, bassLM$residuals)
abline(h=0)

# met only
dailyAll1 <- left_join(soilDaily,dailyPrecip, by="doy")
dailyAll <- left_join(weatherDaily,dailyAll1, by="doy")


# look at hourly patterns ----
# join weather hourly and sapflow hourly


sapHour <- left_join(sapflow.hour, weatherHourly,  by=c("doy", "hour1"="hour"))
sapHourA <- sapHour %>%
  filter(VPD_hr > 0.1 & Precip < 1 & hour1 >= 8 & hour1 <= 19)

daysFull <- sapHourA %>%
  group_by(doy, species) %>%
  summarise(n_count = n()) %>%
  filter(n_count > 11)

# days with both
daysBoth <- daysFull %>%
  group_by(doy) %>%
  summarise(n_spec=n()) %>%
  filter(n_spec == 2)

daysFull <- inner_join(daysFull, daysBoth, by="doy")

sapH <- inner_join(sapHourA, daysFull, by=c("doy","species"))
sapH$Js_g <- sapH$Js*1000

sapH <- left_join(sapH, soilDaily, by="doy")

sapH$swcI <- ifelse(sapH$SWC <= 0.25, 1,
                    ifelse(sapH$SWC > 0.25 & sapH$SWC <=0.35, 2,
                    ifelse(sapH$SWC > 0.35 , 3,NA)))

sapH$swcI <- ifelse(sapH$SWC <= 0.25, 1,
                    ifelse(sapH$SWC > 0.25 & sapH$SWC <=0.35, 2,
                           ifelse(sapH$SWC > 0.35 , 3,NA)))
range(soilDaily$SWC)

ggplot(sapH %>% filter(species == "hemlock"), aes(VPD_hr, Js, color=as.factor(swcI)))+
  geom_point()

ggplot(sapH %>% filter(species == "hemlock"), aes(S_Rad, Js, color=as.factor(swcI)))+
  geom_point()

ggplot(sapH %>% filter(species == "basswood"), aes(VPD_hr, Js, color=as.factor(swcI)))+
  geom_point()

ggplot(sapH %>% filter(species == "basswood"), aes(S_Rad, Js, color=as.factor(swcI)))+
  geom_point()

ggplot(sapHourA, aes(VPD_hr, Js, color=species))+
  geom_point()

hemH <- sapH %>% filter(species == "hemlock")

ggplot(hemH, aes(VPD_hr, Js, color=doy))+
  geom_point()

ggplot(sapHour %>% filter(doy >= 180 & doy <= 190), aes(doy+(hour1/24), Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapHour %>% filter(doy >= 225 & doy <= 235), aes(doy+(hour1/24), Js, color=species))+
  geom_point()+
  geom_line()

#### check correlations  ----
corTest <- na.omit(left_join(soilDaily,dailyPrecip, by="doy"))
corTest <- left_join(corTest, weatherDaily)

plot(corTest$SWC, corTest$weekPr)
cor(corTest$SWC, corTest$weekPr)
plot(corTest$s_temp, corTest$weekPr)
cor(corTest$s_temp, corTest$weekPr)
plot(corTest$s_temp, corTest$SWC)
cor(corTest$s_temp, corTest$SWC)
plot(corTest$maxVPD, corTest$ave_SW)
cor(corTest$maxVPD, corTest$ave_SW)




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

############## Figure 2 bivariate patterns in EL ------


hd <- 3
wd <- 3


xseq1 <- seq(0,3,by=0.5)-1.5
xseqL1 <- seq(0,3,by=0.5)
xl1 <- -1
xh1 <- 1.5

xseq2 <- seq(0.15,0.55,by=0.1)-0.25
xseqL2 <- seq(0.15,0.55,by=0.1)
xl2 <- -0.15
xh2 <- 0.3

xseq3 <- seq(50,350,by=50)-200
xseqL3 <- seq(50,350,by=50)
xl3 <- -150
xh3 <- 150

y1seq <- seq(0,0.1,by=0.02)
yl1 <- 0
yh1 <- 0.1


#axis tick label size
cax <- 1.75

# plot lines width
lw <- 1.5

# line number for x axis label
llx <- 4
llx2 <- 6.5
lly1 <- 6.5
lly2 <- 4
# size of labels
cll <- 1.3
# size of legend
lgc <- 1.5
# size of points
cpt <- 2

names(hemDay)
names(bassDay)
summary(hemLM)
summary(bassLM)



png(paste0(dirFig, "/fig_2_t_lm.png"), width=12, height=5,
    units="in", res=500 )


layout(matrix(seq(1,3),ncol=3, byrow=TRUE), width=lcm(rep(wd*2.54,3)),height=lcm(rep(hd*2.54,1)))

par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl1,xh1), ylim=c(yl1,yh1))

points(hemDay$maxVPDCent, hemDay$El_day, pch=19, col=hemcolt, cex=cpt)
points(bassDay$maxVPDCent,bassDay$El_day, pch=19, col=basscolt, cex=cpt)

axis(1, at=xseq1,labels=xseqL1, cex.axis=cax )
axis(2, y1seq, cex.axis=cax, las=2 )
mtext("Daily transpiration", side=2, line=lly1, cex=cll)
mtext(expression(paste("(L m"^-2, "day"^-1, ")")), side=2, line=lly2, cex=cll)
legend(200,0.12, c("hemlock", "basswood"), pch=19,
       col=c(hemcolt, basscolt), cex=lgc, bty="n")
mtext("Maximum daily VPD", side=1, line=llx, cex=cll)
mtext("(KPA)", side=1, line=llx2, cex=cll)

par(mai=c(0.1,0.1,0.1,0.1))

plot(c(0,1),c(0,1), type="n",axes=FALSE,   xlab= " ", 
     ylab=" ", xlim=c(xl2,xh2), ylim=c(yl1,yh1))


points(hemDay$SWCCent, hemDay$El_day, pch=19, col=hemcolt, cex=cpt)
points(bassDay$SWCCent,bassDay$El_day, pch=19, col=basscolt, cex=cpt)

axis(1, at=xseq2,labels=xseqL2, cex.axis=cax )
mtext("Soil Water Content", side=1, line=llx, cex=cll)
mtext(expression(paste(" (m"^3, "m"^-3, ")")), side=1, line=llx2, cex=cll)



plot(c(0,1),c(0,1), type="n", axes=FALSE,  xlab= " ", 
     ylab=" ", xlim=c(xl3,xh3), ylim=c(yl1,yh1))

points(hemDay$ave_SWCent, hemDay$El_day, pch=19, col=hemcolt, cex=cpt)
points(bassDay$ave_SWCent,bassDay$El_day, pch=19, col=basscolt, cex=cpt)
axis(1, at=xseq3,labels=xseqL3, cex.axis=cax )
mtext("Average Solar Radiation", side=1, line=llx, cex=cll)
mtext(expression(paste(" (W", "m"^-2, ")")), side=1, line=llx2, cex=cll)

dev.off()


############## Figure 3 hourly Js ------

hd <- 2
wd <- 4.5
xl <- 168
xh <- 250

