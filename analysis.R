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
  filter(maxVPD >0.6 & Prec < 2)

ggplot(T_L_day, aes(maxVPD, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(doy, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(ave_SW, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(SWC, El_day, color=species))+
  geom_point()

ggplot(T_L_day, aes(s_temp, El_day, color=species))+
  geom_point()

# separate out by tree genus
hemDay <- T_L_day %>%
  filter(species == "hemlock")

bassDay <- T_L_day %>%
  filter(species == "basswood")


# regressions
hemLM <- lm(hemDay$El_day ~ hemDay$maxVPD + hemDay$ave_SW + hemDay$SWC)
summary(hemLM)
qqnorm(hemLM$residuals)
qqline(hemLM$residuals)
shapiro.test(hemLM$residuals)
plot(hemLM$fitted.values, hemLM$residuals)
abline(h=0)

bassLM <- lm(bassDay$El_day ~ bassDay$maxVPD + bassDay$ave_SW + bassDay$SWC)
summary(bassLM)
qqnorm(bassLM$residuals)
qqline(bassLM$residuals)
shapiro.test(bassLM$residuals)
plot(bassLM$fitted.values, bassLM$residuals)
abline(h=0)

# met only
dailyAll1 <- left_join(soilDaily,dailyPrecip, by="doy")
dailyAll <- left_join(weatherDaily,dailyAll1, by="doy")


# look at hourly patterns
# join weather hourly and sapflow hourly
bassHour <- sapflow.hour %>%
  filter(species == "basswood")

HemHour <- sapflow.hour %>%
  filter(species == "hemlock")

bass_sapHour <- left_join(weatherHourly,bassHour,  by=c("doy", "hour"="hour1"))

hem_sapHour <- left_join(weatherHourly,HemHour,  by=c("doy", "hour"="hour1"))

# check VPD for hysteresis
bass_sapHour$VPD_m1 <- c(NA, bass_sapHour$VPD_hr[1:(nrow(bass_sapHour)-1)])

hem_sapHour$VPD_m1 <- c(NA, hem_sapHour$VPD_hr[1:(nrow(hem_sapHour)-1)])

bass_sapHour$VPD_m2 <- c(NA,NA, bass_sapHour$VPD_hr[1:(nrow(bass_sapHour)-2)])

hem_sapHour$VPD_m2 <- c(NA,NA, hem_sapHour$VPD_hr[1:(nrow(hem_sapHour)-2)])

bass_sapHour$VPD_m3 <- c(NA,NA,NA, bass_sapHour$VPD_hr[1:(nrow(bass_sapHour)-3)])

hem_sapHour$VPD_m3 <- c(NA,NA,NA, hem_sapHour$VPD_hr[1:(nrow(hem_sapHour)-3)])

sap_Hour <- na.omit(rbind(hem_sapHour, bass_sapHour))

# average over same hour
sapH_days <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_hr >0.6 & hour > 6 & hour < 19 & Precip < 1) %>%
  group_by(doy, species) %>%
  summarise(n_obs = n()) %>% filter(n_obs > 6)

sapHourA0 <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_hr >0.6 & hour > 6 & hour < 19 & Precip < 1)
hemHourA0 <- sapHourA0 %>%
  filter(species == "hemlock")
bassHourA0 <- sapHourA0 %>%
  filter(species == "basswood")

bassA0Days <- sapH_days %>%
  filter(species == "basswood")

hemA0Days <- sapH_days %>%
  filter(species == "hemlock")

hemVPDLM <- list()

hemSub <- hemHourA0 %>% filter(doy == hemA0Days$doy[1])

for(i in 1:nrow(hemA0Days)){
  hemSub <- hemHourA0 %>% filter(doy == hemA0Days$doy[i])
  hemVPDLM[[i]] <- data.frame(doy= hemA0Days$doy[i],
                slope0= summary(lm(hemSub$Js ~ log(hemSub$VPD_hr)))$coefficients[2,1],
                 r.sq0 = summary(lm(hemSub$Js ~ log(hemSub$VPD_hr)))$r.squared)
}

hem0DF <- do.call("rbind", hemVPDLM)


bassSub <- bassHourA0 %>% filter(doy == bassA0Days$doy[1])
bassVPDLM <- list()
for(i in 1:nrow(bassA0Days)){
  bassSub <- bassHourA0 %>% filter(doy == bassA0Days$doy[i])
  bassVPDLM[[i]] <- data.frame(doy= bassA0Days$doy[i],
                              slope0= summary(lm(bassSub$Js ~ log(bassSub$VPD_hr)))$coefficients[2,1],
                              r.sq0 = summary(lm(bassSub$Js ~ log(bassSub$VPD_hr)))$r.squared)
}

bass0DF <- do.call("rbind", bassVPDLM)


# Previous hour
sapH_days <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_m1 >0.6 & hour > 6 & hour < 19 & Precip < 1) %>%
  group_by(doy, species) %>%
  summarise(n_obs = n()) %>% filter(n_obs > 6)

sapHourA1 <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_m1 >0.6 & hour > 6 & hour < 19 & Precip < 1)
hemHourA1 <- sapHourA1 %>%
  filter(species == "hemlock")
bassHourA1 <- sapHourA0 %>%
  filter(species == "basswood")

bassA1Days <- sapH_days %>%
  filter(species == "basswood")

hemA1Days <- sapH_days %>%
  filter(species == "hemlock")


hemVPDLM1 <- list()

hemSub1 <- hemHourA1 %>% filter(doy == hemA1Days$doy[1])

for(i in 1:nrow(hemA1Days)){
  hemSub1 <- hemHourA1 %>% filter(doy == hemA1Days$doy[i])
  hemVPDLM1[[i]] <- data.frame(doy= hemA1Days$doy[i],
                              slope1= summary(lm(hemSub1$Js ~ log(hemSub1$VPD_m1)))$coefficients[2,1],
                              r.sq1 = summary(lm(hemSub1$Js ~ log(hemSub1$VPD_m1)))$r.squared)
}

hem1DF <- do.call("rbind", hemVPDLM1)


bassVPDLM1 <- list()

bassSub1 <- bassHourA1 %>% filter(doy == bassA1Days$doy[1])

for(i in 1:nrow(bassA1Days)){
  bassSub1 <- bassHourA1 %>% filter(doy == bassA1Days$doy[i])
  bassVPDLM1[[i]] <- data.frame(doy= bassA1Days$doy[i],
                               slope1= summary(lm(bassSub1$Js ~ log(bassSub1$VPD_m1)))$coefficients[2,1],
                               r.sq1 = summary(lm(bassSub1$Js ~ log(bassSub1$VPD_m1)))$r.squared)
}

bass1DF <- do.call("rbind", bassVPDLM1)


# previous 2 hour


# Previous hour
sapH_days <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_m2 >0.6 & hour > 6 & hour < 19 & Precip < 1) %>%
  group_by(doy, species) %>%
  summarise(n_obs = n()) %>% filter(n_obs > 6)

sapHourA2 <- sap_Hour %>%
  filter(S_Rad > 1 & VPD_m2 >0.6 & hour > 6 & hour < 19 & Precip < 1)
hemHourA2 <- sapHourA1 %>%
  filter(species == "hemlock")
bassHourA2 <- sapHourA0 %>%
  filter(species == "basswood")

bassA2Days <- sapH_days %>%
  filter(species == "basswood")

hemA2Days <- sapH_days %>%
  filter(species == "hemlock")


hemVPDLM2 <- list()

hemSub2 <- hemHourA2 %>% filter(doy == hemA2Days$doy[1])

for(i in 1:nrow(hemA2Days)){
  hemSub2 <- hemHourA0 %>% filter(doy == hemA2Days$doy[i])
  hemVPDLM2[[i]] <- data.frame(doy= hemA2Days$doy[i],
                               slope2= summary(lm(hemSub2$Js ~ log(hemSub2$VPD_m1)))$coefficients[2,1],
                               r.sq2 = summary(lm(hemSub2$Js ~ log(hemSub2$VPD_m1)))$r.squared)
}

hem2DF <- do.call("rbind", hemVPDLM2)


bassVPDLM2 <- list()

bassSub2 <- bassHourA1 %>% filter(doy == bassA2Days$doy[1])

for(i in 1:nrow(bassA2Days)){
  bassSub2 <- bassHourA2 %>% filter(doy == bassA2Days$doy[i])
  bassVPDLM2[[i]] <- data.frame(doy= bassA2Days$doy[i],
                                slope2= summary(lm(bassSub2$Js ~ log(bassSub2$VPD_m2)))$coefficients[2,1],
                                r.sq2 = summary(lm(bassSub2$Js ~ log(bassSub2$VPD_m2)))$r.squared)
}

bass2DF <- do.call("rbind", bassVPDLM2)

bassVPDcomp1 <- full_join(bass0DF, bass1DF, by="doy")
bassVPDcomp <- full_join(bassVPDcomp1, bass2DF, by="doy")

hemVPDcomp1 <- full_join(hem0DF, hem1DF, by="doy")
hemVPDcomp <- full_join(hemVPDcomp1, hem2DF, by="doy")


ggplot(sapHourA0, aes(VPD_hr, Js, color=species))+
  geom_point()

ggplot(sapHourA %>% filter(doy == 170), aes(VPD_hr, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapHourA %>% filter(doy == 245), aes(VPD_hr, Js, color=species))+
  geom_point()+
  geom_line()

ggplot(sapHourA, aes(VPD_hr, Js, color=species))+
  geom_point()


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

############## Figure 2 daily max Js ------



hd <- 2
wd <- 4.5
xl <- 168
xh <- 250
xseq <- seq(160,260,by=10)
y1seq <- seq(0,0.5,by=0.1)
yl1 <- 0
yh1 <- 0.5


