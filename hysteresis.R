# archived section of code from analysis.R that did the hysteresis check
# see commit 34c61b9 for full version in analysis.R


#get data frame of historical VPD
VPD_sub <- data.frame(doy = weatherHourly$doy,
                      hour = weatherHourly$hour,
                      VPD = weatherHourly$VPD_hr,
                      SRad = weatherHourly$S_Rad)

# ensure data is complete
VPD_df <- full_join(VPD_sub, dateAll, by=c("doy","hour"))
VPD_df <- VPD_df %>%
  arrange(doy, hour)
# get 1 hour before
VPD_df$VPD_m1 <- c(NA,VPD_df$VPD[1:(length(VPD_df$VPD)-1)])
VPD_df$SRad_m1 <- c(NA, VPD_df$SRad[1:(length(VPD_df$SRad)-1)])
# 2 hours prior
VPD_df$VPD_m2 <- c(NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-2)])
VPD_df$SRad_m2 <- c(NA, NA, VPD_df$SRad[1:(length(VPD_df$SRad)-2)])

# 3 hours prior
VPD_df$VPD_m3 <- c(NA,NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-3)])
VPD_df$SRad_m3 <- c(NA, NA,NA, VPD_df$SRad[1:(length(VPD_df$SRad)-3)])

# 4 hours prior
VPD_df$VPD_m4 <- c(NA,NA,NA,NA,VPD_df$VPD[1:(length(VPD_df$VPD)-4)])
VPD_df$SRad_m4 <- c(NA, NA,NA, NA, VPD_df$SRad[1:(length(VPD_df$SRad)-4)])

VPD_dfJ <- VPD_df %>%
  select(!c(VPD, SRad))

bassH_all <- left_join(bassWeather, VPD_dfJ, by=c("doy", "hour"))
hemH_all <- left_join(hemWeather, VPD_dfJ, by=c("doy", "hour"))


ggplot(sap_analysis, aes(VPD_hr, T.L.hr, color=species))+
  geom_point()

# check for hysteresis in hourly
ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_m4, T.L.hr, color=species))+
  geom_point()+
  geom_path()
ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 168), aes(VPD_m4, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 173), aes(VPD_m1, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(S_Rad, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(SRad_m4, T.L.hr, color=species))+
  geom_point()+
  geom_path()

ggplot(sap_analysis %>% filter(doy == 173), aes(S_Rad, T.L.hr, color=species))+
  geom_point()

ggplot(sap_analysis %>% filter(doy == 173), aes(VPD_hr, T.L.hr, color=species))+
  geom_point()
# evaluate degree of hysteresis
bass_analysis <- sap_analysis %>%
  filter(species == "basswood")

hem_analysis <- sap_analysis %>%
  filter(species == "hemlock")

##### Assess hysteresis ----
# current

plot(log(bass_analysis$VPD_hr),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m1),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m2),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m3),bass_analysis$T.L.hr)
plot(log(bass_analysis$VPD_m4),bass_analysis$T.L.hr)

plot(log(hem_analysis$VPD_hr),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m1),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m2),hem_analysis$T.L.hr)
plot(log(hem_analysis$VPD_m3),hem_analysis$T.L.hr)


# look at hysteresis on a daily basis for basswood


DaysB <- unique(bass_analysis$doy)
modDay <- bass_analysis %>%
  filter(doy == 222)
intC <- numeric()
slopeC <- numeric()
pslopeC <- numeric()
rsqC <- numeric()

intm1B <- numeric()
slopem1B <- numeric()
pslopem1B <- numeric()
rsqm1B <- numeric()

intm2B <- numeric()
slopem2B <- numeric()
pslopem2B <- numeric()
rsqm2B <- numeric()

intm3B <- numeric()
slopem3B <- numeric()
pslopem3B <- numeric()
rsqm3B <- numeric()

for(i in 1:length(DaysB)){
  modDay <- bass_analysis %>%
    filter(doy == DaysB[i])  
  # current
  mod.CB <- lm(modDay$T.L.hr ~ log(modDay$VPD_hr))
  intC[i] <- summary(mod.CB)$coefficients[1,1]
  slopeC[i] <- summary(mod.CB)$coefficients[2,1]
  pslopeC[i] <- summary(mod.CB)$coefficients[2,4]
  rsqC[i] <- summary(mod.CB)$r.squared 
  # minus 1 hour
  mod.m1B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m1))
  summary(mod.m1B)
  intm1B[i] <- summary(mod.m1B)$coefficients[1,1]
  slopem1B[i] <- summary(mod.m1B)$coefficients[2,1]
  pslopem1B[i] <- summary(mod.m1B)$coefficients[2,4]
  rsqm1B[i] <- summary(mod.m1B)$r.squared   
  # minus 2 hour
  mod.m2B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m2))
  summary(mod.m2B)
  intm2B[i] <- summary(mod.m2B)$coefficients[1,1]
  slopem2B[i] <- summary(mod.m2B)$coefficients[2,1]
  pslopem2B[i] <- summary(mod.m2B)$coefficients[2,4]
  rsqm2B[i] <- summary(mod.m2B)$r.squared 
  # minus 3 hour
  mod.m3B <- lm(modDay$T.L.hr ~ log(ifelse(modDay$VPD_m3 == 0,NA,modDay$VPD_m3)))
  summary(mod.m3B)
  intm3B[i] <- summary(mod.m3B)$coefficients[1,1]
  slopem3B[i] <- summary(mod.m3B)$coefficients[2,1]
  pslopem3B[i] <- summary(mod.m3B)$coefficients[2,4]
  rsqm3B[i] <- summary(mod.m3B)$r.squared 
}


bassDFmod <- data.frame(doy = DaysB,
                        intC = intC,
                        slopeC = slopeC,
                        pslopeC = pslopeC,
                        rsqC = rsqC,
                        intm1B = intm1B,
                        slopem1B = slopem1B,
                        pslopem1B = pslopem1B,
                        rsqm1B = rsqm1B,
                        intm2B = intm2B,
                        slopem2B = slopem1B,
                        pslopem2B = pslopem2B,
                        rsqm2B = rsqm2B,
                        intm3B = intm3B,
                        slopem3B = slopem3B,
                        pslopem3B = pslopem3B,
                        rsqm3B = rsqm3B)

# look at sig of slope

bpS <- data.frame(doy = rep(bassDFmod, times=4),
                  lag = rep(c("current","-1","-2","-3"), each=nrow(bassDFmod)),
                  pS = c(bassDFmod$pslopeC, bassDFmod$pslopem1B,
                         bassDFmod$pslopem2B, bassDFmod$pslopem3B))
ggplot(bpS, aes(lag,pS))+
  geom_boxplot()

rpS <- data.frame(doy = rep(bassDFmod, times=4),
                  lag = rep(c("current","-1","-2","-3"), each=nrow(bassDFmod)),
                  pS = c(bassDFmod$rsqC, bassDFmod$rsqm1B,
                         bassDFmod$rsqm2B, bassDFmod$rsqm3B))


ggplot(rpS, aes(lag,pS))+
  geom_boxplot()




# daily patterns in data
# look at hysteresis on a daily basis for hemlock


DaysB <- unique(hem_analysis$doy)
modDay <- hem_analysis %>%
  filter(doy == 222)
intC <- numeric()
slopeC <- numeric()
pslopeC <- numeric()
rsqC <- numeric()

intm1B <- numeric()
slopem1B <- numeric()
pslopem1B <- numeric()
rsqm1B <- numeric()

intm2B <- numeric()
slopem2B <- numeric()
pslopem2B <- numeric()
rsqm2B <- numeric()

intm3B <- numeric()
slopem3B <- numeric()
pslopem3B <- numeric()
rsqm3B <- numeric()

for(i in 1:length(DaysB)){
  modDay <- hem_analysis %>%
    filter(doy == DaysB[i])  
  # current
  mod.CB <- lm(modDay$T.L.hr ~ log(modDay$VPD_hr))
  intC[i] <- summary(mod.CB)$coefficients[1,1]
  slopeC[i] <- summary(mod.CB)$coefficients[2,1]
  pslopeC[i] <- summary(mod.CB)$coefficients[2,4]
  rsqC[i] <- summary(mod.CB)$r.squared 
  # minus 1 hour
  mod.m1B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m1))
  summary(mod.m1B)
  intm1B[i] <- summary(mod.m1B)$coefficients[1,1]
  slopem1B[i] <- summary(mod.m1B)$coefficients[2,1]
  pslopem1B[i] <- summary(mod.m1B)$coefficients[2,4]
  rsqm1B[i] <- summary(mod.m1B)$r.squared   
  # minus 2 hour
  mod.m2B <- lm(modDay$T.L.hr ~ log(modDay$VPD_m2))
  summary(mod.m2B)
  intm2B[i] <- summary(mod.m2B)$coefficients[1,1]
  slopem2B[i] <- summary(mod.m2B)$coefficients[2,1]
  pslopem2B[i] <- summary(mod.m2B)$coefficients[2,4]
  rsqm2B[i] <- summary(mod.m2B)$r.squared 
  # minus 3 hour
  mod.m3B <- lm(modDay$T.L.hr ~ log(ifelse(modDay$VPD_m3 == 0,NA,modDay$VPD_m3)))
  summary(mod.m3B)
  intm3B[i] <- summary(mod.m3B)$coefficients[1,1]
  slopem3B[i] <- summary(mod.m3B)$coefficients[2,1]
  pslopem3B[i] <- summary(mod.m3B)$coefficients[2,4]
  rsqm3B[i] <- summary(mod.m3B)$r.squared 
}


hemDFmod <- data.frame(doy = DaysB,
                       intC = intC,
                       slopeC = slopeC,
                       pslopeC = pslopeC,
                       rsqC = rsqC,
                       intm1B = intm1B,
                       slopem1B = slopem1B,
                       pslopem1B = pslopem1B,
                       rsqm1B = rsqm1B,
                       intm2B = intm2B,
                       slopem2B = slopem1B,
                       pslopem2B = pslopem2B,
                       rsqm2B = rsqm2B,
                       intm3B = intm3B,
                       slopem3B = slopem3B,
                       pslopem3B = pslopem3B,
                       rsqm3B = rsqm3B)

# look at sig of slope

hpS <- data.frame(doy = rep(hemDFmod, times=4),
                  lag = rep(c("current","-1","-2","-3"), each=nrow(hemDFmod)),
                  pS = c(hemDFmod$pslopeC, hemDFmod$pslopem1B,
                         hemDFmod$pslopem2B, hemDFmod$pslopem3B))
ggplot(hpS, aes(lag,pS))+
  geom_boxplot()

hrpS <- data.frame(doy = rep(hemDFmod, times=4),
                   lag = rep(c("current","-1","-2","-3"), each=nrow(hemDFmod)),
                   rS = c(hemDFmod$rsqC, hemDFmod$rsqm1B,
                          hemDFmod$rsqm2B, hemDFmod$rsqm3B))


ggplot(hrpS, aes(lag,rS))+
  geom_boxplot()
