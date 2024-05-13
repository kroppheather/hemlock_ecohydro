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