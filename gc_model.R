###########################################################################
###########################################################################
############## Created by Heather Kropp in November 2023      ##############
############## The model code for the analysis of the        ##############
############## of canopy stomatal conductance calculated     ##############
############## from sapflow. The model is based on a         ##############
############## phenomenological model that describes         ##############
############## stomatal responses to light and VPD           ##############
############## used in Kropp et. al. 2017, Oren et. al. 1999,##############
############## Jarvis 1976, and White et. al. 1999.          ##############
############## The parameters of this model are considered   ##############
############## to vary with environmental drivers:           ##############
############## soil moisture, average daily air temperature  ##############
############## and past precipitation.                       ##############
###########################################################################

model{
  #################################
  #########Model likelihood########
  #################################
  for(i in 1:Nobs){
    #likelihood for each tree
    gs[i]~dnorm(mu.gs[i],tau.gs[spec.obs[i]])
    rep.gs[i]~dnorm(mu.gs[i],tau.gs[spec.obs[i]])
    
    #gs.rep[i]~dnorm(mu.gs[i],tau.gs)
    #model for mean gs
    #oren model 1999 for mean gs
    mu.gs[i]<-gref[specDay[i]]*(1-(S[specDay[i]]*log(D[i])))
    
  }
  #################################
  #########parameter model ########
  #################################	
  for(i in 1:NspecDay){
    gref[i]<-a[1,specID[i]]+a[2,specID[i]]*airTcent[i]+a[3,specID[i]]*(pastpr[i])+a[4,specID[i]]*SWCcent[i]
    S[i]<-b[1,specID[i]]+b[2,sprecID[i]]*airTcent[i]+b[3,specID[i]]*(pastpr[i])+b[4,specID[i]]*SWCcent[i]
   # covariate center so not at zero
     airTcent[i]<-airT[i]-airTmean	
     SWCcent[i]<-SWC[i]-SWCmean
  }
  #################################
  #########priors          ########
  #################################	
  #define prior distributions for parameters
  #All parameters are given non-informative dist
  
  
  for(i in 1:Nspec){
    
    tau.gs[i]<-pow(sig.gs[i],-2)
    sig.gs[i]~dunif(0,1000)	
    for(j in 1:Nparam){
      a[j,i]~dnorm(0,.0001)
      b[j,i]~dnorm(0,.0001)
    }
    
  }
  
  
} 