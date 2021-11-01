#!/opt/local/bin/R

# get_returnCurve_NormalGPD_SLR.R

# Inputs:
#   - Parameters for Normal-GPD fit to tide gauge data 
#                                           (from tideGauge_normalGPDfit.R)
#   - Text files with heights of observed ESL events at tide gauges 
#                                           (from tideGauge_normalGPDfit.R)
#   - Sea-level rise Monte Carlo samples at tide gauges
#      (e.g., from Kopp et al., 2014 framework; LocalizeSL MATLAB code)
#      * Currently setup for 2C/5C SLR scenarios from Bamber et al., 2019
#
# Outputs:
#   - Extreme sea level frequency return curves for each tide gauge (with SLR)

# Written by: D.J. Rasmussen (dmr2-AT-princeton-DOT-edu)
# Last Updated: Sat Oct 30 10:43:39 PDT 2021


rm(list=ls(all=TRUE))

root <- "/Users/dmr/Dropbox\ (Princeton)/Projects"
proj <- "Flood\ Return\ Curve\ Revisit/Normal-GPD\ Mixture"
subdir <- "return_curve"

workDir <- paste(root,proj,subdir,sep="/")
setwd(workDir)

source("routines/GPDsample.R")
source("routines/ESLreturn_N_mixed.R")
source("routines/openSLR.R")
source("routines/ESLreturn_Nslr_mixed.R") # expected number of events with SLR
source("routines/empiricalCDF.R")
source("routines/plotGPDNExceed_slr.R")


# Get parameters for Normal-GPD mixture model
param_fil_dir = "../fit/"
param_fil = "tide_gauge_normalGPD_parameters_may2021.tsv"
param <- read.csv(paste(param_fil_dir,param_fil,sep="/"),header=T,sep="\t")


# Specify the years of interest
targ_years = c(2070,2100)

# Location of local sea level rise Monte Carlo samples
dir_slr = "/Users/dmr/Dropbox\ (Princeton)/Projects/ESL\ Metrics/SLR"


scenarios = c("2.0C", "5.0C")
scen_dir = c("Bamber19", "Bamber19")
slab = c("2p0degree+L", "rcp85+H")


for(j in 1:length(param$Gauge_ID)){

  print(paste("Working on this tide gauge: ",param$Gauge_ID[j],sep=""))

  site <- param$Station_Name[j] # tide gauge site name
  country <- param$Country[j]
  basin <- param$Basin[j]
  scale <- param$GPD_Scale50[j] # median scale parameter
  shape <- param$GPD_Shape50[j] # median shape parameter
  gauge_id <- param$Gauge_ID[j]
  slr_id <- param$PSMSL_ID[j]
  gpd_thresh <- param$GPD_Threshold[j] # GPD threshold
  mhhw_exceed <- param$MHHW_Exceedances_per_Year[j] # historical expected MHHW exceedances per year
  lambda <- param$GPD_Lambda[j] # mean Poisson arrival rate of threshold
  shapescaleV <- param$GPD_Vscaleshape[j] # covariance of GPD scale and shape paraj=jmeters
  shapeV <- param$GPD_Vshape[j] # variance of shape parameter
  scaleV <- param$GPD_Vscale[j] # variance of scale parameter
  
  phi <- param$Phi[j]
  norm_mu <- param$Norm_Max_Mu[j]
  norm_sigma <- param$Norm_Max_Sigma[j]

  y1 <- param$Distrib_Fit_Start[j]
  y2 <- param$Distrib_Fit_End[j]
  
 # Account for GPD parameter uncertainty by making draws from a
 # bivariate normal distribution using Latin hypercube sampling
  GPD <- GPDsample(1000, scale, shape, shapeV, scaleV, shapescaleV)

  z <- seq(-3,10,.01) # some ESL heights (meters above tide gauge MHHW)
  
  # Historical ESL return curve CDF (No SLR) (include GPD uncertainty)
  qqq <- matrix(NaN, nrow=length(GPD[,2]), ncol=length(z))

  for(iii in 1:length(GPD[,2]) ){
    qqq[iii,] <- ESLreturn_N_mixed(z,gpd_thresh,GPD[iii,2],GPD[iii,1],phi,
                                 norm_mu,norm_sigma)
  }
  
  gpdCI <- apply(qqq,2,quantile,probs=c(0.05,0.5,0.95),na.rm=T) # GPD confidence intervals
  Ne_hist <- apply(qqq,2,mean,na.rm=T) # expected number of historical events
  
  # Ne_slr: expected number of projected extreme sea level events
  Ne_slr <- array(NaN,dim=c(length(targ_years),length(scenarios),length(z))) 
  
      for( s in 1:length(scenarios) ){
        
        sub_dir = paste(scen_dir[s],"/",paste(scen_dir[s],"_",slab[s],sep=""),sep="")
        
        # Get sea level rise Monte Carlo samples
        
        if (scenarios[s]=="5.0C"){
          fil_slr <- paste( dir_slr,"/",sub_dir,"/LSLproj_MC_RCP85+H_",slr_id,"_rcp85+H.tsv",sep="")
        }else if (scenarios[s]=="2.0C"){
          fil_slr <- paste( dir_slr,"/",sub_dir,"/LSLproj_MC_2.0C+L_",slr_id,"_2p0degree+L.tsv",sep="")
        }else{
          fil_slr <- paste( dir_slr,"/",sub_dir,"/LSLproj_MC_",slr_id,"_",slab[s],".tsv",sep="")
        }
        SLR <- getRSLsamps( fil_slr, .999 )
        
        SLRMC <- SLR$samples/100 # centimeters to meters
        years <- SLR$years 
        
        for( t in 1:length(targ_years) ){
          
          slr_samps <- SLRMC[,which(years==targ_years[t])]
          print(paste("The mean SLR projection for ",targ_years[t]," is: ",mean(slr_samps)," meters",sep=""))
          
          # Future flood curves that include GPD parameter and SLR uncertainty
          
          ans <- ESLreturn_Nslr_mixed(z,gpd_thresh,GPD[,2],GPD[,1],phi,
                                      norm_mu,norm_sigma,slr_samps,1000)
          
          Ne_slr[t,s,] <- apply(matrix(ans,nrow=10000,ncol=length(z)),2,mean)
         
        } # each target year
      } # Each SLR scenario
  
  
  # Plot ESL curves
  
  # Get historical ESLs to calculate empirical CDF and to add to plot
  # 1. Get historical ESLs to calculate empirical CDF and to add to plot
  obs_dir <- paste(root,"/",proj,"/fit/","obs_daily_max_",basin,sep="")
  obs_fil <- paste("obs_daily_max_",basin,"_gauge_",gauge_id,".tsv",sep="")
      
  obs <- empiricalCDF(paste(obs_dir,obs_fil,sep="/"), gpd_thresh, BELOW=TRUE) 
  obs$group <- paste("Observed (",y1,"-",y2,")",sep="")
  
  for( t in 1:length(targ_years) ){
    
        # 2. Put data into data frame for plotting

        freq <- c(Ne_hist,gpdCI[1,],gpdCI[2,],gpdCI[3,],Ne_slr[t,1,],Ne_slr[t,2,])
        freq[is.na(freq)] <- 10e-10
        
        name <- c(rep("Historic",length(z)),
                  rep("q5",length(z)),rep("q50",length(z)),rep("q95",length(z)),
                  rep("Ne 2.0°C",length(z)),rep("Ne 5.0°C",length(z)))
        
        df <- data.frame(height=z,freq=freq,name=name,loc=param$Station_Name[j],
                         basin=basin,year=targ_years[t],id=gauge_id)
        
        plotGPDNExceed_slr(df, obs, targ_years[t])
    
  } # each target year

}# end loop over each tide gauge
