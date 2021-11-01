#!/opt/local/bin/R

# get_returnCurve_NormalGPD_noSLR.R


# Inputs:
#   - Parameters for Normal-GPD fit to tide gauge data (from tideGauge_normalGPDfit.R)
#   - Text files with heights of observed ESL events at tide gauges (from tideGauge_normalGPDfit.R)
#   - DOES NOT CONSIDER SEA-LEVEL RISE !
#
# Outputs:
#   - Extreme sea level frequency return curves for each tide gauge

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
source("routines/empiricalCDF.R")
source("routines/plotGPDNexceed.R")


# Get parameters for Normal-GPD mixture model
param_fil_dir = "../fit/"
param_fil = "tide_gauge_normalGPD_parameters_may2021.tsv"
param <- read.csv(paste(param_fil_dir,param_fil,sep="/"),header=T,sep="\t")


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
      shapescaleV <- param$GPD_Vscaleshape[j] # covariance of GPD scale and shape parameters
      shapeV <- param$GPD_Vshape[j] # variance of shape parameter
      scaleV <- param$GPD_Vscale[j] # variance of scale parameter
      
      phi <- param$Phi[j] # fraction of observations above the threshold
      norm_mu <- param$Norm_Max_Mu[j]
      norm_sigma <- param$Norm_Max_Sigma[j]

      y1 <- param$Distrib_Fit_Start[j]
      y2 <- param$Distrib_Fit_End[j]
      
     # Account for GPD parameter uncertainty by making draws from a
     # bivariate normal distribution using Latin hypercube sampling
      GPD <- GPDsample(1000, scale, shape, shapeV, scaleV, shapescaleV)

      # Historical ESL return curve CDF (No SLR) (include GPD uncertainty)
      z <- seq(-3,10,.01) # some ESL heights (meters above tide gauge MHHW)
      qqq <- matrix(NaN, nrow=length(GPD[,2]), ncol=length(z))
      
      for(iii in 1:length(GPD[,2]) ){
        qqq[iii,] <- ESLreturn_N_mixed(z,gpd_thresh,GPD[iii,2],GPD[iii,1],phi,
                                     norm_mu,norm_sigma)
      }
      
      # GPD confidence intervals
      gpdCI <- apply(qqq,2,quantile,probs=c(0.05,0.5,0.95),na.rm=T)
      Ne_hist <- apply(qqq,2,mean,na.rm=T) 
      
      # Plot return curves

      # 1. Get historical ESLs to calculate empirical CDF and to add to plot
      obs_dir <- paste(root,"/",proj,"/fit/","obs_daily_max_",basin,sep="")
      obs_fil <- paste("obs_daily_max_",basin,"_gauge_",gauge_id,".tsv",sep="")
      
      obs <- empiricalCDF(paste(obs_dir,obs_fil,sep="/"), gpd_thresh, BELOW=TRUE) 
      
      # 2. Put data into data frame for plotting
      freq <- c(Ne_hist,gpdCI[1,],gpdCI[2,],gpdCI[3,])
      freq[is.na(freq)] <- 10e-10
      
      name <- c(rep("Historic ESL Curve",length(z)),rep("q05",length(z)),
                  rep("q50",length(z)),rep("q95",length(z)))

      df <- data.frame(height=z,freq=freq,name=name,
                       loc=paste(site,", ",country," (",y1,"-",y2,")",sep=""),
                       basin=basin,id=gauge_id)
                       
      filo = paste("output/ESL_return_curve_",gauge_id,".pdf",sep="")

      # 3. Plot it
      plotGPDNExceed(df, obs,filo)

}# end loop over each tide gauge
