#!/opt/local/bin/R

# run_normalGPDfit.R

# Calculate parameters for Normal-GPD return curve from tide gauge data

# Inputs:

#   - Daily maximum tide gauge data (for the GPD and Normal distribution fit)
#     * e.g. from the GESLA2 database or the University of Hawawii Sea Level Center
#   - Annual average SL data (to de-trend daily maximums)

# Outputs:
#   - Text files with flood heights above MHHW over 99th percentile
#   - Plots with information about GPD parameter fits
#   - Plots with annual SL data over time and latest 30-yr trend
#   - GPD parameters for each tide gauge
#   - Normal distribution parameters for each tide gauge

library("MASS") # for calculating Normal distribution parameters


rm(list=ls(all=TRUE))
root = "/Users/dmr/Dropbox\ (Princeton)/Projects"
proj = "Old/Flood\ Return\ Curve\ Revisit/Normal-GPD\ Mixture"
workDir <- paste(root,proj,"fit",sep="/")

setwd(workDir)

source("routines/getTG.R")
source("routines/getAnnualAvg.R")
source("routines/OpenDailyTide.R")
source("routines/DeclusterTide.R")
source("routines/getGPDparams.R")


# U Hawaii tide gauge data (daily max and annual)
uhawaii_root = paste(root,"Tide\ Gauge\ Data/UHawaiiSL2018/global",sep="/")

# GESLA2 tide gauge data (daily max and annual)
gesla_root = paste(root,"Tide\ Gauge\ Data/GESLA2/",sep="/")


USE_FULL_RECORD = TRUE # use full record, or only period that meets data completion requirement

# Open meta data associated with each tide gauge
metafil <- paste(workDir,"tidegauges_w_min30years.txt",sep="/")
gauge_dat <- getTG2(metafil)
gauge_list <- gauge_dat$Gauge

df <- data.frame(Gauge_ID=character(),PSMSL_ID=character(),Station_Name=character(),
                 Country=character(),Basin=character(),Station_Lat=double(),Station_Lon=double(),
                 GPD_Record_Start=integer(),GPD_Record_End=integer(),MHHW_Exceedances_per_Year=double(),
                 Lambda=double(),GPD_Threshold=double(),Shape50=double(),Shape5=double(),
                 Shape95=double(),Scale50=double(),Scale5=double(),Scale95=double(),
                 Vshape=double(),Vscale=double(),Vscaleshape=double(),Phi=double(),
                 Norm_Max_Mu=double(),Norm_Max_Sigma=double(),stringsAsFactors=FALSE)
                 
# Loop over each tide gauge
for(i in 1:length(gauge_list)){


  print(" ")
  print(paste("Working on... ",gauge_list[i]))
  
  # Open tide gauge data
  
  # 1. Get annual averages
  if (grepl( "gesla",gauge_list[i])){
    
    dir_ann <- paste(gesla_root,"annual",sep="/")
    ann_fil <- Sys.glob(paste(dir_ann,"/annual_sl_",toupper(gauge_list[i]),"*.tsv",sep=""))
    annual <- getAnnualAvg(ann_fil,gauge_list[i])
    
    # remove years before 1900 because timing routines don't work before then
    annual <- annual[annual$year >=1900,]
    
  }else{
    
    dir_ann <- paste(uhawaii_root,gauge_dat$Basin[i],"annual/",sep="/")
    ann_fil <- Sys.glob(paste(dir_ann,"annual_sl_",gauge_dat$Basin[i],"_",gauge_list[i],"*.tsv",sep=""))
    annual <- getAnnualAvg(ann_fil,gauge_list[i])
    
  }
  
  # 2. Get dailies
  # Check to see if we are working with GESLA or U Hawaii...
  if (grepl( "gesla",gauge_list[i])){
    
    # daily max
    dir_max <- paste(gesla_root,"daily_max",sep="/")
    daily_max <- OpenDailyTide(dir_max,"dailymax",gauge_dat$Basin[i],gauge_list[i],gauge_dat,FALSE)
    
  }else{
    
    dir_max <- paste(uhawaii_root,"/",gauge_dat$Basin[i],"/daily_max",sep="")
    daily_max <- OpenDailyTide(dir_max,"dailymax",gauge_dat$Basin[i],gauge_list[i],gauge_dat,TRUE)
    
  }
  
  
  if (USE_FULL_RECORD){
    
    GPD_Record_Start <- gauge_dat$Record_Start_Day[i]
    GPD_Record_End <- gauge_dat$Record_End_Day[i]
    
    # assure that these have same number of years
    daily_max <- daily_max[daily_max[, "Year"] >= GPD_Record_Start & daily_max[, "Year"] <= GPD_Record_End, ]
    annual <- annual[annual[,"year"] >= GPD_Record_Start & annual[, "year"] <= GPD_Record_End, ]
    
  }else{
    
    # only use values in the part of the complete tide gauge record that meets the data completion requirements
     y1d <- gauge_dat$QA_Record_Start_Day[i]
     y2d <- gauge_dat$QA_Record_End_Day[i]
     
     daily_max <- daily_max[daily_max[, "Year"] >= y1d & daily_max[, "Year"] <= y2d, ]
     annual <- annual[annual[,"year"] >= y1d & annual[, "year"] <= y2d, ]
     
     GPD_Record_Start <- gauge_dat$QA_Record_Start_Day[i]
     GPD_Record_End <- gauge_dat$QA_Record_End_Day[i]
     
  }
  
  # Add annual mean to data frame
  daily_max["Annual_Mean"] <- rep(annual$mean, times=table(daily_max["Year"]))
  daily_max["Annual_Mean"][daily_max["Annual_Mean"] == -999] = NA
  
  
  # subtract annual mean from dailies (de-trend)
  daily_max["Max_Tide_detrend"] <- with(daily_max, ifelse((Max_Tide != -999) & (!is.na(Annual_Mean)), Max_Tide - Annual_Mean, -999))

  # Remove missing values
  daily_max <- daily_max[!(daily_max$Max_Tide_detrend==-999),]
  
  # Calculate MHHW relative to MSL (1993 - 2012 or latest 18-yr period)
  yr_last <- annual$year[length(annual$year)]
  
  if(yr_last >= 2012){ # Does record end in 2012 or later?
    y1 <- 1993
    y2 <- 2012
    
  }else{ # Use latest 18-year period in the record as the tidal epoch
    y1 <- yr_last - 18
    y2 <- yr_last
  }
  
  mhhw_period <- as.numeric(unlist(daily_max[daily_max[,"Year"] >= y1 & daily_max[,"Year"] <= y2, ]["Max_Tide_detrend"]))

  # Calculate MMHW for the selected tidal epoch 
  mhhw <- mean(mhhw_period) # relative to msl
  
  # Now calculate daily max tide relative to MHHW (and convert to meters)
  
  daily_max["Max_Tide_rel_MHHW"] <- with(daily_max, ifelse(Max_Tide != -999, (Max_Tide - mhhw)/1000, Max_Tide))
  daily_max["Max_Tide_detrend_rel_MHHW"] <- with(daily_max, ifelse(Max_Tide_detrend != -999, (Max_Tide_detrend - mhhw)/1000, Max_Tide_detrend))
  daily_max_rel_to_mhhw <- as.numeric(unlist(daily_max["Max_Tide_detrend_rel_MHHW"]))
  
  # Stuff for GPD...
  
  # Estimate the frequency of MHHW exceedances
  data_exceed <- daily_max_rel_to_mhhw[daily_max_rel_to_mhhw>0]
  freq_mhhw_exceed <- length(data_exceed)/(length(daily_max_rel_to_mhhw)/365.25) # expected number of MHHW exceedances
  
  gpd_threshold <- quantile(daily_max_rel_to_mhhw,.99) # Use 99th percentile as GPD threshold

  # Decluster exceedances above the GPD threshold
  dc_tide_rel_to_mhhw <- DeclusterTide(daily_max_rel_to_mhhw,gpd_threshold)

  # save out the declustered GPD exceedance events for creating an empirical CDF

  subDir <- paste("obs_gpd_exceed_decluster_",gauge_dat$Basin[i],sep="")
  dir.create(file.path(workDir, subDir), showWarnings = FALSE)

  df_gpd_exceed <- data.frame(Year=daily_max$Year,Month=daily_max$Month,
                              Day=daily_max$Day,Exceedance_Rel_MHHW=dc_tide_rel_to_mhhw)
  outf <- paste(subDir,"/obs_gpd_exceed_decluster_",gauge_dat$Basin[i],"_gauge_",gauge_list[i],".tsv",sep="")
  write.table(df_gpd_exceed, file=outf, col.names = TRUE, row.names = FALSE, sep = "\t")




  # Calculate the GPD parameters using MLE and the Poisson parameter
  data_exceed <- dc_tide_rel_to_mhhw[dc_tide_rel_to_mhhw > gpd_threshold]
  lambda <- length(data_exceed)/(length(dc_tide_rel_to_mhhw)/365.25)

  fit <- gpdFit(data_exceed,gpd_threshold,gauge_dat$Basin[i],gauge_list[i])
  fit_ci <- ci.fevd(fit,alpha=0.05,type="parameter")

  vscale <- parcov.fevd(fit)[1]
  vshape <- parcov.fevd(fit)[4]
  cov <- parcov.fevd(fit)[2] # covariance between scale and shape parameter

  scale5 <- fit_ci[1]
  scale50 <- fit_ci[3]
  scale95 <- fit_ci[5]

  shape5 <- fit_ci[2]
  shape50 <- fit_ci[4]
  shape95 <- fit_ci[6]
 
  # Calculate Normal distribution parameters

  # Write daily maximums to disk (for a scatter plot on top of the mixture model)
  subDir <- paste("obs_daily_max_",gauge_dat$Basin[i],sep="")
  dir.create(file.path(workDir, subDir), showWarnings = FALSE)
  
  outf <- paste(subDir,"/obs_daily_max_",gauge_dat$Basin[i],"_gauge_",gauge_list[i],".tsv",sep="")
  write.table(daily_max$Max_Tide_detrend_rel_MHHW,file=outf,col.names = FALSE,row.names = FALSE,sep = "\t")
              
  # Plot and save image of fit
  subDir <- paste("normal_fit_plots_",gauge_dat$Basin[i],sep="")
  dir.create(file.path(workDir, subDir), showWarnings = FALSE)
  
  file <- paste(file.path(workDir, subDir),"/",paste(gauge_dat$Gauge[i]),".pdf",sep="")
  pdf(file = file, width = 8, height=4)
  
  par(mfrow=c(1,2))
  
  fit <- fitdistr(daily_max$Max_Tide_detrend_rel_MHHW, densfun="normal")
  mu <- fit$estimate[1]
  sig <- fit$estimate[2]
  
  hist(daily_max$Max_Tide_detrend_rel_MHHW, 
       xlab="Detrended, Declustered Daily Max Tide \n (meters relative to MHHW)", 
       main=gauge_dat$Long_Name[i], pch=20, breaks=25, prob=TRUE)
  curve(dnorm(x, mu, sig), col="red", lwd=2, add=T)
  
  # Add Q-Q plots
  qqnorm(daily_max$Max_Tide_detrend_rel_MHHW, pch = 1, frame = FALSE, main=gauge_dat$Gauge[i])
  qqline(daily_max$Max_Tide_detrend_rel_MHHW, col = "red", lwd = 2)
  
  dev.off()
  
  
  # Calculate the ratio of the number of de-clustered observations above the 
  # threshold over the total number of daily observations. This is the probability
  # of exceeding the threshold
  
  num_decluster_exceed <- length( dc_tide_rel_to_mhhw[dc_tide_rel_to_mhhw != -999] )
  num_daily_max_obs <- length( daily_max$Max_Tide_detrend_rel_MHHW )
  
  phi <-  num_decluster_exceed / num_daily_max_obs
  
  df <- rbind(df, data.frame(Gauge_ID=gauge_dat$Gauge[i],
                              PSMSL_ID=gauge_dat$PSMSL[i],
                              Station_Name=gauge_dat$Long_Name[i],
                              Country=gauge_dat$Region[i],
                              Basin=gauge_dat$Basin[i],
                              Station_Lat=gauge_dat$Lat[i],
                              Station_Lon=gauge_dat$Lon[i],
                              Distrib_Fit_Start=GPD_Record_Start,
                              Distrib_Fit_End=GPD_Record_End,
                              MHHW_Exceedances_per_Year=freq_mhhw_exceed,
                              GPD_Lambda=lambda,GPD_Threshold=gpd_threshold,
                              GPD_Shape50=shape50,GPD_Shape5=shape5,GPD_Shape95=shape95,
                              GPD_Scale50=scale50,GPD_Scale5=scale5,GPD_Scale95=scale95,
                              GPD_Vshape=vshape,GPD_Vscale=vscale,GPD_Vscaleshape=cov,Phi=phi,
                              Norm_Max_Mu=mu,Norm_Max_Sigma=sig))
        
} # end loop over each tide gauge

# write file with GPD parameters for this basin only
outfil <- paste("normalGPD_params_tideGauges.tsv",sep="")
write.table(df,file=outfil,quote = TRUE, sep = "\t", eol = "\n", na = "NA",
           dec = ".", row.names = FALSE, col.names = TRUE)