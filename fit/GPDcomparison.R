# Plot GPD curves using Buchanan parameters and Rasmussen parameters

rm(list=ls(all=TRUE))
setwd("/Users/dmr/Dropbox/IPCC Sea Level/GPDfit")

library("reshape")
source("getCurveData.R")
source("histStorms.R")
source("plotGPDNExceed.R")

# Open Buchanan parameters

path <- "/Users/dmr/allowance"
fil <- paste(path,"buchanan_parameters.csv",sep="/")

mkb_param <- read.csv(fil)

path <- "/Users/dmr/Dropbox/IPCC Sea Level/GPDfit"
fil <- paste(path,"gpd_parameters_atlantic.tsv",sep="/")

dmr_param <- read.csv(fil,sep="\t",header=TRUE,quote="\"",comment.char = "")

sites_mkb <- c(12,429,188,234,351,367,396,828,526,224,180,112,395,1153,161,183) # These use PSMSL numbers
sites_dmr <- c(745,744,242,261,253,742,750,767,765,747,264,240,752,746,775,252)
sites_dmr <- paste(as.character(sites_dmr),"a",sep="")

for( i in 1:length(sites_mkb)){
  
  # MKB -- GPD 
  threshold <- mkb_param$mu[mkb_param$PSMSL_ID == sites_mkb[i]]
  lambda <- mkb_param$lambda[mkb_param$PSMSL_ID == sites_mkb[i]]
  scale <- mkb_param$scale50[mkb_param$PSMSL_ID == sites_mkb[i]]
  shape <- mkb_param$shape50[mkb_param$PSMSL_ID == sites_mkb[i]]
  shapeV <- mkb_param$Vshape[mkb_param$PSMSL_ID == sites_mkb[i]]
  scaleV <- mkb_param$Vscale[mkb_param$PSMSL_ID == sites_mkb[i]]
  shapescaleV <- mkb_param$Vscaleshape[mkb_param$PSMSL_ID == sites_mkb[i]]
  
  site <- mkb_param$Site[mkb_param$PSMSL_ID == sites_mkb[i]]
  
  mkb_curve <- getCurveData("buchanan",site,threshold,lambda,scale,shape,shapeV,scaleV,shapescaleV)
  
  # get historical observations
  noaa_id <- mkb_param$NOAA_ID[mkb_param$PSMSL_ID == sites_mkb[i]]
  fil <- paste("/Users/dmr/Dropbox/IPCC Sea Level/maxtofit/maxtofit.dclist.",noaa_id,"_xdat.dc.tsv",sep="")
  obs_mkb <- histStorms(fil,threshold,"buchanan")
  
  # DMR -- GPD 
  threshold <- dmr_param$Q99[dmr_param$UHAWAII_ID == sites_dmr[i]]
  
  lambda <- dmr_param$Lambda[dmr_param$UHAWAII_ID == sites_dmr[i]]
  scale <- dmr_param$Scale50[dmr_param$UHAWAII_ID == sites_dmr[i]]
  shape <- dmr_param$Shape50[dmr_param$UHAWAII_ID == sites_dmr[i]]
  shapeV <- dmr_param$Vshape[dmr_param$UHAWAII_ID == sites_dmr[i]]
  scaleV <- dmr_param$Vscale[dmr_param$UHAWAII_ID == sites_dmr[i]]
  shapescaleV <- dmr_param$Vscaleshape[dmr_param$UHAWAII_ID == sites_dmr[i]]
  
  site <- mkb_param$Site[mkb_param$PSMSL_ID == sites_mkb[i]]
  
  dmr_curve <- getCurveData("rasmussen",site,threshold,lambda,scale,shape,shapeV,scaleV,shapescaleV)
  
  # get historical observations
  fil <- paste("/Users/dmr/Dropbox/IPCC Sea Level/GPDfit/obs_q99exceed_atlantic_gauge_",sites_dmr[i],".csv",sep="")
  obs_dmr <- histStorms(fil,threshold,"rasmussen")
  
  # Plot both curves on the same plot
  plotGPDNExceed2Curves(mkb_curve,dmr_curve,obs_mkb,obs_dmr,sites_dmr[i])
  
}