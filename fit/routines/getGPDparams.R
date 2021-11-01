library("extRemes")

gpdFit <- function(data_exceed,q99,basin,gauge){
  
  fit <- fevd(data_exceed,threshold=q99,time.units="days",type="GP",method="MLE",period.basis="year")
  
  # plot parameters
  fil <- paste("gpd_params_",basin,"_",gauge,".pdf",sep="")
  dir.create("./gpd_param_plots/",showWarnings = FALSE)
  dir.create(paste("./gpd_param_plots/",basin,sep=""),showWarnings = FALSE)
  
  gpd_fit_path <- paste("./gpd_param_plots/",basin,"/",fil,sep="")
  pdf(gpd_fit_path)
  plot(fit)
  dev.off()  
  
  return( fit )
  
}
