getAnnualAvg <- function(ann_fil,gauge){
  
  print(paste("Opening: ",ann_fil))
  df_annual = read.csv(ann_fil,sep=";",header=FALSE,quote="\"",comment.char = "")
  
  all_vals = df_annual$V2
  all_vals[all_vals == -99999] <- NA
  
  all_years = df_annual$V1

  df <- data.frame(year=all_years,mean=all_vals) 
  
  return(df)
  
}