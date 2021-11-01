getRSLsamps <- function(fil,cut_tail){
  
  # Input:
  #   - Open relative sea-level rise Monte Carlo samples (K14 format)
  #   - User-specified cut-off for PDF
  
  # Output:
  #   - samples MC RSL samples (in meters), Years ]
  #   - Years
  
  
  print(" ")
  print( paste( "Opening SL MC samples:", fil) )
  
  x = read.table( fil, skip=1, sep="\t", header=FALSE)
  
  years = x[,1] # Years corresponding to SLR
  
  iii <- as.data.frame( t(x) ) # Transpose to [SAMPLES,YEARS]
  iii <- iii[2:10001,]
  
  # truncate SLR distribution
  
  print( paste( "Applying cut-off to SLR PDF at the ",cut_tail*100," percentile.",sep=" ") )
  print(" ")
  qcut <- apply( iii, 2, quantile, probs=c(cut_tail), na.rm=T)
  samples <- matrix( NaN, nrow=10000, ncol=length(years) )
  
  for(t in 1:length( years )){
    samples[,t] <- pmin(iii[,t],qcut[t])
  }
  
  return ( list(samples=samples, years=years) )
  
}