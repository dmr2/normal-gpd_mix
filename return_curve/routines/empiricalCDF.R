empiricalCDF <- function( fil, threshold, BELOW_GPD_THRESHOLD ){
  
  # From observations of daily max ESL heights
  # Assumes inputs are 1. relative to MHHW and 2. missing values are removed
  
  # determine the empirical CDF
  
  z <- seq(-1, 10, .01) # some ESL heights (meters above tide gauge MHHW)
  
  gt <- function(x,y) length( which( x > y ) )/( length(x)/ 365.25 )
 
  dat <- read.csv( fil, header=F, sep="\t")
  
  ct <- NA
  
  # There are too many obs plotted on top of the curves so we thin out the obs...
  # Use declustered above GPD threshold
  # Use thinned out z below GPD threshold
  
  
 for(iii in 1:length(z) ){
  # 
        ct[iii] <- gt( dat$V1, z[iii] )
  # 
       }
  # 
       subct <- which( diff(ct) < 0 ) # thin data set
       
       
       df <- data.frame( z=z[subct], freq=ct[subct], group="Observed" )
  
  return ( df )
  
  
}
