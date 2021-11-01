ESLreturn_N_mixed <- function(z,u,scale,shape,phi,norm_mu,norm_sigma){
                            
  # Function "ESLreturn_N_mixed"
  #
  # Calculate the number of extreme sea level events using a Normal-generalized
  # Pareto distribution (GPD) probability mixture model

  # Reference:
  
  # M. Ghanbari, M. Arabi, J. Obeysekera, W. Sweet. (2019). 
  # A coherent statistical model for coastal flood frequency
  # analysis under nonstationary sea level conditions
  
  # History
  # 5/24/2021 (DMR): Function created following Ghanbari et al., 2019

  library(pracma) # needed for the error function, erf(x)
  
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  
  z0 <- z # save for later
  
  z <- z - u # z is relative to the GPD threshold
  
  z <- pmax(z,0) # > 0 only b/c GPD is only defined at or above threshold (i.e., z=0)
  
  if ( shape == 0 ){ 
    
    result <- 365.25 * phi * mapply( function(x) exp(-1*x/scale), z)
    
  }else if( shape < 0 ){ 
    
    z <- pmin(z, .99999* -scale/shape) # values stay within the support of the GPD
    result <- 365.25 * phi * mapply( function(x) exponent( 1 + (shape*x /scale), -1/shape), z)
    
  }else{
    
    result <- 365.25 * phi * mapply( function(x) exponent( 1 + (shape*x /scale), -1/shape), z)
    
  }

  # Put ESL events below GPD threshold on a Normal distribution
  
  indx <- which( z0 < u ) # put z's below GPD threshold on a Normal
  
  # NOTE: norm_mu will get shifted due to SLR...
  p1 <- (1 - phi) * ( 1 + erf( (z0[indx] - norm_mu)/(norm_sigma * sqrt(2))) )
  p2 <- 1 / ( 1 + erf( (u - norm_mu)/(norm_sigma * sqrt(2))) )
  result[indx] <- 365.25 * ( 1 - p1 * p2)

  return(result)
    
}
