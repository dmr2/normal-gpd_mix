ESLreturn_CDF_mixed <- function(z,u,scale,shape,phi,norm_mu,norm_sigma){
                            
  # Function "ESLreturn_CDF_mixed"
  #
  # Calculate the extreme sea level CDF using a Normal-generalized
  # Pareto distribution (GPD) probability mixture model

  # Reference:
  
  # M. Ghanbari, M. Arabi, J. Obeysekera, W. Sweet. (2019). 
  # A coherent statistical model for coastal flood frequency
  # analysis under nonstationary sea level conditions
  
  # History
  # 5/24/2021 (DMR): Function created following Ghanbari et al., 2019

  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  
  z0 <- z # save for later
  
  z <- z - u # z is relative to the GPD threshold
  
  z <- pmax(z,0) # > 0 only b/c GPD is only defined at or above threshold (i.e., z=0)
  
  if ( shape == 0 ){ 
    
    result <-  ( 1 - phi ) + phi * mapply( function(x) 1 - exp(-1*x/scale), z)
    
  }else if( shape < 0 ){ 
    
    z <- pmin(z, .99999* -scale/shape) # values stay within the support of the GPD
    result <- ( 1 - phi ) + phi * mapply( function(x) 1 - exponent( 1 + (shape*x /scale), -1/shape), z)
    
  }else{
    
    result <- ( 1 - phi ) + phi * mapply( function(x) 1 - exponent( 1 + (shape*x /scale), -1/shape), z)
    
  }

  # Put flood heights below GPD threshold on a Normal distribution
  
  indx <- which( z0 < u ) # z is relative to the GPD threshold, so all negative z's get put on a Normal
  
  # NOTE: norm_mu will get shifted due to SLR...
  result[indx] <- ( 1 - phi )* pnorm(z0[indx], norm_mu, norm_sigma)/ pnorm(u, norm_mu, norm_sigma)

  return(result)
    
}
