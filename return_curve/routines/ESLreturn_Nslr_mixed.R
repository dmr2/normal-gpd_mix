ESLreturn_Nslr_mixed <- function( z, u, scale, shape, phi, norm_mu, norm_sigma, 
                                  slr_samps, maxinterpN){

  # Function "ESLreturn_Nslr_mixed
  #
  # Calculates the expected number of extreme sea level (ESL) events while 
  # sampling uncertainty in sea-level rise and Generalized Pareto 
  # Distribution parameters.
  
  # Calculate log of the number of exceedances of z from a
  # Poisson-Generalized Pareto Distribution with Poisson mean
  # lambda and the specified shape and scale factor. Assumes
  # the exceedance threshold has already been removed from z.
  #
  # This function ensures that values returned stay within the
  # support of the GPD.
  #
  # For values of z below zero, the function will treat as though
  # z = 0 unless MHHW is specified. If MHHW is specified
  # value, exceedances below zero will be assumed to fall on a
  # Gumbel distribution between lambda exceedances at zero and a
  # specified value at z = MHHW(1) < 0. If MHHW(2) exists, it is
  # the value of exceedances at z = MHHW(1); otherwise, will default
  # to 365.25/2.
  
  # Reference:
  # M.K. Buchanan, R.E. Kopp, M. Oppenheimer, and C. Tebaldi. (2016).
  # Allowances for evolving coastal flood risk under uncertain local
  # sea-level rise. Climatic Change.
  
  # History
  # 8/11/2017 (DMR): Function created
  #
  
  library(pracma) # needed for the error function, erf(x)
  
  eps <- .Machine$double.eps # Machine epsilon

  zminusSLR <- mapply(function(x) x - slr_samps, z)
    
  if(length(shape) == 1){ # Don't sample GPD uncertainty 
    
    result <- ESLreturn_N_mixed(zminusSLR,u,scale,shape,phi,norm_mu,norm_sigma)
    
  }else{
    
    zminusSLR <- zminusSLR - u # zminusSLR is relative to the GPD threshold
    
    minz <- .99999*abs(scale/shape)
    
    # interpolate to handle z of different sizes
    testz <- unique(sort(c(0, as.vector(zminusSLR)))) 
    if( length(testz) > maxinterpN ){
      testz <- seq(testz[1]-eps,testz[length(testz)]+eps,length=maxinterpN)
    }else if( length(testz) == 1 ){
      testz <- c(testz-eps, testz+eps)
    }
  
    testz0 <- testz
    a <- mapply(FUN=function(x)(shape>=0)*x,testz) 
    b <- matrix(mapply(FUN=function(x,y)min(x,y),mapply(rep,times=1000,testz),minz),ncol=1000) 
    testz <- a + b*(shape<0) 
    
    a2 <- (shape+eps)/scale 
    a3 <- matrix(pmax(0,testz),nrow=1000,ncol=1000)
    a <- 1 + (a3*a2) 
    b <- -1/(shape+eps)
    logNref <- log(365.25 * phi * mapply(FUN=function(x,y) (abs(x)^y) * sign(x), a, b)) 
    logENref = log(apply(exp(matrix(logNref,nrow=1000,ncol=1000)),2,mean))

    f <- approxfun(testz0,logENref)
    result <- exp( f(pmax(0,zminusSLR)) )
    
    # Put ESL events below GPD threshold on a Normal distribution
    
    norm_mu_plus_slr <- norm_mu + slr_samps
    gpd_thresh_plus_slr <- u + slr_samps
    
    zz <- rep(z, each=10000)
    
    p1 <- (1 - phi) * (1 + erf( (zz - norm_mu_plus_slr) / (norm_sigma * sqrt(2)) ))
    p2 <- ( 1 - p1/(1 + erf( (gpd_thresh_plus_slr - norm_mu_plus_slr) / (norm_sigma * sqrt(2)) )))
    p3 <- as.vector( p2 )*365.25
    
    result <- ( zminusSLR < 0 ) * p3 + ( zminusSLR >= 0 )*result
    
    return( result )
  }
  
}