DeclusterTide <- function(tide_rel_to_mhhw,q99){
  
  dc_tide_rel_to_mhhw <- cluster3(tide_rel_to_mhhw,q99,replace.with=-999)
  
  # append list if necessary
  diff = length(tide_rel_to_mhhw) - length(dc_tide_rel_to_mhhw)
  
  if (diff > 0){
    dc_tide_rel_to_mhhw <- c(dc_tide_rel_to_mhhw,rep(-999,diff))
  }else{
    dc_tide_rel_to_mhhw <- dc_tide_rel_to_mhhw[1:length(tide_rel_to_mhhw)]
  }
  return(dc_tide_rel_to_mhhw)
}

cluster3<-function(set,threshold,replace.with)
{
  x<-list()
  z<-list()
  j<-1
  {
    for(i in (4):length(set))
    {
      if(set[i-3]>threshold
         & set[i-2]<=threshold
         & set[i-1]<=threshold
         & set[i]<=threshold)
      {
        
        x<-max(set[j:i])
        ifelse(i !=length(set), j<-i+1, NA)
        z<-as.numeric(c(z,x))
      }else{
        z<-as.numeric(c(z,-999))
      }
      
    }
    
  }
  z
}