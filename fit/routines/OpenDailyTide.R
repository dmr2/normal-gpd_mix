OpenDailyTide <- function(dir1,type,basin,gauge,gauge_dat,is_UH){
     
     i <- which(gauge_dat$Gauge==gauge)
     
     y1d <- gauge_dat$Record_Start_Day[i]
     y2d <- gauge_dat$Record_End_Day[i]
     
     if(is_UH){
             
             fn <- paste(type,"_tide_",basin,"_",gauge,"_",y1d,"-",y2d,".tsv",sep="")
             
     }else{
             
             fn <- paste(type,"_tide_",gauge,"_",gauge_dat$Short_Name[i],"_",y1d,"-",y2d,".tsv",sep="")
             
     }
     
     daily_fil <- paste(dir1,fn,sep="/")
     
     print(paste("Opening: ",daily_fil))
     daily <- read.csv(daily_fil,sep="\t",header=TRUE,quote="\"",comment.char = "")
     
     return( daily )

}