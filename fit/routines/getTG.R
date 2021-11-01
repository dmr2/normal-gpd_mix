getTG <- function(fil1,fil2){
  
  dat1 = read.csv(fil1,sep="\t",header=TRUE,quote="\"",comment.char = "") # month
  dat2 = read.csv(fil2,sep="\t",header=TRUE,quote="\"",comment.char = "") # daily
  
  dat1$QA_Pass = toupper(dat1$QA_Pass)
  dat2$QA_Pass = toupper(dat2$QA_Pass)
  
  # Get tide gauges that meet both monthly and daily completion critera
  gauge_list <- intersect(dat2$Gauge[dat2$QA_Pass == TRUE],dat1$Gauge[dat1$QA_Pass == TRUE])
  
  # get inforamtion for these gauges
  
  indx1 <- match(gauge_list,dat1$Gauge)
  indx2 <- match(gauge_list,dat2$Gauge)
  
  dat <- data.frame(Gauge=tolower(gauge_list),
                    PSMSL=dat1$PSMSL_ID,
                    Short_Name=dat1$Short_Name[indx1],
                    Lat=dat1$Lat[indx1],
                    Lon=dat1$Lon[indx1],
                    Long_Name=dat1$Long_Name[indx1],
                    Region=dat1$Region[indx1],
                    Record_Start_Mon=dat1$Record_Start[indx1],
                    Record_End_Mon=dat1$Record_End[indx1],
                    Record_Start_Day=dat2$Record_Start[indx2],
                    Record_End_Day=dat2$Record_End[indx2],
                    QA_Record_Start_Day=dat2$QA_YR1[indx2],
                    QA_Record_End_Day=dat2$QA_YR2[indx2],
                    QA_Record_Start_Month=dat1$QA_YR1[indx1],
                    QA_Record_End_Month=dat1$QA_YR2[indx1])
  
  return( dat )
}

getTG2 <- function(fil){
  
  dat = read.csv(fil,sep="\t",header=TRUE,quote="\"",comment.char = "") # month
  
  dat$QA_Pass = toupper(dat$QA_Pass)
  
  # Get tide gauges that meet both monthly and daily completion critera
  gauge_list <- dat$Gauge[dat$QA_Pass == TRUE]
  
  # get inforamtion for these gauges
  indx <- match(gauge_list,dat$Gauge)
  
  dat <- data.frame(Gauge=tolower(gauge_list),
                    PSMSL=dat$PSMSL_ID,
                    Short_Name=dat$Short_Name[indx],
                    Lat=dat$Lat[indx],
                    Lon=dat$Lon[indx],
                    Long_Name=dat$Long_Name[indx],
                    Region=dat$Region[indx],
                    Basin=tolower(dat$Basin[indx]),
                    Record_Start_Day=dat$Record_Start[indx],
                    Record_End_Day=dat$Record_End[indx],
                    QA_Record_Start_Day=dat$QA_YR1[indx],
                    QA_Record_End_Day=dat$QA_YR2[indx],
                    Adj=dat$Adj[indx])
  
  return( dat )
  
}