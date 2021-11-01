insert_minor <- function(major_labs, n_minor) {labs <- 
  c( sapply( major_labs, function(x) c(x, rep("", 1) ) ) )}


plotGPDNExceed <- function( df, historical, filo){
  
  library(ggplot2)
  library(scales)
  
  
  xmax <- ceiling(tail(df$height,n=1000)[which.min(abs(tail(df$freq,n=1000)-1e-3))])
  
  if (xmax <= 3){
    xmax <- 4
  }
  
  lty=c("solid","dashed","solid","dashed")
  colors = c("grey","grey","grey","grey")
  lwd = c(1.5,.5,.5,.5)
  breaks=c("Historic ESL Return Curve")
           
  
  p <- ggplot(df)  + 
    geom_line(data=df, aes(x=height,y=freq,colour=name,lty=name,size=name))+
    geom_point(data=historical, aes(x=z,y=freq,shape=group),size=5,color="black",
               stroke=1.5,alpha=0.5) +
    scale_size_manual("",values=lwd,guide=FALSE) +
    scale_linetype_manual("",breaks=breaks,values=lty) +
    scale_shape_manual("",values=1) +
    scale_color_manual("",breaks=breaks,values=colors) +
    guides(colour = guide_legend(override.aes = list(lwd=1.5))) +
    
    theme_bw(base_size = 27) + annotation_logticks(sides = "lr", size=1,
    short = unit(0.3, "cm"), mid = unit(0.3, "cm"), long = unit(0.5, "cm")) + 
    scale_x_continuous(breaks=seq(0,xmax,by=.5),limits=c(0,xmax),
                       labels=head(insert_minor(seq(0,xmax,by=1),1),-1),expand = c(0,0)) + 
    scale_y_log10(breaks=c(100,10,1,0.1,0.01,0.001),
                  labels=trans_format("log10", scales::math_format(10^.x)),expand = c(0,0)) +
    coord_cartesian(ylim=c(1e-3, 100)) +
    theme(legend.justification = c(1, 1), legend.position = c(1,-3),
        legend.key = element_blank(), legend.text.align = 0,
        legend.direction="vertical", legend.text=element_text(size=18),
        legend.key.height = unit(1.5, 'lines'), legend.key.width = unit(3.5, 'lines'),
        panel.grid.minor = element_blank(), aspect.ratio=.5,
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        axis.line = element_line(colour = 'black', size = .5),
        axis.ticks = element_line(colour = "black", size = .25),
        axis.text.x = element_text(size=27), axis.text.y = element_text(size=27),
        plot.margin=unit(c(1,1,5,0.5),"cm"),
        legend.key.size = unit(2, 'lines')) + labs(title=df$loc,
        x="Extreme Sea Level (m)",y="Expected Events per Year")
  
  ggsave(filo,p,width=11, height=8.5)

  
  
}
