
rm(list=ls())
library(mapplots)
library(mapdata)
library(rgdal)
load('L:/StockBooks/_StockBook2020/maps/VMS/Landings2019/LandingsEffort2019.Rdata')
eezWorld <- readOGR("L:/StockBooks/Stock Books Historic/StockBook 2018/Maps/VMS/PropSpecies","eez_boundaries") # marineregions v9
eezWorld1 <- subset(eezWorld,Line_type%in%c('Treaty','200 NM','Connection line','Median line','Unilateral claim (undisputed)'))
rm(eezWorld)

col.sea <- "#245372" #'lightcyan'
col.land <- "#7D86A1" #'cornsilk'

#col.sea <- 'lightcyan'
#col.land <- 'darkolivegreen1'

legend.grid0 <- function (x, y = NULL, breaks, col, suffix = "", type = 1, 
                          pch = 15, pt.cex = 2.5, bg = "lightblue", ...) 
{
  ncol <- length(breaks) - 1
  legend <- signif(((breaks[(ncol):1]+breaks[(ncol + 1):2]))/2, 2)
  legend(x, y, legend = legend, col = col[ncol:1], pch = pch, 
         pt.cex = pt.cex, bg = bg, ...)
}


####
species <- c('Anf','Sol','Whb','Boc','Cod','Had','Hke','Her','Jax','Lin'
             ,'Mac','Lez','Ple','Pol','Pok','Spr'
             ,'Whg')
#species <- c('Cod','Jax')

for(s in 1:length(species)){
  sp <- species[s]
  message(sp)
  
  #  if(sp%in%c('Alb','Bft','Boc','Whb','Her','Mac','Jax','Spr','Bft','Pil','Swo')) {byx <- 0.18;  byy <- 0.12} else {byx <- 0.09;  byy <- 0.06}
  if(sp%in%c('Alb','Bft','Boc','Whb','Her','Mac','Jax','Spr','Pil','Swo')) {byx <- 0.09;  byy <- 0.06} else {byx <- 0.03;  byy <- 0.02} 
  
  vms$land <- vms[,paste0('LiveWt',sp)] / (cos(vms$Lat*pi/180)*byx*60*1.852*byy*60*1.852)/1 # mean land per km2 per year
  grd1 <- with(vms, make.grid(.0001+Lon,.0001+Lat,land,byx,byy,c(-19.5,7.5),c(35,65.5)) )
  grd1 <- ifelse(grd1==0,NA,grd1)
  if(sum(vms$land,na.rm=T)==0) breaks1=c(0,0) else breaks1 <- breaks.grid(ifelse(grd1==0,NA,grd1),0.975,zero=T)
  ncol <- length(breaks1) - 1
  col1 <- colorRampPalette(c("white", "yellow", "orange", "red", "brown4"))(ncol)
  
  ####save all images in LandingsDistribution folder
  png(paste0('R/LandingsDistribution/Land',toupper(sp),'.png'),2.75,3.25,'in',6,res=600) #3.5,4,'in',6,res=600)
  par(mar=c(1,1,0.1,0.1),lwd=0.5)
  basemap(xlim=c(-16,-3),ylim=c(48.5,57.5),xaxt='n',yaxt='n',ann=F,bg=col.sea)
  if(sp%in%c('Alb','Bft'))  basemap(xlim=c(-21,-1),ylim=c(45.5,55),xaxt='n',yaxt='n',ann=F,bg=col.sea)
  if(sp%in%c('Boc'))  basemap(xlim=c(-16,-3),ylim=c(47.5,58.5),xaxt='n',yaxt='n',ann=F,bg=col.sea)
  if(sp%in%c('Jax','Mac'))  basemap(xlim=c(-12,1),ylim=c(49.5,61),xaxt='n',yaxt='n',ann=F,bg=col.sea)
  if(sp%in%c('Pil'))  basemap(xlim=c(-16,-3)+3,ylim=c(48.5,57.75),xaxt='n',yaxt='n',ann=F,bg=col.sea)
  
  axis(1,-30:-1,labels=paste0(30:1,'?W'),lwd=0.5,tcl=-0.15,padj=-2.25,cex.axis=0.7)
  axis(1,0:10,labels=paste0(0:10,'?E'),lwd=0.5,tcl=-0.15,padj=-2.25,cex.axis=0.7)
  axis(2,30:70,labels=paste0(30:70,'?N'),lwd=0.5,tcl=-0.15,padj=2,cex.axis=0.7)
  draw.grid(grd1,breaks1,col1)
  plot(eezWorld1,lwd=0.25,add=T,col='grey')
  map('worldHires',fill=T,col=col.land,border=NA, add=T)
  title <- expression(paste('kg/',km^2))
  if(sp%in%c('Alb','Bft')) x <- 'topleft' else x <- 'bottomright'
  legend.grid0(x,breaks=breaks1,col=col1,title=title,inset=0.02,bg=col.sea,text.col='grey',box.col='grey')
  dev.off()
  
}



