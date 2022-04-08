rm(list=ls())
gc()

library(fields)
library(ncdf4)
library(raster)
library(rgdal)
library(scales)


setwd("~/Desktop/professional/biblioteca/data")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

################## geographic scope
lonbox_e <- -80 ### Florida Bay
lonbox_w <- -87 ### mouth of Mississippi River
latbox_n <- 31 ### northern coast
latbox_s <- 24 ### remove the Keys

ind_lat <- which(topo_lat<latbox_n & topo_lat>latbox_s)
ind_lon <- which(topo_lon<lonbox_e & topo_lon>lonbox_w)

topo_lat <- topo_lat[ind_lat]
topo_lon <- topo_lon[ind_lon]
topo <- topo[ind_lon,ind_lat]

### load map
setwd("~/Desktop/professional/biblioteca/data/shapefiles/gshhg-shp-2.3.7/GSHHS_shp/h/")
world <- readOGR('GSHHS_h_L1.shp')
world <- crop(world, extent(-87, -79, 24, 31))

### colorpalettes
### breaks and colors
temp_col <- colorRampPalette(c('gray20','purple','darkorange','gold'))
sal_col <- colorRampPalette(c('purple4','dodgerblue4','seagreen3','khaki1'))
uv_col <- colorRampPalette(c('white','thistle1','purple2'))
# lm_neg <- colorRampPalette(c(1,'dodgerblue4','lightskyblue1','white'))
# lm_pos <- colorRampPalette(c('white','mistyrose2','firebrick3'))
lm_neg <- colorRampPalette(c('dodgerblue4','deepskyblue3','lightskyblue1','gray95'))
lm_pos <- colorRampPalette(c('gray95','rosybrown1','tomato2','red4'))
col_sd <- colorRampPalette(c('gray20','dodgerblue4','indianred3','gold1'))
strat_n_col <- colorRampPalette(c('purple4','purple2','orchid1','gray90'))
strat_p_col <- colorRampPalette(rev(c('darkgreen','green3','palegreen2','gray90')))

################## geogrpahic scope
lonbox_e <- -80.5 ### Florida Bay
lonbox_e <- (lonbox_e + 360)
lonbox_w <- -86 ### mouth of Mississippi River
lonbox_w <- (lonbox_w + 360)
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.5 ### remove the Keys

# https://tds.hycom.org/thredds/catalogs/GLBy0.08/expt_93.0.html
### now
url <- 'https://tds.hycom.org/thredds/dodsC/GLBy0.08/expt_93.0'
data <- nc_open(url)

time <- ncvar_get(data,'time')
time2 <- as.Date(time/24,origin='2000-01-01 00:00:00')
time3 <- as.POSIXct(time*3600,origin='2000-01-01',tz='GMT')
# time2 <- as.Date(time/24,origin='2022-02-25 12:00:00')
# time3 <- as.POSIXct(time*3600,origin='2022-02-25 12:00:00',tz='GMT')

lat <- ncvar_get(data,'lat')
ind_lat <- which(lat>=latbox_s & lat<=latbox_n)
lon <- ncvar_get(data,'lon')
ind_lon <- which(lon>=lonbox_w & lon<=lonbox_e)

z <- ncvar_get(data,'depth')

n <- 2 # 48-hour mean
n <- n*8
sal_bot_now <- ncvar_get(data,'salinity_bottom',
                         start=c(ind_lon[1],ind_lat[1],length(time)-n),
                         count=c(length(ind_lon),length(ind_lat),1+n))
sal_bot_now <- apply(sal_bot_now,c(1,2),mean,na.rm=T)

temp_bot_now <- ncvar_get(data,'water_temp_bottom',
                          start=c(ind_lon[1],ind_lat[1],length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1+n))
temp_bot_now <- apply(temp_bot_now,c(1,2),mean,na.rm=T)

sal_surf_now <- ncvar_get(data,'salinity',
                         start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                         count=c(length(ind_lon),length(ind_lat),1,1+n))
sal_surf_now <- apply(sal_surf_now,c(1,2),mean,na.rm=T)

temp_surf_now <- ncvar_get(data,'water_temp',
                          start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1,1+n))
temp_surf_now <- apply(temp_surf_now,c(1,2),mean,na.rm=T)

u_now <- ncvar_get(data,'water_u_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
u_now <- apply(u_now,c(1,2),mean,na.rm=T)

v_now <- ncvar_get(data,'water_v_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
v_now <- apply(v_now,c(1,2),mean,na.rm=T)

uv_now <- sqrt(u_now^2 + v_now^2)
u_nows <- u_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
v_nows <- v_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
uv_now_sub <- uv_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
lons <- lon[ind_lon[seq(1,length(ind_lon),2)]]-360
lats <- lat[ind_lat[seq(1,length(ind_lat),2)]]
lonlat <- expand.grid(lons,lats)
names(lonlat) <- c('lon','lat')

n <- 7 # 7-day regression
n <- n*8
time2[c((length(time)-n),length(time))]
sal_bot <- ncvar_get(data,'salinity_bottom',
                     start=c(ind_lon[1],ind_lat[1],length(time)-n),
                     count=c(length(ind_lon),length(ind_lat),1+n))
sal_bsd <- apply(sal_bot,c(1,2),sd,na.rm=T)

temp_bot <- ncvar_get(data,'water_temp_bottom',
                      start=c(ind_lon[1],ind_lat[1],length(time)-n),
                      count=c(length(ind_lon),length(ind_lat),1+n))
temp_bsd <- apply(temp_bot,c(1,2),sd,na.rm=T)

u_bot <- ncvar_get(data,'water_u_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
u_bot <- apply(u_bot,c(1,2),mean,na.rm=T)

v_bot <- ncvar_get(data,'water_v_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
v_bot <- apply(v_bot,c(1,2),mean,na.rm=T)

uv_bot <- sqrt(u_bot^2 + v_bot^2)
u_bot <- u_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
v_bot <- v_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
uv_bot_sub <- uv_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]

nc_close(data)

### bathymetry
b_url <- 'https://tds.hycom.org/thredds/dodsC/datasets/GLBy0.08/expt_93.0/topo/depth_GLBy0.08_09m11.nc'
data <- nc_open(b_url)
bathy <- ncvar_get(data,'bathymetry',
                   start=c(ind_lon[1],ind_lat[1],1),
                   count=c(length(ind_lon),length(ind_lat),1))
nc_close(data)

sal_strat_now <- (sal_bot_now-sal_surf_now)/bathy
temp_strat_now <- (temp_bot_now-temp_surf_now)/bathy

### breaks and colors
quant <- .99
sal_breaks <- pretty(sal_bot_now,n=20)
sal_cols <- sal_col(length(sal_breaks)-1)
temp_bot_now[which(temp_bot_now<10)] <- 10
temp_breaks <- pretty(temp_bot_now,n=20)
temp_cols <- temp_col(length(temp_breaks)-1)
uv_breaks <- pretty(uv_now,n=20)
uv_cols <- uv_col(length(uv_breaks)-1)
uv_breaks2 <- pretty(uv_bot,n=20)
uv_cols2 <- uv_col(length(uv_breaks2)-1)
tsd_breaks <- pretty(temp_bsd[which(temp_bsd<=quantile(temp_bsd,quant,na.rm=T))],n=20)
tsd_cols <- col_sd(length(tsd_breaks)-1)
temp_bsd[which(temp_bsd>tsd_breaks[length(tsd_breaks)])] <- tsd_breaks[length(tsd_breaks)]
ssd_breaks <- pretty(sal_bsd[which(sal_bsd<=quantile(sal_bsd,quant,na.rm=T))],n=20)
ssd_cols <- col_sd(length(ssd_breaks)-1)
sal_bsd[which(sal_bsd>ssd_breaks[length(ssd_breaks)])] <- ssd_breaks[length(ssd_breaks)]
sstrat_breaks <- pretty(sal_strat_now[which(sal_strat_now<=quantile(sal_strat_now,quant,na.rm=T))],n=20)
if(any(sstrat_breaks==0)){
  limit <- mean(abs(range(sal_strat_now,na.rm=T)))
  sal_strat_now[which(sal_strat_now<(-limit))] <- -limit
  sal_strat_now[which(sal_strat_now>limit)] <- limit
  sstrat_breaks <- pretty(sal_strat_now,n=20)
  sstrat_cols <- c(strat_n_col(length(which(sstrat_breaks<0))),
                   strat_p_col(length(which(sstrat_breaks>0))))
}else{
  sstrat_cols <- rev(strat_p_col(length(sstrat_breaks)-1))  
  sal_strat_now[which(sal_strat_now>sstrat_breaks[length(sstrat_breaks)])] <- sstrat_breaks[length(sstrat_breaks)]
  sal_strat_now[which(sal_strat_now<sstrat_breaks[1])] <- sstrat_breaks[1]
}
# sal_strat_now[which(sal_strat_now>sstrat_breaks[length(sstrat_breaks)])] <- sstrat_breaks[length(sstrat_breaks)]
# sal_strat_now[which(sal_strat_now<sstrat_breaks[1])] <- sstrat_breaks[1]
tstrat_breaks <- pretty(temp_strat_now[which(temp_strat_now<=quantile(temp_strat_now,quant,na.rm=T))],n=20)
if(any(tstrat_breaks==0)){
  limit <- mean(abs(range(temp_strat_now,na.rm=T)))
  temp_strat_now[which(temp_strat_now<(-limit))] <- -limit
  temp_strat_now[which(temp_strat_now>limit)] <- limit
  tstrat_breaks <- pretty(temp_strat_now,n=20)
  tstrat_cols <- c(strat_n_col(length(which(tstrat_breaks<0))),
                   strat_p_col(length(which(tstrat_breaks>0))))
}else{
  tstrat_cols <- strat_n_col(length(tstrat_breaks)-1)
  temp_strat_now[which(temp_strat_now>tstrat_breaks[length(tstrat_breaks)])] <- tstrat_breaks[length(tstrat_breaks)]
  temp_strat_now[which(temp_strat_now<tstrat_breaks[1])] <- tstrat_breaks[1]
}
# temp_strat_now[which(temp_strat_now>tstrat_breaks[length(tstrat_breaks)])] <- tstrat_breaks[length(tstrat_breaks)]
# temp_strat_now[which(temp_strat_now<tstrat_breaks[1])] <- tstrat_breaks[1]

### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_now.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_bot_now,breaks=temp_breaks,col=temp_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(temp_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_nows),
       lonlat$lat+as.vector(v_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_now_sub)/max(uv_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Bottom Temperature (',degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_bot_now,breaks=sal_breaks,col=sal_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(sal_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_nows),
       lonlat$lat+as.vector(v_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_now_sub)/max(uv_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('Bottom Salinity (PSU)',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()


setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_strat_now.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_strat_now,
          breaks=tstrat_breaks,col=tstrat_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(temp_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_nows),
       lonlat$lat+as.vector(v_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_now_sub)/max(uv_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Temperature stratification (',degree,'C m'^-1,')')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_strat_now,
          breaks=sstrat_breaks,col=sstrat_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(sal_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_nows),
       lonlat$lat+as.vector(v_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_now_sub)/max(uv_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Salinity stratification (PSU m'^-1,')')),adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()

# imagePlot(lon[ind_lon]-360,
#           lat[ind_lat],
#           uv_now,breaks=uv_breaks,col=uv_cols,asp=1)
# plot(world,col='gray70',add=T)
# arrows(lonlat$lon,
#        lonlat$lat,
#        lonlat$lon+as.vector(u_nows),
#        lonlat$lat+as.vector(v_nows),
#        length = .025,
#        col=alpha(1,(as.vector(uv_now_sub)/max(uv_now_sub,na.rm=T))))
# contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')

# imagePlot(lon[ind_lon]-360,
#           lat[ind_lat],
#           uv_bot,breaks=uv_breaks2,col=uv_cols2,asp=1)
# plot(world,col='gray70',add=T)
# arrows(lonlat$lon,
#        lonlat$lat,
#        lonlat$lon+as.vector(u_bot),
#        lonlat$lat+as.vector(v_bot),
#        length = .025,
#        col=alpha(1,(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
# contour(topo_lon,topo_lat,topo,
#         add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')



### 7 day linear trend
sal_lm <- sal_p <- matrix(NA,length(ind_lon),length(ind_lat))
for(i in 1:length(ind_lon)){
  for(j in 1:length(ind_lat)){
    if(all(!is.na(sal_bot[i,j,]))){
      res <- lm(sal_bot[i,j,]~c(1:dim(sal_bot)[3]))
      sal_lm[i,j] <- coefficients(summary(res))[2]
      sal_p[i,j] <- coefficients(summary(res))[8]
    }
  }
}
plot(sal_lm,sal_p)
limit <- round(ifelse(abs(range(sal_lm,na.rm=T)[1])>abs(range(sal_lm,na.rm=T)[2]),
                abs(range(sal_lm,na.rm=T)[2]),
                abs(range(sal_lm,na.rm=T)[1])),2)
sal_lm[which(sal_lm<(-limit))] <- -limit
sal_lm[which(sal_lm>limit)] <- limit
# sal_lm[which(sal_lm<(-.05))] <- -.05
# sal_lm[which(sal_lm>.05)] <- .05
sal_lm2 <- sal_lm
sal_lm2[which(sal_p>.1)] <- NA


temp_lm <- temp_p <- matrix(NA,length(ind_lon),length(ind_lat))
for(i in 1:length(ind_lon)){
  for(j in 1:length(ind_lat)){
    if(all(!is.na(temp_bot[i,j,]))){
      res <- lm(temp_bot[i,j,]~c(1:dim(temp_bot)[3]))
      temp_lm[i,j] <- coefficients(summary(res))[2]
      temp_p[i,j] <- coefficients(summary(res))[8]
    }
  }
}
plot(temp_lm,temp_p)
limit <- round(ifelse(abs(range(temp_lm,na.rm=T)[1])>abs(range(temp_lm,na.rm=T)[2]),
                      abs(range(temp_lm,na.rm=T)[2]),
                      abs(range(temp_lm,na.rm=T)[1])),2)
temp_lm[which(temp_lm<(-limit))] <- -limit
temp_lm[which(temp_lm>limit)] <- limit
# temp_lm[which(temp_lm<(-.05))] <- -.05
# temp_lm[which(temp_lm>.05)] <- .05
temp_lm2 <- temp_lm
temp_lm2[which(temp_p>.1)] <- NA


### breaks and colors
# sal_lm_brks <- seq(-.05,.05,.005)
sal_lm_brks <- pretty(sal_lm,20)
sal_lm_cols <- c(lm_neg(length(which(sal_lm_brks<0))),
                 lm_pos(length(which(sal_lm_brks>0))))

# temp_lm_brks <- seq(-.05,.05,.005)
temp_lm_brks <- pretty(temp_lm,20)
temp_lm_cols <- c(lm_neg(length(which(temp_lm_brks<0))),
                  lm_pos(length(which(temp_lm_brks>0))))


### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_lin.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_lm2,
          asp=1,breaks=temp_lm_brks,col=temp_lm_cols,
          xlab='',ylab='',las=1,
          nlevel=length(temp_lm_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('7-day change in bottom Temperature (',degree,'C)')),adj=1)
mtext(paste('7-day period: ',time2[(length(time)-n)],'-',time2[length(time)]),
      line=2,side=3,col='red',font=2,adj=0,cex=1,outer=F)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_lm2,
          asp=1,breaks=sal_lm_brks,col=sal_lm_cols,
          xlab='',ylab='',las=1,
          nlevel=length(sal_lm_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('7-day change in bottom Salinity (PSU)',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()


### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_sd.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_bsd,breaks=tsd_breaks,col=tsd_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(tsd_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('7-day Bottom Temperature (',degree,'C) standard deviation')),adj=1)
mtext(paste('7-day period: ',time2[(length(time)-n)],'-',time2[length(time)]),
      line=2,side=3,col='red',font=2,adj=0,cex=1,outer=F)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_bsd,breaks=ssd_breaks,col=ssd_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(ssd_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('7-day Bottom Salinity (PSU) standard deviation',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()


### forecast
f_url <- 'https://tds.hycom.org/thredds/dodsC/GLBy0.08/expt_93.0/FMRC/GLBy0.08_930_FMRC_best.ncd'

data <- nc_open(f_url)

origin <- substr(data$var$time_offset$units,13,100)
# time <- ncvar_get(data,'time_offset')

time <- ncvar_get(data,'time')
time2 <- as.Date(time/24,origin=origin)
time3 <- as.POSIXct(time*3600,origin=origin,tz='GMT')


lat <- ncvar_get(data,'lat')
ind_lat <- which(lat>=latbox_s & lat<=latbox_n)
lon <- ncvar_get(data,'lon')
ind_lon <- which(lon>=lonbox_w & lon<=lonbox_e)

n <- 7
n <- n*8
time2[c((length(time)-n),length(time))]
sal_bot <- ncvar_get(data,'salinity_bottom',
                     start=c(ind_lon[1],ind_lat[1],length(time)-n),
                     count=c(length(ind_lon),length(ind_lat),1+n))
sal_bsd <- apply(sal_bot,c(1,2),sd,na.rm=T)

temp_bot <- ncvar_get(data,'water_temp_bottom',
                      start=c(ind_lon[1],ind_lat[1],length(time)-n),
                      count=c(length(ind_lon),length(ind_lat),1+n))
temp_bsd <- apply(temp_bot,c(1,2),sd,na.rm=T)

u_bot <- ncvar_get(data,'water_u_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
u_bot <- apply(u_bot,c(1,2),mean,na.rm=T)

v_bot <- ncvar_get(data,'water_v_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
v_bot <- apply(v_bot,c(1,2),mean,na.rm=T)

uv_bot <- sqrt(u_bot^2 + v_bot^2)
u_bot <- u_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
v_bot <- v_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
uv_bot_sub <- uv_bot[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]

nc_close(data)

### breaks and colors
quant <- .99
uv_breaks2 <- pretty(uv_bot,n=20)
uv_cols2 <- uv_col(length(uv_breaks2)-1)
tsd_breaks <- pretty(temp_bsd[which(temp_bsd<=quantile(temp_bsd,quant,na.rm=T))],n=20)
tsd_cols <- col_sd(length(tsd_breaks)-1)
temp_bsd[which(temp_bsd>tsd_breaks[length(tsd_breaks)])] <- tsd_breaks[length(tsd_breaks)]
ssd_breaks <- pretty(sal_bsd[which(sal_bsd<=quantile(sal_bsd,quant,na.rm=T))],n=20)
ssd_cols <- col_sd(length(ssd_breaks)-1)
sal_bsd[which(sal_bsd>ssd_breaks[length(ssd_breaks)])] <- ssd_breaks[length(ssd_breaks)]


### 7 day linear trend
sal_lm <- sal_p <- matrix(NA,length(ind_lon),length(ind_lat))
for(i in 1:length(ind_lon)){
  for(j in 1:length(ind_lat)){
    if(all(!is.na(sal_bot[i,j,]))){
      res <- lm(sal_bot[i,j,]~c(1:dim(sal_bot)[3]))
      sal_lm[i,j] <- coefficients(summary(res))[2]
      sal_p[i,j] <- coefficients(summary(res))[8]
    }
  }
}
plot(sal_lm,sal_p)
limit <- round(ifelse(abs(range(sal_lm,na.rm=T)[1])>abs(range(sal_lm,na.rm=T)[2]),
                      abs(range(sal_lm,na.rm=T)[2]),
                      abs(range(sal_lm,na.rm=T)[1])),2)
sal_lm[which(sal_lm<(-limit))] <- -limit
sal_lm[which(sal_lm>limit)] <- limit
# sal_lm[which(sal_lm<(-.05))] <- -.05
# sal_lm[which(sal_lm>.05)] <- .05
sal_lm2 <- sal_lm
sal_lm2[which(sal_p>.05)] <- NA


temp_lm <- temp_p <- matrix(NA,length(ind_lon),length(ind_lat))
for(i in 1:length(ind_lon)){
  for(j in 1:length(ind_lat)){
    if(all(!is.na(temp_bot[i,j,]))){
      res <- lm(temp_bot[i,j,]~c(1:dim(temp_bot)[3]))
      temp_lm[i,j] <- coefficients(summary(res))[2]
      temp_p[i,j] <- coefficients(summary(res))[8]
    }
  }
}
plot(temp_lm,temp_p)
limit <- round(ifelse(abs(range(temp_lm,na.rm=T)[1])>abs(range(temp_lm,na.rm=T)[2]),
                      abs(range(temp_lm,na.rm=T)[2]),
                      abs(range(temp_lm,na.rm=T)[1])),2)
temp_lm[which(temp_lm<(-limit))] <- -limit
temp_lm[which(temp_lm>limit)] <- limit
# temp_lm[which(temp_lm<(-.05))] <- -.05
# temp_lm[which(temp_lm>.05)] <- .05
temp_lm2 <- temp_lm
temp_lm2[which(temp_p>.05)] <- NA


### breaks and colors
### breaks and colors
# sal_lm_brks <- seq(-.05,.05,.005)
sal_lm_brks <- pretty(sal_lm,20)
sal_lm_cols <- c(lm_neg(length(which(sal_lm_brks<0))),
                 lm_pos(length(which(sal_lm_brks>0))))

# temp_lm_brks <- seq(-.05,.05,.005)
temp_lm_brks <- pretty(temp_lm,20)
temp_lm_cols <- c(lm_neg(length(which(temp_lm_brks<0))),
                  lm_pos(length(which(temp_lm_brks>0))))


### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_lin_for.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_lm2,
          asp=1,breaks=temp_lm_brks,col=temp_lm_cols,
          xlab='',ylab='',las=1,
          nlevel=length(temp_lm_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('7-day change in bottom Temperature (',degree,'C)')),adj=1)
mtext(paste('7-day period: ',time2[(length(time)-n)],'-',time2[length(time)]),
      line=2,side=3,col='red',font=2,adj=0,cex=1,outer=F)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_lm2,
          asp=1,breaks=sal_lm_brks,col=sal_lm_cols,
          xlab='',ylab='',las=1,
          nlevel=length(sal_lm_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('7-day change in bottom Salinity (PSU)',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()


### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_sd_for.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_bsd,breaks=tsd_breaks,col=tsd_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(tsd_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('7-day Bottom Temperature (',degree,'C) standard deviation')),adj=1)
mtext(paste('7-day period: ',time2[(length(time)-n)],'-',time2[length(time)]),
      line=2,side=3,col='red',font=2,adj=0,cex=1,outer=F)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_bsd,breaks=ssd_breaks,col=ssd_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(ssd_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot),
       lonlat$lat+as.vector(v_bot),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_sub)/max(uv_bot_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('7-day Bottom Salinity (PSU) standard deviation',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()



### stratification
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          (temp_surf_now-temp_bot_now)/bathy,asp=1)
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          (sal_surf_now-sal_bot_now)/bathy,asp=1)
