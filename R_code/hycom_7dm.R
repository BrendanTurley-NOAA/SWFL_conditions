# rm(list=ls())
# gc()
# 
# library(fields)
# library(ncdf4)
# library(raster)
# library(rgdal)
# library(scales)
# 
# 
# setwd("~/Desktop/professional/biblioteca/data")
# bathy <- nc_open('etopo1.nc')
# topo <- ncvar_get(bathy, 'Band1')
# topo_lat <- ncvar_get(bathy, 'lat')
# topo_lon <- ncvar_get(bathy, 'lon')
# nc_close(bathy)
# 
# ################## geographic scope
# lonbox_e <- -80 ### Florida Bay
# lonbox_w <- -87 ### mouth of Mississippi River
# latbox_n <- 31 ### northern coast
# latbox_s <- 24 ### remove the Keys
# 
# ind_lat <- which(topo_lat<latbox_n & topo_lat>latbox_s)
# ind_lon <- which(topo_lon<lonbox_e & topo_lon>lonbox_w)
# 
# topo_lat <- topo_lat[ind_lat]
# topo_lon <- topo_lon[ind_lon]
# topo <- topo[ind_lon,ind_lat]
# 
# ### load map
# setwd("~/Desktop/professional/biblioteca/data/shapefiles/gshhg-shp-2.3.7/GSHHS_shp/h/")
# world <- readOGR('GSHHS_h_L1.shp')
# world <- crop(world, extent(-87, -79, 24, 31))
# 
# ### colorpalettes
# ### breaks and colors
# temp_col <- colorRampPalette(c('gray20','purple','darkorange','gold'))
# sal_col <- colorRampPalette(c('midnightblue','dodgerblue4','seagreen3','khaki1'))
# uv_col <- colorRampPalette(c('white','thistle1','purple2'))
# # lm_neg <- colorRampPalette(c(1,'dodgerblue4','lightskyblue1','white'))
# # lm_pos <- colorRampPalette(c('white','mistyrose2','firebrick3'))
# lm_neg <- colorRampPalette(c('dodgerblue4','deepskyblue3','lightskyblue1','gray95'))
# lm_pos <- colorRampPalette(c('gray95','rosybrown1','tomato2','red4'))
# col_sd <- colorRampPalette(c('gray20','dodgerblue4','indianred3','gold1'))
# strat_n_col <- colorRampPalette(c('purple4','purple2','orchid1','gray90'))
# strat_p_col <- colorRampPalette(rev(c('darkgreen','green3','palegreen2','gray90')))
# 
# ################## geogrpahic scope
# lonbox_e <- -80.5 ### Florida Bay
# lonbox_e <- (lonbox_e + 360)
# lonbox_w <- -86 ### mouth of Mississippi River
# lonbox_w <- (lonbox_w + 360)
# latbox_n <- 30.5 ### northern coast
# latbox_s <- 24.5 ### remove the Keys

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
### for uv plotting subsample
lons <- lon[ind_lon[seq(1,length(ind_lon),3)]]-360
lats <- lat[ind_lat[seq(1,length(ind_lat),3)]]
lonlat <- expand.grid(lons,lats)
names(lonlat) <- c('lon','lat')

z <- ncvar_get(data,'depth')


n <- 7 # past 7-day regression
n <- n*8
# time2[c((length(time)-n),length(time))]

### temperature
# surface
temp_surf_7dm <- ncvar_get(data,'water_temp',
                           start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                           count=c(length(ind_lon),length(ind_lat),1,1+n))
# temp_surf_7dm <- apply(temp_surf_7dm,c(1,2),mean,na.rm=T)
# bottom
temp_bot_7dm <- ncvar_get(data,'water_temp_bottom',
                          start=c(ind_lon[1],ind_lat[1],length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1+n))
# temp_bot_7dm <- apply(temp_bot_7d,c(1,2),mean,na.rm=T)
temp_bsd <- apply(temp_bot_7dm,c(1,2),sd,na.rm=T)

### salinity
# surface
sal_surf_7dm <- ncvar_get(data,'salinity',
                          start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1,1+n))
# sal_surf_7dm <- apply(sal_surf_7dm,c(1,2),mean,na.rm=T)
# bottom
sal_bot_7dm <- ncvar_get(data,'salinity_bottom',
                         start=c(ind_lon[1],ind_lat[1],length(time)-n),
                         count=c(length(ind_lon),length(ind_lat),1+n))
# sal_bot_7dm <- apply(sal_bot_7d,c(1,2),mean,na.rm=T)
sal_bsd <- apply(sal_bot_7dm,c(1,2),sd,na.rm=T)

### currents
# surface
u_surf_7dm <- ncvar_get(data,'water_u',
                        start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                        count=c(length(ind_lon),length(ind_lat),1,1+n))
u_surf_7dm <- apply(u_surf_7dm,c(1,2),mean,na.rm=T)
v_surf_7dm <- ncvar_get(data,'water_v',
                        start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                        count=c(length(ind_lon),length(ind_lat),1,1+n))
v_surf_7dm <- apply(v_surf_7dm,c(1,2),mean,na.rm=T)
uv_surf_7dm <- sqrt(u_surf_7dm^2 + v_surf_7dm^2)
u_surf_7dms <- u_surf_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]
v_surf_7dms <- v_surf_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]
uv_surf_7dm_sub <- uv_surf_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]
# bottom
u_bot_7dm <- ncvar_get(data,'water_u_bottom',
                       start=c(ind_lon[1],ind_lat[1],length(time)-n),
                       count=c(length(ind_lon),length(ind_lat),1+n))
u_bot_7dm <- apply(u_bot_7dm,c(1,2),mean,na.rm=T)
v_bot_7dm <- ncvar_get(data,'water_v_bottom',
                       start=c(ind_lon[1],ind_lat[1],length(time)-n),
                       count=c(length(ind_lon),length(ind_lat),1+n))
v_bot_7dm <- apply(v_bot_7dm,c(1,2),mean,na.rm=T)
uv_bot_7dm <- sqrt(u_bot_7dm^2 + v_bot_7dm^2)
u_bot_7dms <- u_bot_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]
v_bot_7dms <- v_bot_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]
uv_bot_7dm_sub <- uv_bot_7dm[seq(1,length(ind_lon),3),seq(1,length(ind_lat),3)]

nc_close(data)


### breaks and colors
quant <- .99
uv_breaks2 <- pretty(uv_bot_7dm,n=20)
uv_cols2 <- uv_col(length(uv_breaks2)-1)
tsd_breaks <- pretty(temp_bsd[which(temp_bsd<=quantile(temp_bsd,quant,na.rm=T))],n=30)
tsd_cols <- col_sd(length(tsd_breaks)-1)
temp_bsd[which(temp_bsd>tsd_breaks[length(tsd_breaks)])] <- tsd_breaks[length(tsd_breaks)]
ssd_breaks <- pretty(sal_bsd[which(sal_bsd<=quantile(sal_bsd,quant,na.rm=T))],n=30)
ssd_cols <- col_sd(length(ssd_breaks)-1)
sal_bsd[which(sal_bsd>ssd_breaks[length(ssd_breaks)])] <- ssd_breaks[length(ssd_breaks)]


### 7 day linear trend
sal_lm <- sal_p <- matrix(NA,length(ind_lon),length(ind_lat))
for(i in 1:length(ind_lon)){
  for(j in 1:length(ind_lat)){
    if(all(!is.na(sal_bot_7dm[i,j,]))){
      res <- lm(sal_bot_7dm[i,j,]~c(1:dim(sal_bot_7dm)[3]))
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
    if(all(!is.na(temp_bot_7dm[i,j,]))){
      res <- lm(temp_bot_7dm[i,j,]~c(1:dim(temp_bot_7dm)[3]))
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
sal_lm_brks <- pretty(sal_lm,30)
sal_lm_cols <- c(lm_neg(length(which(sal_lm_brks<0))),
                 lm_pos(length(which(sal_lm_brks>0))))

# temp_lm_brks <- seq(-.05,.05,.005)
temp_lm_brks <- pretty(temp_lm,30)
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
       lonlat$lon+as.vector(u_bot_7dms),
       lonlat$lat+as.vector(v_bot_7dms),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_7dm_sub)/max(uv_bot_7dm_sub,na.rm=T))))
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
       lonlat$lon+as.vector(u_bot_7dms),
       lonlat$lat+as.vector(v_bot_7dms),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_7dm_sub)/max(uv_bot_7dm_sub,na.rm=T))))
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
       lonlat$lon+as.vector(u_bot_7dms),
       lonlat$lat+as.vector(v_bot_7dms),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_7dm_sub)/max(uv_bot_7dm_sub,na.rm=T))))
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
       lonlat$lon+as.vector(u_bot_7dms),
       lonlat$lat+as.vector(v_bot_7dms),
       length = .025,
       col=alpha('gray50',(as.vector(uv_bot_7dm_sub)/max(uv_bot_7dm_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('7-day Bottom Salinity (PSU) standard deviation',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()

