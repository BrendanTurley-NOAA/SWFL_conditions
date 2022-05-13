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
lons <- lon[ind_lon[seq(1,length(ind_lon),2)]]-360
lats <- lat[ind_lat[seq(1,length(ind_lat),2)]]
lonlat <- expand.grid(lons,lats)
names(lonlat) <- c('lon','lat')

z <- ncvar_get(data,'depth')

### 48-hour mean
n <- 2 
n <- n*8

### temperature
# surface
temp_surf_now <- ncvar_get(data,'water_temp',
                           start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                           count=c(length(ind_lon),length(ind_lat),1,1+n))
temp_surf_now <- apply(temp_surf_now,c(1,2),mean,na.rm=T)
# @ 50m
temp_50m_now <- ncvar_get(data,'water_temp',
                           start=c(ind_lon[1],ind_lat[1],which(z==50),length(time)-n),
                           count=c(length(ind_lon),length(ind_lat),1,1+n))
temp_50m_now <- apply(temp_50m_now,c(1,2),mean,na.rm=T)
# bottom
temp_bot_now <- ncvar_get(data,'water_temp_bottom',
                          start=c(ind_lon[1],ind_lat[1],length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1+n))
temp_bot_now <- apply(temp_bot_now,c(1,2),mean,na.rm=T)

### salinity
# surface
sal_surf_now <- ncvar_get(data,'salinity',
                          start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1,1+n))
sal_surf_now <- apply(sal_surf_now,c(1,2),mean,na.rm=T)
# surface
sal_50m_now <- ncvar_get(data,'salinity',
                          start=c(ind_lon[1],ind_lat[1],which(z==50),length(time)-n),
                          count=c(length(ind_lon),length(ind_lat),1,1+n))
sal_50m_now <- apply(sal_50m_now,c(1,2),mean,na.rm=T)
# bottom
sal_bot_now <- ncvar_get(data,'salinity_bottom',
                         start=c(ind_lon[1],ind_lat[1],length(time)-n),
                         count=c(length(ind_lon),length(ind_lat),1+n))
sal_bot_now <- apply(sal_bot_now,c(1,2),mean,na.rm=T)

### currents
# surface
u_surf_now <- ncvar_get(data,'water_u',
                       start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                       count=c(length(ind_lon),length(ind_lat),1,1+n))
u_surf_now <- apply(u_surf_now,c(1,2),mean,na.rm=T)
v_surf_now <- ncvar_get(data,'water_v',
                       start=c(ind_lon[1],ind_lat[1],1,length(time)-n),
                       count=c(length(ind_lon),length(ind_lat),1,1+n))
v_surf_now <- apply(v_surf_now,c(1,2),mean,na.rm=T)
uv_surf_now <- sqrt(u_surf_now^2 + v_surf_now^2)
u_surf_nows <- u_surf_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
v_surf_nows <- v_surf_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
uv_surf_now_sub <- uv_surf_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
# bottom
u_bot_now <- ncvar_get(data,'water_u_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
u_bot_now <- apply(u_bot_now,c(1,2),mean,na.rm=T)
v_bot_now <- ncvar_get(data,'water_v_bottom',
                   start=c(ind_lon[1],ind_lat[1],length(time)-n),
                   count=c(length(ind_lon),length(ind_lat),1+n))
v_bot_now <- apply(v_bot_now,c(1,2),mean,na.rm=T)
uv_bot_now <- sqrt(u_bot_now^2 + v_bot_now^2)
u_bot_nows <- u_bot_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
v_bot_nows <- v_bot_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]
uv_bot_now_sub <- uv_bot_now[seq(1,length(ind_lon),2),seq(1,length(ind_lat),2)]

nc_close(data)

# ### bathymetry
# b_url <- 'https://tds.hycom.org/thredds/dodsC/datasets/GLBy0.08/expt_93.0/topo/depth_GLBy0.08_09m11.nc'
# data <- nc_open(b_url)
# bathy <- ncvar_get(data,'bathymetry',
#                    start=c(ind_lon[1],ind_lat[1],1),
#                    count=c(length(ind_lon),length(ind_lat),1))
# nc_close(data)

### stratification
# sal_strat_now <- (sal_bot_now-sal_surf_now)/bathy
# temp_strat_now <- (temp_bot_now-temp_surf_now)/bathy

# sal_strat_now <- (sal_bot_now-sal_surf_now)
# temp_strat_now <- (temp_bot_now-temp_surf_now)

sal_50m_now[which(is.na(sal_50m_now))] <- sal_bot_now[which(is.na(sal_50m_now))]
sal_strat_now <- (sal_50m_now-sal_surf_now)

temp_50m_now[which(is.na(temp_50m_now))] <- temp_bot_now[which(is.na(temp_50m_now))]
temp_strat_now <- (temp_50m_now-temp_surf_now)


### breaks and colors
quant <- .99
### temperature
# surface
temp_surf_breaks <- pretty(temp_surf_now,n=20)
temp_surf_cols <- temp_col(length(temp_surf_breaks)-1)
# bottom
temp_bot_now[which(temp_bot_now<10)] <- 10
temp_bot_breaks <- pretty(temp_bot_now,n=20)
temp_bot_cols <- temp_col(length(temp_bot_breaks)-1)

### salinity
# surface
sal_surf_breaks <- pretty(sal_surf_now,n=20)
sal_surf_cols <- sal_col(length(sal_surf_breaks)-1)
# bottom
sal_bot_breaks <- pretty(sal_bot_now,n=20)
sal_bot_cols <- sal_col(length(sal_bot_breaks)-1)

### currents
# surface
uv_surf_breaks <- pretty(uv_surf_now,n=20)
uv_surf_cols <- uv_col(length(uv_surf_breaks)-1)
# bottom
uv_bot_breaks <- pretty(uv_bot_now,n=20)
uv_bot_cols <- uv_col(length(uv_bot_breaks)-1)

### stratification
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

### plots
setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_surf_now.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_surf_now,breaks=temp_surf_breaks,col=temp_surf_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(temp_surf_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_surf_nows)/2,
       lonlat$lat+as.vector(v_surf_nows)/2,
       length = .025,
       col=alpha(1,(as.vector(uv_surf_now_sub)/max(uv_surf_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Surface Temperature (',degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_surf_now,breaks=sal_surf_breaks,col=sal_surf_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(sal_surf_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_surf_nows)/2,
       lonlat$lat+as.vector(v_surf_nows)/2,
       length = .025,
       col=alpha(1,(as.vector(uv_surf_now_sub)/max(uv_surf_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext('Surface Salinity (PSU)',adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()

setwd('~/Documents/R/Github/SWFL_conditions/figures')
png('hycom_bottom_now.png', height = 6, width = 11, units = 'in', res=300)
par(mfrow=c(1,2),mar=c(5,5,2,1),oma=c(1,1,1,1.5))
imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          temp_bot_now,breaks=temp_bot_breaks,col=temp_bot_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(temp_bot_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot_nows),
       lonlat$lat+as.vector(v_bot_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_now_sub)/max(uv_bot_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Bottom Temperature (',degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_bot_now,breaks=sal_bot_breaks,col=sal_bot_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(sal_bot_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_bot_nows),
       lonlat$lat+as.vector(v_bot_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_bot_now_sub)/max(uv_bot_now_sub,na.rm=T))))
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
       lonlat$lon+as.vector(u_surf_nows),
       lonlat$lat+as.vector(v_surf_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_surf_now_sub)/max(uv_surf_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
# mtext(expression(paste('Temperature stratification (',degree,'C m'^-1,')')),adj=1)
mtext(expression(paste('Temperature stratification (',Delta, degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sal_strat_now,
          breaks=sstrat_breaks,col=sstrat_cols,asp=1,
          xlab='',ylab='',las=1,
          nlevel=length(sal_cols),legend.mar=5)
plot(world,col='gray70',add=T)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u_surf_nows),
       lonlat$lat+as.vector(v_surf_nows),
       length = .025,
       col=alpha(1,(as.vector(uv_surf_now_sub)/max(uv_surf_now_sub,na.rm=T))))
contour(topo_lon,topo_lat,topo,
        add=T,levels=c(-200,-100,-50,-25,-10),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
# mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
# mtext(expression(paste('Salinity stratification (PSU m'^-1,')')),adj=1)
mtext(expression(paste('Salinity stratification (',Delta,'PSU)')),adj=1)
mtext(paste('Processed: ',as.Date(Sys.time())),
      line=4,side=1,col='red',font=2,adj=1,cex=1,outer=F)
dev.off()

### stratification colorbar explanation
strat_ex <- seq(-.3,.3,.01)
strat_exm <- cbind(strat_ex,strat_ex)
exp_breaks <- pretty(strat_ex,n=10)

exp_cols <- c(strat_n_col(length(which(exp_breaks<0))),
              strat_p_col(length(which(exp_breaks>0))))

png('stratification_example.png',width=5,height=2,units='in',res=300)
par(mar=c(4.5,1,4,1))
image(strat_exm,
      breaks=exp_breaks,
      col=exp_cols,
      xaxt='n',yaxt='n')
mtext('warmer surface or\nsaltier surface',adj=0)
mtext('cooler surface or\nfresher surface',adj=1)
dev.off()