library(fields)
library(ncdf4)

temp_col <- colorRampPalette(c('gray20','purple','darkorange','gold'))
anom_neg <- colorRampPalette(c('dodgerblue4','deepskyblue3','lightskyblue1','gray95'))
anom_pos <- colorRampPalette(c('gray95','rosybrown1','tomato2','red4'))

################## geogrpahic scope
lonbox_e <- -79 ### Florida Bay
lonbox_e <- (lonbox_e + 360)
lonbox_w <- -99 ### mouth of Mississippi River
lonbox_w <- (lonbox_w + 360)
latbox_n <- 31 ### northern coast
latbox_s <- 17.5 ### remove the Keys

### https://www.ncei.noaa.gov/products/optimum-interpolation-sst
url <- 'https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/202205/oisst-avhrr-v02r01.20220522_preliminary.nc'
data <- nc_open(url)

time <- ncvar_get(data,'time')
time2 <- as.Date(time,origin='1978-01-01 12:00:00')

z <- ncvar_get(data,'zlev')

lat <- ncvar_get(data,'lat')
ind_lat <- which(lat>=latbox_s & lat<=latbox_n)
ind_lat <- ind_lat-1
lon <- ncvar_get(data,'lon')
ind_lon <- which(lon>=lonbox_w & lon<=lonbox_e)
ind_lon <- ind_lon-1

sst <- ncvar_get(data,'sst',
                 start=c(ind_lon[1],ind_lat[1],1,1),
                 count=c(length(ind_lon),length(ind_lat),1,1))

anom <- ncvar_get(data,'anom',
                  start=c(ind_lon[1],ind_lat[1],1,1),
                  count=c(length(ind_lon),length(ind_lat),1,1))

nc_close(data)

### breaks and colors
# sst
sst_brks <- pretty(sst,n=30)
sst_cols <- temp_col(length(sst_brks)-1)
# anom
# anom_brks <- pretty(anom,30)
anom_brks <- seq(-2,2,.1)
anom[which(anom<(-2))] <- -2
anom[which(anom>2)] <- 2

anom_cols <- c(anom_neg(length(which(anom_brks<0))),
               anom_pos(length(which(anom_brks>0))))

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          sst,
          asp=1,breaks=sst_brks,col=sst_cols,
          xlab='',ylab='',las=1,
          nlevel=length(sst_cols),legend.mar=5)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Surface Temperature (',degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          anom,
          asp=1,breaks=anom_brks,col=anom_cols,
          xlab='',ylab='',las=1,
          nlevel=length(anom_cols),legend.mar=5)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Surface Temperature Anomaly (',degree,'C)')),adj=1)
