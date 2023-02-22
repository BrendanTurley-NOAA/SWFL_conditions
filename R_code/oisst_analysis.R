
library(rerddap)
library(fields)
library(lubridate)
library(ncdf4)
library(scales)
library(rgdal)

erddap_extract <- function(data, info, parameter){
  data_temp <- data$data
  ind_extract <- which(names(data_temp)==parameter)
  time_step <- unique(data_temp$time)
  lon <- data$summary$dim$longitude$vals
  lat <- data$summary$dim$latitude$vals
  
  new_data <- array(data_temp[,ind_extract], 
                    c(length(lon),
                      length(lat),
                      length(time_step)))
  
  row_ind <- which(info$alldata$NC_GLOBAL$attribute_name=='title')
  col_ind <- which(colnames(info$alldata$NC_GLOBAL)=='value')
  name <- info$alldata$NC_GLOBAL[row_ind,col_ind]
  name <- unlist(strsplit(name,split=','))
  return(list(data = new_data,
              lon = lon,
              lat = lat,
              time = time_step,
              name = name))
  # setClass('erddap',slots=c(data='matrix',lon='array',lat='array'))
  # return(new('erddap',data=new_data,lon=lon,lat=lat))
  # return(list(new_data,lon,lat))
}

setwd("~/Desktop/professional/biblioteca/data")
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

################## geographic scope
lonbox_e <- -79 ### Florida Bay
lonbox_w <- -99 ### mouth of Mississippi River
latbox_n <- 31 ### northern coast
latbox_s <- 17.5 ### remove the Keys

ind_lat <- which(topo_lat<latbox_n & topo_lat>latbox_s)
ind_lon <- which(topo_lon<lonbox_e & topo_lon>lonbox_w)

topo_lat <- topo_lat[ind_lat]
topo_lon <- topo_lon[ind_lon]
topo <- topo[ind_lon,ind_lat]

### load map
setwd("~/Desktop/professional/biblioteca/data/shapefiles/gshhg-shp-2.3.7/GSHHS_shp/h/")
world <- readOGR('GSHHS_h_L1.shp')

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

### erddap

# sst_pull <- info('ncdcOisst21Agg_LonPM180')
sst_pull <- info('ncdcOisst21Agg')

latitude = c(latbox_s, latbox_n)
longitude = c(lonbox_w, lonbox_e)
# time <- '2022-07-13T12:00:00Z'

anom_grab <- griddap(sst_pull,
                     time=c('last-14','last'),
                     zlev=c(0,0),
                     latitude=latitude,
                     longitude=longitude,
                     fields='anom')

sst_grab <- griddap(sst_pull,
                     time=c('last-14','last'),
                     zlev=c(0,0),
                     latitude=latitude,
                     longitude=longitude,
                     fields='sst')

lon <- sort(unique(anom_grab$data$lon))
lat <- sort(unique(anom_grab$data$lat))

sst_1 <- erddap_extract(sst_grab,sst_pull,'sst')

imagePlot(sst_1$data[,,1])

### breaks and colors
# sst
# sst_brks <- pretty(sst,n=30)
# sst_cols <- temp_col(length(sst_brks)-1)
# anom
# anom_brks <- pretty(anom,30)

anom <- erddap_extract(anom_grab,sst_pull,'anom')
anom_m <- anom$data[,,1]

anom_brks <- seq(-3,3,.1)
anom_m[which(anom_m<(-3))] <- -3
anom_m[which(anom_m>3)] <- 3

anom_cols <- c(anom_neg(length(which(anom_brks<0))),
               anom_pos(length(which(anom_brks>0))))

imagePlot(lon,
          lat,
          anom_m,
          asp=1,breaks=anom_brks,col=anom_cols,
          xlab='',ylab='',las=1,
          nlevel=length(anom_cols),legend.mar=5)



### https://www.ncei.noaa.gov/products/optimum-interpolation-sst
url <- 'https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/202206/oisst-avhrr-v02r01.20220607.nc'
data <- try(nc_open(url))

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

# Which OSCAR file?
as.integer(Sys.time()-as.POSIXct('1992-10-05'))

### every 5 days
as.Date(10837,origin='1992-10-05')

### https://podaac.jpl.nasa.gov/dataset/OSCAR_L4_OC_third-deg?ids=Keywords:Projects&values=Oceans:Ocean%20Circulation::OSCAR&provider=PODAAC
url <- 'https://opendap.jpl.nasa.gov/opendap/OceanCirculation/oscar/preview/L4/oscar_third_deg/oscar_vel10837.nc.gz'
data <- try(nc_open(url))

time <- ncvar_get(data,'time')
time2 <- as.Date(time,origin='1992-10-05 00:00:00')

lat2 <- ncvar_get(data,'latitude')
ind_lat2 <- which(lat2>=latbox_s & lat2<=latbox_n)
ind_lat2 <- ind_lat2-1
lon2 <- ncvar_get(data,'longitude')
ind_lon2 <- which(lon2>=lonbox_w & lon2<=lonbox_e)
ind_lon2 <- ind_lon2-1

u <- ncvar_get(data,'u',
                 start=c(ind_lon2[1],ind_lat2[1],1,1),
                 count=c(length(ind_lon2),length(ind_lat2),1,1))
u <- u[,ncol(u):1]

v <- ncvar_get(data,'v',
                  start=c(ind_lon2[1],ind_lat2[1],1,1),
                  count=c(length(ind_lon2),length(ind_lat2),1,1))
v <- v[,ncol(v):1]

nc_close(data)

uv <- sqrt(u^2 + v^2)
lonlat <- expand.grid(lon2[ind_lon2]-360,rev(lat2[ind_lat2]))
names(lonlat) <- c('lon','lat')

imagePlot(lon2[ind_lon2]-360,
          rev(lat2[ind_lat2]),
          uv)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u)/abs(as.vector(uv))/5,
       lonlat$lat+as.vector(v)/abs(as.vector(uv))/5,
       length = .025,
       col=alpha(1,(as.vector(uv)/max(uv,na.rm=T))))

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
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u)/abs(as.vector(uv))/5,
       lonlat$lat+as.vector(v)/abs(as.vector(uv))/5,
       length = .025,
       col=alpha(1,(as.vector(uv)/max(uv,na.rm=T))))
plot(world,col='gray70',add=T)
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Surface Temperature (',degree,'C)')),adj=1)

imagePlot(lon[ind_lon]-360,
          lat[ind_lat],
          anom,
          asp=1,breaks=anom_brks,col=anom_cols,
          xlab='',ylab='',las=1,
          nlevel=length(anom_cols),legend.mar=5)
arrows(lonlat$lon,
       lonlat$lat,
       lonlat$lon+as.vector(u)/abs(as.vector(uv))/5,
       lonlat$lat+as.vector(v)/abs(as.vector(uv))/5,
       length = .025,
       col=alpha(1,(as.vector(uv)/max(uv,na.rm=T))))
plot(world,col='gray70',add=T)
contour(topo_lon,topo_lat,topo,add=T,levels=c(-200),col='gray40')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
mtext(expression(paste('Surface Temperature Anomaly (',degree,'C)')),adj=1)


### create daily plots with current YTD versus mean, max, and min