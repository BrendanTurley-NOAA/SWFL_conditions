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

### now
url <- 'https://tds.hycom.org/thredds/dodsC/GLBy0.08/expt_93.0/sur'
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


n <- 2 # 48-hour mean
n <- n*8
mld_now <- ncvar_get(data,'mixed_layer_thickness',
                         start=c(ind_lon[1],ind_lat[1],length(time)-n),
                         count=c(length(ind_lon),length(ind_lat),1+n))
mld_now <- apply(mld_now,c(1,2),mean,na.rm=T)

nc_close(data)

