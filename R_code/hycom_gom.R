library(ncdf4)
url <- 'https://tds.hycom.org/thredds/dodsC/GOMu0.04/expt_90.1m000/data/hindcasts/2023'
data <- nc_open(url)

time <- ncvar_get(data,'time')
datetime <- as.POSIXct(time*3600,origin='2000-01-01 00:00:00')
lon <- ncvar_get(data,'lon')
lat <- ncvar_get(data,'lat')
z <- ncvar_get(data,'depth')
u <- ncvar_get(data,'water_u',start=c(1,1,1,1),count=c(-1,-1,1,1))
v <- ncvar_get(data,'water_v',start=c(1,1,1,1),count=c(-1,-1,1,1))
temp <- ncvar_get(data,'water_temp',start=c(1,1,1,1),count=c(-1,-1,1,1))
sal<- ncvar_get(data,'salinity',start=c(1,1,1,1),count=c(-1,-1,1,1))
ssh <- ncvar_get(data,'surf_el',start=c(1,1,1),count=c(-1,-1,1))


image(lon,lat,log(sqrt(u^2+v^2)),asp=1)
image(lon,lat,ssh,asp=1)
image(lon,lat,temp,asp=1)
image(lon,lat,sal,asp=1)


