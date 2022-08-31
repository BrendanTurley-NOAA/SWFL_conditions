### one script to rule them all; but...

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

### FL cities
setwd("~/Desktop/professional/projects/postdoc_FL/data/")
fl <- read.csv('fl_cities.csv')
fl <- fl[c(1,2,4,6),]

### colorpalettes
### breaks and colors
temp_col <- colorRampPalette(c('gray20','purple','darkorange','gold'))
sal_col <- colorRampPalette(c('midnightblue','dodgerblue4','seagreen3','khaki1'))
uv_col <- colorRampPalette(c('white','thistle1','purple2'))
lm_neg <- colorRampPalette(c('dodgerblue4','deepskyblue3','lightskyblue1','gray95'))
lm_pos <- colorRampPalette(c('gray95','rosybrown1','tomato2','red4'))
col_sd <- colorRampPalette(c('gray20','dodgerblue4','indianred3','gold1'))
strat_n_col <- colorRampPalette(c('purple4','purple2','orchid1','gray90'),interpolate='spline',bias=.9)
strat_p_col <- colorRampPalette(rev(c('darkgreen','green3','palegreen2','gray90')),interpolate='spline',bias=.9)

################## geogrpahic scope
lonbox_e <- -80.5 ### Florida Bay
lonbox_e <- (lonbox_e + 360)
lonbox_w <- -86 ### mouth of Mississippi River
lonbox_w <- (lonbox_w + 360)
latbox_n <- 30.5 ### northern coast
latbox_s <- 24.5 ### remove the Keys

### bathymetry
b_url <- 'https://tds.hycom.org/thredds/dodsC/datasets/GLBy0.08/expt_93.0/topo/depth_GLBy0.08_09m11.nc'
data <- nc_open(b_url)
bathy <- ncvar_get(data,'bathymetry',
                   start=c(ind_lon[1],ind_lat[1],1),
                   count=c(length(ind_lon),length(ind_lat),1))
nc_close(data)

### 48-hour mean conditions
source("~/Documents/R/Github/SWFL_conditions/R_code/hycom_now.R", echo=TRUE)
### past 7-day conditions
source("~/Documents/R/Github/SWFL_conditions/R_code/hycom_7dm.R", echo=TRUE)
### forecasted 7-day conditions
source("~/Documents/R/Github/SWFL_conditions/R_code/hycom_7dp.R", echo=TRUE)
