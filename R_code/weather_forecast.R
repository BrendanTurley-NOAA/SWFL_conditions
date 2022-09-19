
### https://www.e-education.psu.edu/meteo810/content/l4_p4.html

library(fields)
library(rNOMADS)

model.list <- NOMADSRealTimeList("grib")
model.list <- NOMADSRealTimeList("dods")

#get the available dates and urls for this model (14 day archive)
model.urls <- GetDODSDates("gfs_0p50")
# model.urls <- GetDODSDates("gdas_0p25")
# model.urls <- GetDODSDates("rtofs")

latest.model <- tail(model.urls$url, 1)

# Get the model runs for a date
model.runs <- GetDODSModelRuns(latest.model)

latest.model.run <- tail(model.runs$model.run, 1)

# Get info for a particular model run
model.run.info <- GetDODSModelRunInfo(latest.model, latest.model.run)

# Type model.run.info at the command prompt 
#    to see the information

# OR You can search the file for specific model 
#   variables such as temperature: 
#   model.run.info[grep("temp", model.run.info)]
# variable <- "tmp2m"
variable <- "tmpsig995"
# variable <- "ugrd10m"
time <- c(0, 0) # Analysis run, index starts at 0
lon <- c(0, 719) # All 720 longitude points (it's total_points -1)
lat <- c(0, 360) # All 361 latitude points
lev <- NULL      # DO NOT include level if variable is non-level type

print("getting data...")
model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, levels=lev, lon, lat)

# reorder the lat/lon so that the maps work
model.data$lon<-ifelse(model.data$lon>180,model.data$lon-360,model.data$lon)

# regrid the data
model.grid <- ModelGrid(model.data, c(0.5, 0.5), model.domain = c(-100,-79,31,18) )

# replace out-of-bounds values
model.grid$z <- ifelse (model.grid$z>99999,NA,model.grid$z)

#convert data to Fahrenheit
model.grid$z[1,1,,]=(model.grid$z[1,1,,]-273.15)*9/5+32

par(pin=c(3,3))

# image.plot is a function from the fields library
# It automatically adds a legend
imagePlot(model.grid$x, sort(model.grid$y), model.grid$z[1,1,,],
           xlab = "Longitude", ylab = "Latitude",
           main = paste("Surface Temperature",
                        model.grid$fcst.date),asp=1)
