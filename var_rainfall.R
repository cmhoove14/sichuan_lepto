#' ---
#' title: "Analysis of correlations between SMR and covariates"
#' author: "Karina Cucchi"
#' date: "November 28th, 2016"
#' ---
#' 
#' In this script I load and format rainfall data 
#' for further exploration of relationship between lepto and rainfall data.
#' 

## load packages
library(raster) # for rasters
library(ncdf4) # for netCDF files
library(sp) # for spatial objects
library(rgdal) # for spatial objects

source('utils.R')

#'
#' # Load township data #
#'

load('RData/townships_sir.RData')
proj4string(townships)

#' 
#' # Load rainfall satellite data
#' 

load('../sichuan_hydroDB/satellite_products/processed/Sichuan_2003-2015_CMORPH.RData')
proj4string(r)

# convert raster to utm projection (this is townships' projection)
r <- projectRaster(r, crs = proj4string(townships))

# plot townships and raster on top of each other
plot(r[[5]])
plot(townships,add=T,axes=T)

plot(r[[5]],xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
plot(townships,add=T,axes=T)

#' 
#' # Extract yearly data by township #
#' 
#' In each township, extract total rainfall.

# first convert to mm
# data is in mm/h, multiply by 24
r <- r * 24

# The extract function is quite slow.
# First, sum by year over raster 
# so that I end up with 13 years going from 2003 to 2015

# prepare for summation by year
# extract unique index corresponding to year
r_dates <- as.Date(names(r),format = "X%Y.%m.%d")
r_years <- as.numeric(format(r_dates,format="%Y"))
# get indices
r_yearIdx <- as.numeric(as.factor(r_years))
# get years correspnding to indices
r_yearNames <- levels(as.factor(r_years))

# actually sum the layers by year
r_byYear <- stackApply(r, r_yearIdx, fun = sum)
names(r_byYear) <- r_yearNames


# plot rainfall for year 2003
plot(r_byYear[["X2003"]],legend=FALSE, axes=FALSE,
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
plot(townships,add=T,axes=T)
# add legend
plot(r_byYear[["X2003"]], legend.only=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=pretty(values(r_byYear[["X2003"]])),
                    labels=pretty(values(r_byYear[["X2003"]])), 
                    cex.axis=0.6),
     legend.args=list(text='rainfall [mm]', side=4, font=2, line=2.5, cex=0.8))
title("2003")

# plot on smaller region 
plot(r_byYear[["X2003"]],legend=FALSE, axes=T,
     xlim=c(5e5,7.8e5),ylim=c(3.4e6,3.55e6))
plot(townships,add=T,axes=T)
# add legend
plot(r_byYear[["X2003"]], legend.only=TRUE,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=pretty(values(r_byYear[["X2003"]])),
                    labels=pretty(values(r_byYear[["X2003"]])), 
                    cex.axis=0.6),
     legend.args=list(text='rainfall [mm]', side=4, font=2, line=2.5, cex=0.8))
title("2003")
myScalebar(units_label = 10000,"10 km",xleft = 7e5)

# extract yearly rainfall at each township
r_byTwnshp <- extract(r_byYear, townships,fun="mean",na.rm=TRUE,method="simple")
colnames(r_byTwnshp) <- r_yearNames

# add in township dataframe
townships$cmorph_yearlymm <- r_byTwnshp

save(townships,file = "RData/townships_cmorph.RData")
