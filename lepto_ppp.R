#Lepto PPA

require(spatstat)
require(rgdal)
require(maptools)
require(geostatsp)
require(raster)

#Read in spatial data ##############
setwd("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/SL_ST/SL_ST/")
#Projection to use (UTM zone 47N, WGS'84)
prj<-CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

cc<-read.csv('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/cases_XY_UTM.csv')

#Sichuan shapefile as owin for compatibility with spatstat
sich<-readShapeSpatial('years/Sichuan', proj4string = prj)
sichspat<-as.owin(sich)

#cases for each year and all cases from 2004-2014 at the end
cases<-ppp(cc[,17], cc[,18], window = sichspat) #, marks = cc[,15])

plot(sichspat, hatch = TRUE)
plot(cases, add=TRUE, pch = 16, col = 'red', cex = 0.6)

#read in rasters
pop10<-raster('tiffs/NASA_pop_2010_prj.tif')
pop10im<-as.im.RasterLayer(pop10)
pop10.001<-raster('tiffs/NASA_pop_2010.001_prj.tif')
pop10.001im<-as.im.RasterLayer(pop10.001)
worldpop<-raster('tiffs/worldpop.tif') 
worldpopim<-as.im.RasterLayer(worldpop)
twi<-raster('tiffs/SAGA_TWI1.tif')
twiim<-as.im.RasterLayer(twi)
dem<-raster('tiffs/SRTM_90m_prj.tif')
demim<-as.im.RasterLayer(dem)
lndcv<-raster('tiffs/globcvr_prj.tif')
lndcvim<-as.im.RasterLayer(lndcv)  

#ppm with rasters above
offset.nasa10.001<-ppm(cases ~ offset(log(pop10.001im)))
print(offset.nasa10.001)
offset.wp<-ppm(cases ~ offset(log(worldpopim)))  
print(offset.wp)

dem.mod<-ppm(cases ~ offset(log(worldpopim)) + demim)  
print(dem.mod)

twi.mod<-ppm(cases ~ offset(log(worldpopim)) + twiim) 
print(twi.mod)



