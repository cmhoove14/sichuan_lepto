#Lepto PPA

require(spatstat)
require(rgdal)
require(maptools)
require(geostatsp)
require(raster)

#Read in spatial data ##############
# setwd("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/SL_ST/SL_ST/years/")
#Projection to use (UTM zone 47N, WGS'84)
prj<-CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# cases<-readShapePoints('total_lepto_china_reduced', proj4string = prj)
cc<-read.csv('../sichuan_database/cases_add.csv')

cases04<-readShapePoints('../sichuan_database/2004casepoints', proj4string = prj)
cases05<-readShapePoints('../sichuan_database/2005casepoints', proj4string = prj)
cases06<-readShapePoints('../sichuan_database/2006casepoints', proj4string = prj)
cases07<-readShapePoints('../sichuan_database/2007casepoints', proj4string = prj)
cases08<-readShapePoints('../sichuan_database/2008casepoints', proj4string = prj)
cases09<-readShapePoints('../sichuan_database/2009casepoints', proj4string = prj)
cases10<-readShapePoints('../sichuan_database/2010casepoints', proj4string = prj)
cases11<-readShapePoints('../sichuan_database/2011casepoints', proj4string = prj)
cases12<-readShapePoints('../sichuan_database/2012casepoints', proj4string = prj)
cases13<-readShapePoints('../sichuan_database/2013casepoints', proj4string = prj)
cases14<-readShapePoints('../sichuan_database/2014casepoints', proj4string = prj)

#Sichuan shapefile as owin for compatibility with spatstat
sich<-readShapeSpatial('../sichuan_database/Sichuan', proj4string = prj)
sichspat<-as.owin(sich)
#cases for each year and all cases from 2004-2014 at the end
cases<-ppp(cc[,4], cc[,3], window = sichspat)


