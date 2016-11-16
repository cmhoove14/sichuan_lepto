#' ---
#' title: "Exploratory analysis of Sichuan lepto data"
#' author: "Karina Cucchi"
#' date: "November 10th, 2016"
#' ---

source('utils_colorbar.R')

library(fields) # used here for color scheme
library(sp) # convenient format for spatial dataset
library(maptools) # for importing shapefiles

#' # Load data #
#' 
#' ## Lepto dataset ##

lep<-read.csv('../sichuan_database/cases_XY_UTM.csv',stringsAsFactors = F)
names(lep)

coordinates(lep) <- c("POINT_X", "POINT_Y") # Convert dataframe to SpatialPointsDataFrame

# some points have coordinates 0,0
# get rid of them

idx_zeros <- which(lep$x1 == 0 | lep$y1 == 0)
lep <- lep[-idx_zeros,]
plot(lep,pch=19)

#' ## Sichuan shapefile ##
#' 
#Projection to use (UTM zone 47N, WGS'84)
prj<-CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
sichuan <- readShapePoly('../sichuan_database/from_gdb/Sichuan.shp',
                         proj4string = prj)
plot(sichuan,col='grey')

#' # Spatio-temporal investigation #
#' 
#' 
#' ## plot with color changing with year ##
#' 
pal <- tim.colors(length(unique(lep$year_diag)))
intcols <- pal[lep$year_diag - 2003]
layout(matrix(c(1,2),nrow=1), widths=c(4,1))
plot(sichuan,col='grey')
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

#'
#' ## plot only year 2005 ##
#' 
layout(matrix(1))
plot_year=2005
col_year=pal[plot_year - 2003]
plot(sichuan,col='grey')
plot(lep[which(lep$year_diag==plot_year),],
     col = col_year, pch = 19,add=T,cex=0.5)
title(paste('year',plot_year))

#'
#' ## plot frequency per year of diagnosis ##
#' 

#+ fig.width=8, fig.height=4
counts_years <- table(lep$year_diag)
barplot(counts_years, main="", xlab="Years")

#'
#' ## plot frequency per month of diagnosis ##
#' 

#+ fig.width=8, fig.height=4
counts_months <- table(lep$month_diag)
barplot(counts_months, main="", xlab="Months")

#'
#' ## plot timeseries per year, monthly average ##
#' 

all_years = unique(lep$year_diag)
all_months=1:12
counts_yearMonth <- array(NA,
                          dim=c(length(all_years),length(all_months)))
for(iy in 1:length(all_years)){
  for(im in 1:length(all_months)){
    counts_yearMonth[iy,im] <- sum(lep$year_diag==all_years[iy] &
                                     lep$month_diag==all_months[im])
  }
}

layout(matrix(c(1,2),nrow=1), widths=c(3,1))
plot(NA,xlim=range(all_months),ylim=range(counts_yearMonth),
     xlab='months',ylab='counts',main='time series of lepto diagnosis')
for(iy in 1:length(all_years)){
  lines(x=all_months,y=counts_yearMonth[iy,],col=pal[iy])
}
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

#'
#' ## plot timeseries per year, weekly average ##
#' 

all_years = unique(lep$year_diag)
all_weeks=1:54
counts_yearWeek <- array(NA,
                         dim=c(length(all_years),length(all_weeks)))
# create new field in lep for week of the year diagnosed
lep$week_diag <- as.numeric(format(x = as.Date(lep$date_diagn,
                                               format = '%m/%d/%Y'),
                                   format="%U"))
for(iy in 1:length(all_years)){
  for(iw in 1:length(all_weeks)){
    counts_yearWeek[iy,iw] <- sum(lep$year_diag==all_years[iy] &
                                    lep$week_diag==all_weeks[iw])
  }
}

# plot entire year
layout(matrix(c(1,2,3),nrow=1), widths=c(3,3,1))
plot(NA,xlim=range(all_weeks),ylim=range(counts_yearWeek),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per month',main='weekly diagnosis')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek[iy,],col=pal[iy])
}
abline(v=c(28,45),col='grey',lty=2)
# color.bar(pal,
#           min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

# zoom at high incidence period
plot(NA,xlim=c(28,45),ylim=range(counts_yearWeek),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per week',main='weekly diagnosis (zoom)')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek[iy,],col=pal[iy],lwd=2)
}
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),
          title = 'years')


#'
#' ## check multi-year dynamics in region 1 ##
#' 
#' 
#' This study follows the clustering analysis done using arcgis.

#'
#' ### plot points on top of map from openstreet map ###
#' 

# would be cool for later

#'
#' ### bound region ###
#' 

# define bounding region for region 1
layout(matrix(c(1,2),nrow=1), widths=c(3,1))
plot(sichuan,col='grey',axes=T,
     xlim=c(6e5,7e5),ylim=c(3.3e6,3.8e6))
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)

library(sp)
x = 6.4e5 # center x
y = 3.48e6 # center y
n = 100 # nr of pts
pts = seq(0, 2 * pi, length.out = n)

# small region
r = 4e4 # radius
xy = cbind(x + r * sin(pts), y + r * cos(pts))
ps = Polygons(list(Polygon(xy)),1) # create polygon object
region_1a = SpatialPolygons(list(ps)) # wrap into spatial polygon
plot(region_1a, add=T, border = 'black',lwd=2)
text(x=6.4e5,y=3.48e6+r,
     labels = '1a',col='black',cex=1.5)

# larger region
r = 1.5e5 # radius
xy = cbind(x + r * sin(pts), y + r * cos(pts))
ps = Polygons(list(Polygon(xy)),1) # create polygon object
region_1b = SpatialPolygons(list(ps))
plot(region_1b, add=T, border = 'black',lwd=2)
text(x=6.4e5,y=3.48e6+r,labels = '1b',col='black',cex=1.5)

color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),
          title = 'years')

#'
#' ### extract points within these regions ###
#' 

# 
library(SDMTools)
which_ina = which(pnt.in.poly(coordinates(lep),
                              region_1a@polygons[[1]]@Polygons[[1]]@coords)$pip == 1)
which_inb = which(pnt.in.poly(coordinates(lep),
                              region_1b@polygons[[1]]@Polygons[[1]]@coords)$pip == 1)

length(which_ina)
length(which_inb)


#' 
#' ### check number of cases for each year in these regions ###
#' 

#+ fig.width=8, fig.height=4
layout(matrix(c(1,2),nrow=1))

counts_years <- table(lep[which_ina,]$year_diag)
barplot(counts_years, main="", xlab="Years")
title('region 1a')

counts_years <- table(lep[which_inb,]$year_diag)
barplot(counts_years, main="", xlab="Years")
title('region 1b')

#' 
#' Similar trends, with many cases in years 2008-2010 in both cases
#' 

#' 
#' ### now per month ###
#' 

#+ fig.width=8, fig.height=4
layout(matrix(c(1,2),nrow=1))

counts_months <- table(lep[which_ina,]$month_diag)
barplot(counts_months, main="", xlab="Months")
title('region 1a')

counts_months <- table(lep[which_inb,]$month_diag)
barplot(counts_months, main="", xlab="Months")
title('region 1b')

#' 
#' ### plot yearly timeseries within these regions ###
#' 
#' 
#' first at a monthly resolution
#' 

# recount number of points within each (year,month) for subsets

all_years = unique(lep$year_diag)
all_months=1:12
counts_yearMonth_a <- array(NA,
                            dim=c(length(all_years),length(all_months)))
counts_yearMonth_b <- array(NA,
                            dim=c(length(all_years),length(all_months)))
for(iy in 1:length(all_years)){
  for(im in 1:length(all_months)){
    counts_yearMonth_a[iy,im] <- sum(lep[which_ina,]$year_diag==all_years[iy] &
                                       lep[which_ina,]$month_diag==all_months[im])
    counts_yearMonth_b[iy,im] <- sum(lep[which_inb,]$year_diag==all_years[iy] &
                                       lep[which_inb,]$month_diag==all_months[im])
  }
}

layout(matrix(c(1,2,3),nrow=1), widths=c(3,3,1))
plot(NA,xlim=range(all_months),ylim=range(counts_yearMonth_a),
     xlab='months',ylab='counts',main='time series in region 1a')
for(iy in 1:length(all_years)){
  lines(x=all_months,y=counts_yearMonth_a[iy,],col=pal[iy])
}
plot(NA,xlim=range(all_months),ylim=range(counts_yearMonth_b),
     xlab='months',ylab='counts',main='time series in region 1b')
for(iy in 1:length(all_years)){
  lines(x=all_months,y=counts_yearMonth_b[iy,],col=pal[iy])
}
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

#' 
#' now at a weekly resolution
#' 

counts_yearWeek_a <- array(NA,
                           dim=c(length(all_years),length(all_weeks)))
counts_yearWeek_b <- array(NA,
                           dim=c(length(all_years),length(all_weeks)))
# create new field in lep for week of the year diagnosed
lep$week_diag <- as.numeric(format(x = as.Date(lep$date_diagn,
                                               format = '%m/%d/%Y'),
                                   format="%U"))
for(iy in 1:length(all_years)){
  for(iw in 1:length(all_weeks)){
    counts_yearWeek_a[iy,iw] <- sum(lep[which_ina,]$year_diag==all_years[iy] &
                                      lep[which_ina,]$week_diag==all_weeks[iw])
    counts_yearWeek_b[iy,iw] <- sum(lep[which_inb,]$year_diag==all_years[iy] &
                                      lep[which_inb,]$week_diag==all_weeks[iw])
  }
}

#'
#' first for region 1a
#' 
#' 
# plot entire year
layout(matrix(c(1,2,3),nrow=1), widths=c(3,3,1))
plot(NA,xlim=range(all_weeks),ylim=range(counts_yearWeek_a),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per month',main='1a - weekly diagnosis')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek_a[iy,],col=pal[iy])
}
abline(v=c(28,45),col='grey',lty=2)
# color.bar(pal,
#           min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

# zoom at high incidence period
plot(NA,xlim=c(30,45),ylim=range(counts_yearWeek_a),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per week',main='1a - weekly diagnosis (zoom)')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek_a[iy,],col=pal[iy],lwd=2)
}
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),
          title = 'years')

#'
#' then for region 1b
#' 
#' 
# plot entire year 
layout(matrix(c(1,2,3),nrow=1), widths=c(3,3,1))
plot(NA,xlim=range(all_weeks),ylim=range(counts_yearWeek_b),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per month',main='1b - weekly diagnosis')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek_b[iy,],col=pal[iy])
}
abline(v=c(28,45),col='grey',lty=2)
# color.bar(pal,
#           min=min(lep$year_diag),max=max(lep$year_diag),title = 'years')

# zoom at high incidence period
plot(NA,xlim=c(30,45),ylim=range(counts_yearWeek_b),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab='week in year',ylab='counts per week',main='1b - weekly diagnosis (zoom)')
for(iy in 1:length(all_years)){
  lines(x=all_weeks,y=counts_yearWeek_b[iy,],col=pal[iy],lwd=2)
}
color.bar(pal,
          min=min(lep$year_diag),max=max(lep$year_diag),
          title = 'years')

#' 
#' # Analysis of number of reported cases per location #
#' 
#' 

# check individual locations of lepto incidence
unique_loc <- unique(coordinates(lep))

# this vector will contain times of incidence diagnosis at each location
ts_loc <- list(length=nrow(unique_loc))

for(i_loc in 1:nrow(unique_loc)){
  
  # find indices of rows corresponding to location unique_loc[i_loc,]
  idx_loc <- which(apply(coordinates(lep), 1, function(x) all(x == unique_loc[i_loc,])))
  
  # get corresponding diagnosis date
  ts_loc[[i_loc]] <- lep[['date_diagn']][idx_loc]
}

#' We can now check locations of higher diagnosis rates
#' 
#' The follwing plot shows how many incidence cases were reported per unique location. Most locations come with less than 5 incidence points, this gives confidence in the fact that we can model this data using a point process model (eg. POisson).

library(plyr)
lengths_perLoc <- unlist(lapply(ts_loc, length))
tab_lengths_perLoc <- table(lengths_perLoc)

barplot(tab_lengths_perLoc,
        xlab=expression('n'['i']*' - number of reported cases at one location'),
        ylab=expression('number of locations reporting n'['i']*' cases'))
title('number of reported cases by location')

#'
#' How many data points would there be if we discarded data with more than 5 occurences?
#' 

nbLoc_pernbCases <- cbind(1:length(tab_lengths_perLoc),
                          as.numeric(tab_lengths_perLoc))

cases_cumsum = rep(0,nrow(nbLoc_pernbCases))
accu = 0
for (i in 1:nrow(nbLoc_pernbCases)){
  accu <- accu + i*nbLoc_pernbCases[i,2]
  cases_cumsum[i] <- accu
}
plot(cases_cumsum,pch=19,ylim=c(0,3000),
     xlab=expression('n'['i']*'number of cases reported at one location'),
     ylab=expression('cumulative number of cases from regions with n'['i']*' cases'))
abline(v=pretty(1:nrow(nbLoc_pernbCases)),col='grey',lty=2)
abline(h=pretty(cases_cumsum),col='grey',lty=2)

#
#' # Check how many cases are reported at township centroids #
#' 
#' 

#' 
#' ## Import centroids from Sophie ##
#' 

library(foreign)
twn_centroids <- read.dbf('../sichuan_database/twnshps_centroids/sichuancentroids.dbf')
# transform to SpatialPointsDataFrame
coordinates(twn_centroids) <- c('Longitude','Latitude')

plot(sichuan,col='grey',axes=T)
plot(twn_centroids,add=T)

#' oops, they do not overlap... projection issue?


#' 
#' ## Import centroids from database ##
#' 

townships <- readShapePoly('../sichuan_database/from_gdb/Townships.shp',
                           proj4string = prj)
plot(townships,col='grey',axes=T)

plot(sichuan,col='grey',axes=T)
# get centroids of townships
twn_centroids <- coordinates(townships)
# tramsform numeric to dataframe
twn_centroids <- data.frame(twn_centroids)
names(twn_centroids) <- c('long','lat')
# transform to SpatialPointsDataFrame
coordinates(twn_centroids) <- c('long','lat')
plot(twn_centroids,pch=20,add=T)

#' 
#' ## Overlaps - visual checks ##
#' 

#' Check for eg region 1 if it looks like they overlap the data

#' Color points by number of values at each location
#' 
#' 

# unique_loc contains the list of unique locations
# lengths_perLoc contains number of points at each location

# transform unique_loc to spatial points dataframe (easier for plotting)
unique_loc <- as.data.frame(unique_loc)
coordinates(unique_loc) <- c('POINT_X','POINT_Y')
unique_loc$lengthsPerLoc <- lengths_perLoc

# define colors corresponding to number of points per location
# change the colorbar not to confuse with years of incidence
pal <- rev(heat.colors(max(unique_loc$lengthsPerLoc)))
# plot(1:length(pal),pch=19,col=pal)
intcols_lengthPerLoc <- pal[unique_loc$lengthsPerLoc]

layout(matrix(c(1,2),nrow=1), widths=c(4,1))
plot(sichuan,col='grey',axes=T,
     xlim=c(6.2e5,6.8e5),ylim=c(3.45e6,3.5e6))
plot(townships,add=T)
plot(unique_loc, col = intcols_lengthPerLoc, pch = 19,add=T,cex=0.5)
plot(twn_centroids,pch=3,add=T)
color.bar(pal,
          min=min(unique_loc$lengthsPerLoc),
          max=max(unique_loc$lengthsPerLoc),
          title = '# incidence')

#' It looks like locations where incidence is reported usually are unique within a township, and that does not depend on the number of incidence points found at that location.

#' 
#' ## Check wrt to locations with high number of incidence reported ##
#' 


#' Check for region where largest number of reported cases 
#' 
#' Red crosses indicates that these locations are amongst the 10 locations reporting the most cases (from 26 to 39 cases reported at one location).

# vector if indices corresponding to 10 largest numbers of reported cases
idx_lengthest = match(sort(lengths_perLoc,decreasing = T)[1:10],
                      lengths_perLoc)

# now get corresponding location
loc_lengthest <- unique_loc[idx_lengthest,]

plot(sichuan)
points(loc_lengthest,pch=19,
       col=colorRampPalette(c("red", "white"))( 10 ),cex=0.5)

plot(sichuan,col='grey',axes=T,
     xlim=c(6.2e5,6.8e5),ylim=c(3.45e6,3.5e6))
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
plot(twn_centroids,pch=3,add=3)
points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)

#' Red crosses indicates that these locations are amongst the 10 locations reporting the most cases (from 26 to 39 cases reported at one location).
#' 
#' Hard to discard whether they are actually located at township centroids...

#' 
#' ## Overlaps - compute number of overlapping data with buffers ##
#' 

#' First create buffer around township centroids

# 
library(rgeos)
townships_buffer <- gBuffer(twn_centroids, width=500, byid=TRUE )

# # plot buffers on top of map
# plot(sichuan,col='grey',axes=T,
#      xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
# plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
# plot(townships,add=T)
# plot(twn_centroids,pch=3,add=3)
# points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)
# plot(townships_buffer,add=T)

#' Now check intersection between lep data and buffers around township centers 

# this takes the intersection
lep_inBuffers <- lep[townships_buffer,]

paste(nrow(lep_inBuffers),'/',nrow(lep))

#' Seems that not many points are located at township centroids... Check where they are

library(rgeos)
townships_buffer <- gBuffer( twn_centroids, width=500, byid=TRUE )
plot(sichuan,col='grey',axes=T,
     xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
plot(townships,add=T)
plot(twn_centroids,pch=3,add=3)
points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)
plot(townships_buffer,add=T)
plot(lep_inBuffers,add=T,pch=19)

#' And they also come with locations with high point density, meaning that there is a higher chance that some incidence points are located close to township centroids...

#' Or maybe township centroids computed here from township shapefile
#' are not the ones used when cases are reported at centroid locations ? 
#' 
#' Conclusion on township vs point-level modeling : the conclusions shouldn't be very different, as most of the time there is one location per township. For now, township level analysis is enough, with covariates extracted at township level.
#' 
#' At the scale of graph shown previously, it is possible that points show patterns of regularity. I could test for that using the F function, not sure if that's useful here so I'll stop here.
#' 
#' 
#' 
#' #
#' # Number of cases per township #
#' 
#' Howard said that our data is very sparse, with many township coming with no data points. Here I investigate the number of cases found in each township.
#' 

# Histogram of number of cases found per township

# define projection for lepto data
proj4string(lep) <- proj4string(townships)

library(GISTools)
# this vector contains the number of points in each township
ptsPerTwnshp <- poly.counts(lep,townships)

# count how many township contain n incidence points
tab_ptsPerTwnshp <- table(ptsPerTwnshp)

# this is the percentage of township with no data at all.
tab_ptsPerTwnshp[1]/nrow(townships) * 100

# plot with cut y-axis
# install.packages('plotrix', dependencies = TRUE)
library(plotrix)
ylocs = c(seq(from=0,to=400,by=100),3850,3950)
gap.barplot(y=tab_ptsPerTwnshp,
            ytics = ylocs,yaxlab = ylocs,
            xaxlab = names(tab_ptsPerTwnshp),
            gap=c(401,3800),
            xlab = 'number of incidence points in township',
            ylab = 'number of townships')

#' Most townships come with no incidence point...
#' Confirmation of very sparse dataset.