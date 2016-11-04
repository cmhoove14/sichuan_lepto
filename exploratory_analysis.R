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
counts_years <- table(lep$year_diag)
barplot(counts_years, main="", xlab="Years")

#'
#' ## plot frequency per month of diagnosis ##
#' 

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

#' # Get timeseries per township #

unique_loc <- unique(coordinates(lep))
ts_loc <- list(length=nrow(unique_loc))
for(i_loc in 1:nrow(unique_loc)){

  # find indices of rows corresponding to location unique_loc[i_loc,]
  idx_loc <- which(apply(coordinates(lep), 1, function(x) all(x == unique_loc[i_loc,])))

  # get corresponding diagnosis date
  ts_loc[[i_loc]] <- lep[['date_diagn']][idx_loc]
}

