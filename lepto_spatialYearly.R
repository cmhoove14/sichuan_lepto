#' ---
#' title: "Bayesian analysis of yearly lepto incidence data"
#' author: "Karina Cucchi, Christopher Hoover"
#' date: "November 16th, 2016"
#' ---
#' 

library(fields) # used here for color scheme
library(sp) # convenient format for spatial dataset
library(maptools) # for importing shapefiles
library(rgdal) # for coordinates things
library(classInt) # for classIntervals ploteqc

source('utils_colorbar.R')

#'
#' # Load data #
#'

#' ## Lepto dataset ##

# reads lepto from shapefile and associated projection data
lep <- readOGR('../sichuan_database/from_gdb','all_cases',verbose = T)
proj4string(lep)

# some points have negative coordinates, get rid of them
wrong_rows <- which(coordinates(lep) < 0,arr.ind = T)[,1]
lep <- lep[-wrong_rows,] ; rm(wrong_rows)
plot(lep)

#' ## Townships polygons with population ## 

# get population data from Chris' incidence shapefiles

#'
#' ## Population data ## 
#' 

# get population data from Chris' incidence shapefiles

# townships projections are WGS_1984_UTM_Zone_48N
# incidence projections are WGS_1984_UTM_Zone_47N
# change everything to 48 because this is the zone covering the western part of Sichuan where incidence cases happen

townships.47 <- readOGR(
  '../sichuan_database/twnshps_incidence','2004towninc')
proj4string(townships.47)

# transform to zone 48
library(rgdal)
townships <- spTransform(townships.47, proj4string(lep))
rm(townships.47)

par(oma=c( 0,0,0,3)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$total,
        breaks = pretty(townships$total,30))
title('population per township')

# check overlay with lepto data
par(oma=c( 0,0,0,3)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$total,
        breaks = pretty(townships$total,30))
plot(lep,add=T,col='burlywood')

sum(townships$total) / 10^6

#' We are missing about 10 million people when compared to the national Bureau of Statistics of China
#' \url{http://www.stats.gov.cn/english/statisticaldata/yearlydata/YB2000e/D03E.htm}

#'
#' # Univariate analysis of lepto data #
#'
#' Focus at one year only, year 2005.
#' 

lep_dates <- as.Date(as.character(lep$date_diagn),format = '%Y/%m/%d')
lep$year_diag <- as.numeric(format(lep_dates,'%Y'))

#'
#' ## Calculate expected and observed rates for entire dataset ##
#' 

# total incidence rate
e_total = nrow(lep)/sum(townships$total)
print(e_total)
#' The total expected rate for the entire dataset is 40 out of one million indoviduals per 11 years.

# expected incidence rate per township
townships$e_i = townships$total * e_total
# number of incidence points per township
library(GISTools)
townships$y_i = poly.counts(lep,townships)
sum(townships$y_i)
nrow(lep)
#' Some datapoints are not overlayed by a polygon... a few townships are missing

# corresponding relative risk
townships$theta_i = townships$y_i / townships$e_i

# plot map
par(oma=c( 0,0,0,3)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$theta_i,
        breaks = pretty(townships$theta_i,30),
        xlim=c(2e5,8e5),ylim=c(3e6,3.6e6),
        axes=T)
title(expression('map of relative risks '*theta['i']))

# take log-transform
townships$eta_i <- log(townships$theta_i)
townships$eta_i[is.infinite(townships$eta_i)] <- NA

hist(townships$eta_i,probability = T,main='',
     xlab=expression(eta['i']),ylab=expression('p('*eta['i']*')'))

# plot map
par(oma=c(0,0,0,3)) # margin of 4 spaces width at right hand side
plot(townships,border='grey',
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
ploteqc(spobj = townships,border=NA,
        z = townships$eta_i,
        breaks = pretty(townships$eta_i,30),
        add=T)
title(expression('map of log-transformed relative risks '*eta['i']))

#'
#' ## Now focus at one year only, year 2005. ##
#' 

lep_dates <- as.Date(as.character(lep$date_diagn),format = '%Y/%m/%d')
lep$year_diag <- as.numeric(format(lep_dates,'%Y')); rm(lep_dates)
lep.2005 <- lep[which(lep$year_diag==2005),]

#' Reproduce the analysis above

# total incidence rate
e_total.2005 = nrow(lep.2005)/sum(townships$total)
print(e_total.2005)
#' The total expected rate for the entire dataset is 11 out of one million indoviduals per 11 years.

# expected incidence rate per township
townships$e_i.2005 = townships$total * e_total.2005
# number of incidence points per township
library(GISTools)
townships$y_i.2005 = poly.counts(lep.2005,townships)

# corresponding relative risk
townships$theta_i.2005 = townships$y_i.2005 / townships$e_i.2005

hist(townships$theta_i.2005,probability = T,main='year 2005',
     xlab=expression(theta['i']),ylab=expression('p('*theta['i']*')'))

# plot map
par(oma=c( 0,0,0,3)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$theta_i.2005,
        breaks = pretty(townships$theta_i.2005,30),
        xlim=c(2e5,8e5),ylim=c(3e6,3.6e6),
        axes=T)
title(expression('map of relative risks '*theta['i']*', 2005'))

# take log-transform
townships$eta_i.2005 <- log(townships$theta_i.2005)
townships$eta_i.2005[is.infinite(townships$eta_i.2005)] <- NA

hist(townships$eta_i.2005,probability = T,main='year 2005',
     xlab=expression(eta['i']),ylab=expression('p('*eta['i']*')'))

# plot map
par(oma=c(0,0,0,3)) # margin of 4 spaces width at right hand side
plot(townships,border='grey',
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
ploteqc(spobj = townships,border=NA,
        z = townships$eta_i.2005,
        breaks = pretty(townships$eta_i.2005,30),
        add=T)
title(expression('log-transformed relative risks '*eta['i']*', 2005'))

#'
#' ## Calculate neighboring relationships between townships ##
#' 
#' The neighboring relationship is based on townships sharing one or more boundary points.

library(spdep) #spatial dependance
# ploy2nb calculate a neighbor list object
nb.bound <- poly2nb(townships)

layout(matrix(c(1,2),nrow=1))
plot(townships, border = "gray")
coords=coordinates(townships) # centroids of township polygons
plot(nb.bound, coords, pch = 19, cex = 0.4, add = TRUE)

plot(townships, border = "gray",
     xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
plot(nb.bound, coords, pch = 19, cex = 0.4, add = TRUE)


#'
#' ## Run CARBayes ##
#'

#' Code is borrowed from chap.7 - Bayesian Modeling and Inference
#' in Handbook of Spatial Epidemiology

library(CARBayes)

# transforms the neighbor list object to a matrix
weight <- listw2mat(nb2listw(nb.bound, style = "B"))
Wmat<- as(weight, "CsparseMatrix")

# define variables for formula
y = townships$y_i.2005
pop = townships$total

#'
#' ### First run without autocorrelation ###
#'

formulaR = y ~ 1+offset(log(townships$total)) #intercept only
# build model
modelR=S.CARbym(formula = formulaR,
                family="poisson",
                W=as.matrix(Wmat),
                burnin=30000,
                n.sample=40000,
                thin=1,
                verbose=TRUE)

REsum=modelR$samples$re

REsummean=colMeans(REsum)


#'
#' ### Now run with autocorrelation ###
#'

bymCAR.re