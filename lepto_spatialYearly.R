#' ---
#' title: "Bayesian analysis of yearly lepto incidence data"
#' author: "Karina Cucchi, Christopher Hoover"
#' date: "November 23rd, 2016"
#' ---
#' 

library(fields) # used here for color scheme
library(sp) # convenient format for spatial dataset
library(maptools) # for importing shapefiles
library(rgdal) # for coordinates things
library(classInt) # for classIntervals ploteqc

source('utils.R')

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

#'
#' ## Population data ## 
#' 

#' get population data from Chris' incidence shapefiles

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
library(GISTools) # for poly.counts function
townships$y_i = poly.counts(lep,townships)
sum(townships$y_i)
nrow(lep)
#' Some datapoints are not overlayed by a polygon... a few townships are missing

# corresponding SIR (Standard Morbidity Ratio)
# ratio of observed to expected
townships$sir_i = townships$y_i / townships$e_i

# plot map
par(oma=c( 0,0,0,4)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$sir_i,
        breaks = pretty(townships$sir_i,30),
        xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
title(expression('map of SIR'))
# Add scalebar
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

# take log-transform
townships$log_sir_i <- log(townships$sir_i)
townships$log_sir_i[is.infinite(townships$log_sir_i)] <- NA

hist(townships$log_sir_i,probability = T,main='',
     xlab='log(SIR)',ylab=expression('p(log(SIR))'))

# plot map
par(oma=c(0,0,0,3)) # margin of 4 spaces width at right hand side
plot(townships,border='grey',
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
ploteqc(spobj = townships,border=NA,
        z = townships$log_sir_i,
        breaks = pretty(townships$log_sir_i,30),
        add=T)
title(expression('map of log-transformed SIR'))
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

#'
#' ## Now focus at one year only, year 2005. ##
#' 

#' First calculate yearly SIR year by year

lep_dates <- as.Date(as.character(lep$date_diagn),format = '%Y/%m/%d')
lep$year_diag <- as.numeric(format(lep_dates,'%Y')); rm(lep_dates)
allYears <- unique(unique(lep$year_diag))

sir_byyear <- matrix(NA,nrow = nrow(townships),ncol=length(allYears))
colnames(sir_byyear) <- allYears

for(iy in 1:length(allYears)){
  
  # this is the subset of lep for year allYears[iy]
  lep.iy <- lep[which(lep$year_diag==allYears[iy]),]
  
  # expected incidence rate per township
  # take expected in 11 years and
  # divide by 11 (=length(allYears)) to get per year 
  e_i_in1year <- townships$e_i / 11
  # number of incidence points per township
  y_i_inyeariy = poly.counts(lep.iy,townships)
  
  sir_byyear[,iy] <- y_i_inyeariy / e_i_in1year
  
}
rm(e_i_in1year,y_i_inyeariy,iy)
townships$yearly_sir_i <- sir_byyear; rm(sir_byyear)

#' old way for year 2005

lep.2005 <- lep[which(lep$year_diag==2005),]

# total incidence rate
e_total.2005 = nrow(lep.2005)/sum(townships$total)
print(e_total.2005)

# expected incidence rate per township
townships$e_i.2005 = townships$total * e_total.2005
# number of incidence points per township
townships$y_i.2005 = poly.counts(lep.2005,townships)

# corresponding standardized incidence ratio
# SIR in unit si is count yi divided by expected
townships$sir_i.2005 = townships$y_i.2005 / townships$e_i.2005

hist(townships$sir_i.2005,probability = T,main='year 2005',
     xlab='SIR',ylab=expression('p(SIR)'))

# plot map
par(oma=c( 0,0,0,3)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$sir_i.2005,
        breaks = pretty(townships$sir_i.2005,30),
        xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
title(expression('map of SIR, 2005'))
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

# take log-transform
townships$log_sir_i.2005 <- log(townships$sir_i.2005)
townships$log_sir_i.2005[is.infinite(townships$log_sir_i.2005)] <- NA

hist(townships$log_sir_i.2005,probability = T,main='year 2005',
     xlab='log(SIR)',ylab=expression('p(log(SIR))'))

# plot map
par(oma=c(0,0,0,3)) # margin of 4 spaces width at right hand side
plot(townships,border='grey',
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
ploteqc(spobj = townships,border=NA,
        z = townships$log_sir_i.2005,
        breaks = pretty(townships$log_sir_i.2005,30),
        add=T)
title(expression('log-transformed SIR, 2005'))
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

#'
#' Now save townships variable for other scripts
#'

save('townships',file = "RData/townships_sir.RData")

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
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

plot(townships, border = "gray",
     xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
plot(nb.bound, coords, pch = 19, cex = 0.4, add = TRUE)
myScalebar(units_label = 10000,text_label = "10 km",xleft=6.7e5)

#'
#' ## Run CARBayes ##
#'

#' Code is borrowed from chap.7 - Bayesian Modeling and Inference
#' in Handbook of Spatial Epidemiology

library(CARBayes)

# transforms the neighbor list object to a matrix
weight <- listw2mat(nb2listw(nb.bound, style = "B"))
Wmat<- as(weight, "CsparseMatrix")

# build model

file_save = "RData/lepto_spatialYearly.RData"
if(file.exists(file_save)) load(file_save)
if(!exists("modelR")){
  modelR=S.CARbym(formula = y_i.2005 ~ 1+offset(log(total)),
                  data = townships,
                  family = "poisson",
                  W = as.matrix(Wmat),
                  burnin = 30000,
                  n.sample = 40000,
                  thin = 1,
                  verbose = TRUE)
  save(list = "modelR",file = file_save)
}

summary(modelR)
print(modelR)


#'
#' Investigate intensities of spatial autocorrelation and of random effects
#'
#'spatial autocorrelation :
# tau2 here corresponds to r in Onicescu & Lawson 2016
# sigma2 is random effects in both cases

layout(matrix(c(1,2),nrow=1))
plot(density(sqrt(modelR$samples$tau2)),
     xlab='r',
     ylab='p(r)',
     main='posterior for r')
plot(density(sqrt(modelR$samples$sigma2)),
     xlab=expression(sigma),
     ylab=expression('p('*sigma*')'),
     main=expression('posterior for '*sigma))

# plot resulting fitted values for mean risk (?)

layout(1)
par(oma=c(0,0,0,3)) # margin of 4 spaces width at right hand side
plot(townships,border='grey',
     xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
ploteqc(spobj = townships,border=NA,
        z = fitted.values(modelR),
        breaks = pretty(fitted.values(modelR),30),
        add=T)
title(expression('fitted risk values'))
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)

#' I don't think we can say anything on the basis of that map.
#' Try focus on a subregion in that map.
#' For example region 1.

