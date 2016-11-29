#' ---
#' title: "Analysis of correlations between SMR and covariates"
#' author: "Karina Cucchi"
#' date: "November 28th, 2016"
#' ---

library(fields) # used here for color scheme
library(sp) # convenient format for spatial dataset
library(maptools) # for importing shapefiles
library(rgdal) # for coordinates things
library(classInt) # for classIntervals ploteqc
source('utils.R')

#'
#' # Load township incidence and rainfall data #
#'

load('RData/townships_cmorph.RData')

# plot SIR 

par(oma=c( 0,0,0,4)) # margin of 4 spaces width at right hand side
ploteqc(spobj = townships,border=NA,
        z = townships$sir_i,
        breaks = pretty(townships$sir_i,30),
        xlim=c(2e5,8e5),ylim=c(3e6,3.6e6))
title(expression('map of SIR'))
# Add scalebar
myScalebar(units_label = 100000,text_label = "100 km",xleft=6e5)


#' # Relationship between SIR and rainfall #

#' Here rainfall is extracted from CMORPH dataset.

# first for year 2005

plot(x=townships$cmorph_yearlymm[,"2005"],
     y=townships$sir_i.2005,
     pch=19,
     xlab="yearly rainfall in township [mm]",ylab="SIR",
     xlim=range(townships$cmorph_yearlymm[,"2005"]),
     main="2005")

idx_isnotna <- which(!is.na(townships$log_sir_i.2005))
plot(x=townships$cmorph_yearlymm[idx_isnotna,"2005"],
     y=townships$log_sir_i.2005[idx_isnotna],
     pch=19,
     xlab="yearly rainfall in township [mm]",ylab="log(SIR)",
     xlim=range(townships$cmorph_yearlymm[idx_isnotna,"2005"]),
     main="2005")

# now for all years
plot(x=rowSums(townships$cmorph_yearlymm),
     y=townships$sir_i,
     pch=19,
     xlab="total rainfall in township [mm]",ylab="SIR",
     xlim=range(rowSums(townships$cmorph_yearlymm)),
     main="all years")

idx_isnotna <- which(!is.na(townships$log_sir_i))
plot(x=rowSums(townships$cmorph_yearlymm)[idx_isnotna],
     y=townships$log_sir_i[idx_isnotna],
     pch=19,
     xlab="total rainfall in township [mm]",ylab="log(SIR)",
     xlim=range(rowSums(townships$cmorph_yearlymm)[idx_isnotna]),
     main="all years")

#' Now plot relationship year by year

str(townships$yearly_sir_i)
str(townships$cmorph_yearlymm)

# define color scheme

pal <- tim.colors(length(colnames(townships$yearly_sir_i)))
layout(matrix(c(1,2),nrow=1), widths=c(4,1))
plot(NA,
     xlim=range(townships$cmorph_yearlymm,na.rm = T),
     ylim=range(townships$yearly_sir_i,na.rm=T),
     xlab="yearly rainfall in township [mm]",
     ylab="SIR")
for(i in 1:ncol(townships$yearly_sir_i)){
  points(x=townships$cmorph_yearlymm[,i+1],
         y=townships$yearly_sir_i[,i],
         col=pal[i],
         pch=19)
  
}
title('SIR and yearly rainfall')
color.bar(pal,
          min=2004,max=2014,title = 'years')

#' Now plot log-transformed SIR

layout(matrix(c(1,2),nrow=1), widths=c(4,1))
townships$yearly_log_sir_i <- log(townships$yearly_sir_i)
townships$yearly_log_sir_i[is.infinite(townships$yearly_log_sir_i)] <- NA
plot(NA,
     xlim=c(400,1500),
     ylim=range(townships$yearly_log_sir_i,na.rm=T),
     xlab="yearly rainfall in township [mm]",
     ylab="log SIR")
for(i in 1:ncol(townships$yearly_sir_i)){
  points(x=townships$cmorph_yearlymm[,i+1],
         y=townships$yearly_log_sir_i[,i],
         col=pal[i],
         pch=19)
  
}
title('SIR and yearly rainfall')
color.bar(pal,
          min=2004,max=2014,title = 'years')

# plot on different plots
layout(matrix(1:12,nrow=3))
par(oma=c(0,0,0,0),mar=c(0,0,0,0))
for(i in 1:11){
  plot(NA,
       xlim=c(400,1500),
       ylim=range(townships$yearly_log_sir_i,na.rm=T),
       xaxt='n',yaxt='n')
  points(x=townships$cmorph_yearlymm[,i+1],
         y=townships$yearly_log_sir_i[,i],
         col=pal[i],
         pch=19)
}


#' # Relationship between SIR and TWI #





 

