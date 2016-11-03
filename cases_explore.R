#Lepto case data with land cover, population, and SAGA TWI extracted to case locations
require(lubridate)
require(raster)
lep<-read.csv('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/cases_twi_pop_globcov.csv')
twi<-raster('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/SL_ST/SL_ST/tiffs/SAGA_TWI1.tif')

#Data cleaning ############
  #convert birthday and case date to dates
  lep$date_diagn<-as.Date(lep$date_diagn, format = '%m/%d/%Y')
  lep$birthday<-as.Date(lep$birthday, format = '%m/%d/%Y')
  
  #calculate age at time of diagnosis
  lep$age_diag<-round((lep$date_diagn - lep$birthday)/365, digits=2)
  
  #extract year and month of diagnosis
  lep$year_diag<-year(lep$date_diagn)
  lep$month_diag<-month(lep$date_diagn)
  
  #Convert globcover rastervalue to land use category
  lep$landcv<-NA
    codes<-sort(unique(lep$GlobCover_NoInt))
    covs<-c("NA", rep("cropland", 4), rep("forest",4), rep("shrub_grass", 2), "urban", "water")
    for(i in 1:nrow(lep)){
      code = lep[i, 12]
      lep[i,16] = covs[which(code==codes)]
    }
    
  
twidf<-as.data.frame(twi)      
#Get summaries of data ##########
  cases_crop = length(lep$OBJECTID[lep$landcv == "cropland"])
  cases_for = length(lep$OBJECTID[lep$landcv == "forest"])
  cases_shrub = length(lep$OBJECTID[lep$landcv == "shrub_grass"])
  cases_urb = length(lep$OBJECTID[lep$landcv == "urban"])
  cases_wat = length(lep$OBJECTID[lep$landcv == "water"])
  
  cases_tot = sum(cases_crop,cases_for,cases_urb,cases_wat,cases_shrub)
  
  
plot(density(twidf$SAGA_TWI1, na.rm = TRUE), xlab = 'TWI Value', main = '', lwd = 2)
  lines(density(lep$SAGA_TWI, na.rm = TRUE), col = 'blue', lwd = 2) 
  
boxplot(twidf$SAGA_TWI1, lep$SAGA_TWI, na.rm = TRUE, 
        ylab = 'TWI', boxwex = 0.25, names = c('All', 'Cases'))
    mean(lep$SAGA_TWI)
    sd(lep$SAGA_TWI)
    summary(twidf$SAGA_TWI1, na.rm=TRUE)
    median(twidf$SAGA_TWI1, na.rm=TRUE)
    quantile(twidf$SAGA_TWI1, na.rm=TRUE)
    