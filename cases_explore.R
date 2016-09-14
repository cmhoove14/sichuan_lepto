#Lepto case data with land cover, population, and SAGA TWI extracted to case locations
lep<-read.csv('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/cases_twi_pop_globcov.csv')

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
    
#Get summaries of data ##########
  cases_crop = length(lep$OBJECTID[lep$landcv == "cropland"])
  cases_for = length(lep$OBJECTID[lep$landcv == "forest"])
  cases_shrub = length(lep$OBJECTID[lep$landcv == "shrub_grass"])
  cases_urb = length(lep$OBJECTID[lep$landcv == "urban"])
  cases_wat = length(lep$OBJECTID[lep$landcv == "water"])
  
  cases_tot = sum(cases_crop,cases_for,cases_urb,cases_wat,cases_shrub)
  
  boxplot(lep$SAGA_TWI)
    mean(lep$SAGA_TWI)
    sd(lep$SAGA_TWI)
    