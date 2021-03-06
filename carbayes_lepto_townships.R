library(sp)
library(Matrix)
library(maptools)
library(CARBayes)
library(spdep)
library(CARBayesdata)
library(coda)

#Read in shapefile and develop neighborhood matrix #############
#Use WGS UTM zone 47N projection
prj<-CRS("+proj=utm +zone=47 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#Read in township level incidence shapefile here; this one contains all cases below contains code to break up into years
SW <- readShapePoly("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/Scratch/Case_extracts/twnshp_incBAD",
                       proj4string = prj)
SW_nb <- poly2nb(SW)
  summary(SW_nb)
  
SW_b <- nb2listw(SW_nb, style="B", zero.policy = T)
SW_b$style

#Convert to binary neighborhood matrix where neighbors indicated by 1
  SW_binary <- as(SW_b, "symmetricMatrix")
  SW_M <- as.matrix(SW_binary)

#For each year and for all years, find number of case points within each township ###############
pts04<-readShapePoints('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/SL_ST/SL_ST/years/2004casepoints.shp',
                        proj4string = prj)  

plot(SW)
  plot(pts04, add = TRUE, col = 'red')
  
#Something wrong with the projections here or something, but township shapefile and cases aren't overlapping
#For now, just going to do spatial processing in Arc and just use R for analysis  

#Convert proportions to percentages in each township ##############   
lep<-as.data.frame(SW)
  lep$MEAN = lep$MEAN * 100
  lep$MEAN_1 = lep$MEAN_1 * 100
  
colnames(lep)[c(61,66)] <- c('prop_ag', 'prop_twi_upper_quartile')
  
#carbayes formula; ###############
#left is outcome (twnshp incidence here) 
#right side of the formula indicates tested predictor variables; shuld always have poulation offset
sich_CB <- S.CARbym(formula=Count_1 ~ prop_ag + prop_twi_upper_quartile + offset(log(total)), family="poisson", data=lep,  
                    W=SW_M, burnin=5000, n.sample=100000, thin=100, 
                    prior.mean.beta = NULL, prior.var.beta = NULL, prior.tau2 = NULL, prior.sigma2 = NULL, 
                    verbose = T) 
#Check for model convergence; trace plots should look like ~fuzzy caterpillars indicating the entire sample space was explored and the model converged
plot(sich_CB$samples$beta) 
plot(sich_CB$samples$tau2) 
plot(sich_CB$samples$sigma2) 

#Check out model summary
sich_CB$summary.results

#Exponentiate coefficients to get IRRs associated with each predictor variable
exp(sich_CB$summary.results)
  