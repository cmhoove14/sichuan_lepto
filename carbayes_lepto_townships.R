library(sp)
library(Matrix)
library(maptools)
library(CARBayes)
library(spdep)
library(CARBayesdata)
library(coda)

SW <- readShapeSpatial("C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/Scratch/Case_extracts/twnshp_incBAD")
SW_nb <- poly2nb(SW)
summary(SW_nb)
###SW_b is my spatial weights 
SW_b <- nb2listw(SW_nb, style="B", zero.policy = T)
SW_b$style
SW_binary <- as(SW_b, "symmetricMatrix")
SW_M <- as.matrix(SW_binary)
W <- SW_b

lep<-as.data.frame(SW)
  lep$MEAN = lep$MEAN * 100
  lep$MEAN_1 = lep$MEAN_1 * 100
  
colnames(lep)[c(61,66)] <- c('prop_ag', 'prop_twi_upper_quartile')
  
#carbayes formula;
sich_CB <- S.CARbym(formula=Count_1 ~ prop_ag + prop_twi_upper_quartile + offset(log(total)), family="poisson", data=lep,  
                    W=SW_M, burnin=5000, n.sample=100000, thin=50, prior.mean.beta = NULL, prior.var.beta = NULL, prior.tau2 = NULL, prior.sigma2 = NULL, 
                    verbose = T) 

plot(sich_CB$samples$beta) #Check for convergence

sich_CB$summary.results
exp(sich_CB$summary.results)
  