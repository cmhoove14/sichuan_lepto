require(CARBayes)

leps<-read.csv('C:/Users/chris_hoover/Documents/RemaisWork/SurfaceH2O/China/Data/Lepto/sichuan_lepto/township_allCases_aggregated_90mTWI3rdQuart_propAg_10_28_16.csv')

plot(lep$prop_ag*100, lep$prop_TWI3rd*100, xlim = c(0,100), ylim = c(0,100),
     xlab = '% Agricultural area', ylab = '%TWI >3rd quartile', cex = 0.75, 
     main = 'Township level topo/land cover')

plot(lep$prop_ag*100, lep$inc_rate*1000, xlim = c(0,100), ylim = c(0,6),
     xlab = '% Agricultural area', ylab = 'Incidence Rate (cases/1000 people)', cex = 0.75)

plot(lep$prop_TWI3rd*100, lep$inc_rate*1000, xlim = c(0,100), ylim = c(0,6),
     xlab = '%TWI >3rd quartile', ylab = 'Incidence Rate (cases/1000 people)', cex = 0.75)

det<-glm(cases_all ~ prop_ag + prop_TWI3rd + offset(log(total)), data=lep, family=poisson(link='log'))
