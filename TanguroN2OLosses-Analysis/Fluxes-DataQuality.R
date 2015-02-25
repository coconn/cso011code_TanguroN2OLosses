# Fluxes-DataQuality.R
# how good are the fits for the fluxes?
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN DATA, MAKE DATAFRAME

fluxesfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxesfull.csv", stringsAsFactors=FALSE)
samplenum <- dim(fluxesfull)[1]/3 # sample size of chambers (div by 3 because each chamber has a CO2, CH4 and N2O row)



########################################################################
# DATA QUALITY: R^2 INFO

LinearR2_N2O <- fluxesfull$LinearR2[(fluxesfull$GasType)=="N2O"]
QuadR2_N2O <- fluxesfull$QuadR2[(fluxesfull$GasType)=="N2O"]

LinearR2_CO2 <- fluxesfull$LinearR2[(fluxesfull$GasType)=="CO2"]
QuadR2_CO2 <- fluxesfull$QuadR2[(fluxesfull$GasType)=="CO2"]

LinearR2_CH4 <- fluxesfull$LinearR2[(fluxesfull$GasType)=="CH4"]
QuadR2_CH4 <- fluxesfull$QuadR2[(fluxesfull$GasType)=="CH4"]

## histograms of CO2, CH4 and N2O R^2 values

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Fluxes-DataQuality/"

# make plot and save
png(file = paste(pathsavefigures, "R2-fluxes-histograms.png", sep=""),width=6,height=6,units="in",res=400)

# make plot
par(mfrow=c(3,2),oma=c(0,0,2,0),mar = c(5.1, 4.1, 2.1, 2.1))
hist(LinearR2_N2O, main="")
hist(QuadR2_N2O, main="")
hist(LinearR2_CO2, main="")
hist(QuadR2_CO2, main="")
hist(LinearR2_CH4, main="")
hist(QuadR2_CH4, main="")
title(substitute(paste(R^2, " Histograms, Sample Size = ", samplenum, " chambers"), list(samplenum = samplenum)), outer=TRUE)

dev.off()



########################################################################
# DATA QUALITY: CO2, N2O AND CH4 CORRELATION

## correlation between the CO2 and N2O R^2 values
# correlation info
cor1t <- cor.test(LinearR2_CO2,LinearR2_N2O)
cor2t <- cor.test(QuadR2_CO2,QuadR2_N2O)
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2 ) {
  
  corpval <- paste("cor",i,"t$p.value",sep="")
  
  if(eval(parse(text=corpval)) < 0.001){
    cor_stars[i] <- "***"
  } else if(eval(parse(text=corpval)) < 0.01){
    cor_stars[i] <- "**"
  } else if(eval(parse(text=corpval)) < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- " "
  }
  
}
# to make abline
r2linearlm <- lm(LinearR2_N2O ~ LinearR2_CO2)
r2quadlm <- lm(QuadR2_N2O ~ QuadR2_CO2)

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Fluxes-DataQuality/"

# make plot and save
png(file = paste(pathsavefigures, "n2o-co2-correlation.png", sep=""),width=6,height=4,units="in",res=400)

# make plot
par(mfrow=c(1,2),oma=c(0,0,2,0),mar = c(5.1, 4.1, 2.1, 2.1))
#plot1
plot(LinearR2_CO2,LinearR2_N2O)
abline(r2linearlm)
legend('topleft', legend = paste("ρ =", round(cor1t$estimate,4), cor_stars[1]), bty = 'n')
#plot2
plot(QuadR2_CO2,QuadR2_N2O)
abline(r2quadlm)
legend('topleft', legend = paste("ρ =", round(cor2t$estimate,4), cor_stars[2]), bty = 'n')

# # plot 3-4 is CO2 vs CH4
# #plot3
# plot(LinearR2_CO2,LinearR2_CH4)
# abline(r2linearlm)
# legend('topleft', legend = paste("ρ =", round(cor1t$estimate,4), cor_stars[1]), bty = 'n')
# #plot4
# plot(QuadR2_CO2,QuadR2_N2O)
# abline(r2quadlm)
# legend('topleft', legend = paste("ρ =", round(cor2t$estimate,4), cor_stars[2]), bty = 'n')
# 
# # plot 5-6 is N2O vs CH4
# #plot5
# plot(LinearR2_CO2,LinearR2_N2O)
# abline(r2linearlm)
# legend('topleft', legend = paste("ρ =", round(cor1t$estimate,4), cor_stars[1]), bty = 'n')
# #plot6
# plot(QuadR2_CO2,QuadR2_N2O)
# abline(r2quadlm)
# legend('topleft', legend = paste("ρ =", round(cor2t$estimate,4), cor_stars[2]), bty = 'n')


dev.off()






########################################################################
# NOTES AND TESTING

# see notes in evernote from mtg with Eric Davidson
# come up with way to rigorously deal with linear vs. quad, etc.



########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




