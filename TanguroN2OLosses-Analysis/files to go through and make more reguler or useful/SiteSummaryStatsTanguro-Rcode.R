# SiteSummaryStatsTanguro-Rcode.R
# taking flux data and doing something with it
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN DATA, MAKE DATAFRAME

fluxesfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxesfull.csv", stringsAsFactors=FALSE)
dim(fluxesfull)[1]/2 # sample size of chambers (div by two because each chamber has a CO2 and N2O row)




#### dealt with in new file - data quality figures made as well
# ########################################################################
# # DATA QUALITY: R^2 INFO
# 
# LinearR2_N2O <- fluxesfull$LinearR2[(fluxesfull$GasType)=="N2O"]
# QuadR2_N2O <- fluxesfull$QuadR2[(fluxesfull$GasType)=="N2O"]
# 
# LinearR2_CO2 <- fluxesfull$LinearR2[(fluxesfull$GasType)=="CO2"]
# QuadR2_CO2 <- fluxesfull$QuadR2[(fluxesfull$GasType)=="CO2"]
# 
# ## histograms of CO2 and N2O R^2 values
# par(mfrow=c(2,2),oma=c(0,0,2,0),mar = c(5.1, 4.1, 2.1, 2.1))
# hist(LinearR2_N2O, main="")
# hist(QuadR2_N2O, main="")
# hist(LinearR2_CO2, main="")
# hist(QuadR2_CO2, main="")
# title(expression("R"^2*" Histograms"), outer=TRUE)
# 
# ## correlation between the CO2 and N2O R^2 values
# # correlation info
# cor1t <- cor.test(LinearR2_CO2,LinearR2_N2O)
# cor2t <- cor.test(QuadR2_CO2,QuadR2_N2O)
# cor_stars <- numeric(length=2)
# # cycle through to set number of stars
# for (i in 1:2 ) {
#   
#   corpval <- paste("cor",i,"t$p.value",sep="")
#   
#   if(eval(parse(text=corpval)) < 0.001){
#     cor_stars[i] <- "***"
#   } else if(eval(parse(text=corpval)) < 0.01){
#     cor_stars[i] <- "**"
#   } else if(eval(parse(text=corpval)) < 0.05){
#     cor_stars[i] <- "*"
#   } else {
#     cor_stars[i] <- " "
#   }
#   
# }
# # to make abline
# r2linearlm <- lm(LinearR2_N2O ~ LinearR2_CO2)
# r2quadlm <- lm(QuadR2_N2O ~ QuadR2_CO2)
# 
# # make plot
# par(mfrow=c(1,2),oma=c(0,0,2,0),mar = c(5.1, 4.1, 2.1, 2.1))
# #plot1
# plot(LinearR2_CO2,LinearR2_N2O)
# abline(r2linearlm)
# legend('topleft', legend = paste("ρ =", round(cor1t$estimate,4), cor_stars[1]), bty = 'n')
# #plot2
# plot(QuadR2_CO2,QuadR2_N2O)
# abline(r2quadlm)
# legend('topleft', legend = paste("ρ =", round(cor2t$estimate,4), cor_stars[2]), bty = 'n')
# 
# #### save these


########################################################################
# SITE SUMMARY

# include cut off in R2 values when making a summary

# Which flux to use?
# Generally Rod thinks the quadratic fit makes more sense (based on theory), but if the second derivative is positive (meaning the fit is accelerating up), then use the linear fit
# This gets "linked" into a summary excel doc that brings in all of the different summary sheets from all the excel files for each date of sampling

# for now, let's just use the quadratic fluxes


## list of every site
fluxesfull$easysitename <- do.call(paste, c(fluxesfull[c("Site", "SampleDate")], sep = "_")) 
sitesummarylist <- unique(fluxesfull$easysitename)
length(sitesummarylist)

## pre-start output df
outputdffull <- data.frame()


for (i in 1:length(sitesummarylist)) {
  
  ## get each site id
  
  sitehere <- sitesummarylist[i]
  
  ## get linear flux info
  
  # eventually make this an ifelse system where if the second derivative is positive (meaning the fit is accelerating up), then use the linear fit
  # perhaps also use ifelse to make sure that the R^2 for whatever fit you use is acceptable
  meanfluxN2Ol <- mean(fluxesfull$LinearFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  meanfluxCO2l <- mean(fluxesfull$LinearFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  sdfluxN2Ol <- sd(fluxesfull$LinearFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  sdfluxCO2l <- sd(fluxesfull$LinearFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  meanR2N2Ol <- mean(fluxesfull$LinearR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  meanR2CO2l <- mean(fluxesfull$LinearR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  sdR2N2Ol <- sd(fluxesfull$LinearR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  sdR2CO2l <- sd(fluxesfull$LinearR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  
  SampleDate <- unique(fluxesfull$SampleDate[(fluxesfull$easysitename)==sitehere])
  LUtype <- unique(fluxesfull$LUtype[(fluxesfull$easysitename)==sitehere])
  Site <- unique(fluxesfull$Site[(fluxesfull$easysitename)==sitehere])
  
  outputdf <- data.frame(sitehere,SampleDate,Site,LUtype,meanfluxN2Ol,sdfluxN2Ol,meanR2N2Ol,sdR2N2Ol,meanfluxCO2l,sdfluxCO2l,meanR2CO2l,sdR2CO2l)
  
  ## get quad flux info
  
  # eventually make this an ifelse system where if the second derivative is positive (meaning the fit is accelerating up), then use the linear fit
  # perhaps also use ifelse to make sure that the R^2 for whatever fit you use is acceptable
  meanfluxN2Oq <- mean(fluxesfull$QuadFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  meanfluxCO2q <- mean(fluxesfull$QuadFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  sdfluxN2Oq <- sd(fluxesfull$QuadFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  sdfluxCO2q <- sd(fluxesfull$QuadFlux[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  meanR2N2Oq <- mean(fluxesfull$QuadR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  meanR2CO2q <- mean(fluxesfull$QuadR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  sdR2N2Oq <- sd(fluxesfull$QuadR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="N2O"])
  sdR2CO2q <- sd(fluxesfull$QuadR2[(fluxesfull$easysitename)==sitehere & (fluxesfull$GasType)=="CO2"])
  
  SampleDate <- unique(fluxesfull$SampleDate[(fluxesfull$easysitename)==sitehere])
  LUtype <- unique(fluxesfull$LUtype[(fluxesfull$easysitename)==sitehere])
  Site <- unique(fluxesfull$Site[(fluxesfull$easysitename)==sitehere])
  
  outputdfcbind <- data.frame(meanfluxN2Oq,sdfluxN2Oq,meanR2N2Oq,sdR2N2Oq,meanfluxCO2q,sdfluxCO2q,meanR2CO2q,sdR2CO2q)
  
  ## combine data
  
  # bind linear and quad flux info into one row
  outputdf <- cbind(outputdf, outputdfcbind)
  # bind onto output table
  outputdffull <- rbind(outputdffull, outputdf)
  
}

## save outputdffull as csv file
# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/"
# this is currently straightforward linear and Venterea quad flux only (no instantaneous flux, removing 4th vial, etc.)
write.csv(outputdffull, file=paste(pathsavefiles, "fluxessitesummary.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING

# see notes in evernote from mtg with Eric Davidson
# come up with way to rigorously deal with linear vs. quad, etc.



########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




