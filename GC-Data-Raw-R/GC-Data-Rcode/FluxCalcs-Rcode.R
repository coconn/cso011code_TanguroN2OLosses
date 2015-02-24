# FluxCalcs-Rcode.R
# taking vial data and converting it into fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in GC-Rcode-fileloop.R

# output products:
# fluxesfull.csv : master csv of fluxes
# Append_FluxFiguresFull.pdf : shows each flux fit


########################################################################
# BRING IN DATA, MAKE DATAFRAME

vialDFfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/vialDFfull.csv", stringsAsFactors=FALSE)

# make new column to ensure no repeats (easycallname = unique per chamber)
vialDFfull$easycallname <- do.call(paste, c(vialDFfull[c("Site", "SampleDate", "Chamber")], sep = "_")) 

#in case I want to see all the names outside of R environment
#write.csv(vialDFfull, file=paste("vialDFfull_prac.csv", sep = ""), row.names=FALSE)  

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/FluxFigures/"


########################################################################
# FLUXES

# calc flux for each site, chamber, date (using easycallname = unique per chamber)
# only in places where timept isn't NA (those are pits or standards/amb)


## list of every site
fluxcalclist <- unique(vialDFfull$easycallname)
# get rid of NA ones
# this gets rid of pit samples and samples from the travel and leak tests
i1 <- grepl("NA", fluxcalclist) 
fluxcalclist <- fluxcalclist[!i1]
length(fluxcalclist)
# cool - I have 453 unique fluxes to calc (can that be right???  crazy!)


## pre-start output df
outputdffull <- data.frame()

## chamber volume and area
chambervol <- 18000
chamberarea <- 1425


for (i in 1:length(fluxcalclist)) {
  
  ## get each flux calc id and info
  
  fluxhere <- fluxcalclist[i]
  
  # are there four points here or only two? (some GC runs)
  checklength <- length(vialDFfull$ngN_cm3_N2O[(vialDFfull$easycallname)==fluxhere])
  if(checklength == 4) {
    
    # if there are four samples for that chamber available, do calcs
    
    # for that flux calc id, get ngN_cm3_N2O, ngC_cm3_CO2, ngC_cm3_CH4, TimePt, TimePtSq
    ngN_cm3_N2O <- vialDFfull$ngN_cm3_N2O[(vialDFfull$easycallname)==fluxhere]
    ngC_cm3_CO2 <- vialDFfull$ngC_cm3_CO2[(vialDFfull$easycallname)==fluxhere]
    ngC_cm3_CH4 <- vialDFfull$ngC_cm3_CH4[(vialDFfull$easycallname)==fluxhere]
    TimePt <- vialDFfull$TimePt[(vialDFfull$easycallname)==fluxhere]
    TimePtSq <- vialDFfull$TimePtSq[(vialDFfull$easycallname)==fluxhere]
    # informational stuff to log
    Site <- unique(vialDFfull$Site[(vialDFfull$easycallname)==fluxhere])
    LUtype <- unique(vialDFfull$LUtype[(vialDFfull$easycallname)==fluxhere])
    Chamber <- unique(vialDFfull$Chamber[(vialDFfull$easycallname)==fluxhere])
    SampleDate <- unique(vialDFfull$SampleDate[(vialDFfull$easycallname)==fluxhere])
    
    # before you make the models, set any no pressure vials to NA
    #i=3
    nopressure <- vialDFfull$Pressure[(vialDFfull$easycallname)==fluxhere]
    nopressureid <- which(nopressure=="N")
    ngN_cm3_N2O[nopressureid] <- NA
    ngC_cm3_CO2[nopressureid] <- NA
    ngC_cm3_CH4[nopressureid] <- NA
    
    # if there is more than 1 no pressure vial in this flux, skip all the rest
    checkpressurelength <- sum(!is.na(ngN_cm3_N2O))
    
    if(checkpressurelength == 4 || checkpressurelength == 3) 
    {
      
      ## get linear model and info
      
      # N
      lmfitN <- lm(ngN_cm3_N2O ~ TimePt)
      r2_N <- summary(lmfitN)$r.squared
      tmpslope_N <- summary(lmfitN)$coefficients[2,1]
      tmpintc_N <- summary(lmfitN)$coefficients[1,1]
      # linear flux
      # mass per area^2 per time
      lmfluxN <- chambervol * tmpslope_N/chamberarea * 60
      
      # CO2
      lmfitC <- lm(ngC_cm3_CO2 ~ TimePt)
      r2_C <- summary(lmfitC)$r.squared
      tmpslope_C <- summary(lmfitC)$coefficients[2,1]
      tmpintc_C <- summary(lmfitC)$coefficients[1,1]
      # linear flux
      # mass per area^2 per time
      lmfluxC <- chambervol * tmpslope_C/chamberarea * 60
      
      # CH4
      lmfitCH <- lm(ngC_cm3_CH4 ~ TimePt)
      r2_CH <- summary(lmfitCH)$r.squared
      tmpslope_CH <- summary(lmfitCH)$coefficients[2,1]
      tmpintc_CH <- summary(lmfitCH)$coefficients[1,1]
      # linear flux
      # mass per area^2 per time
      lmfluxCH <- chambervol * tmpslope_CH/chamberarea * 60
      
      ###### make sure that you understand the flux calcs 
      ###### (what is the unit that we're trying to cancel out by multiplying by chamber volume?)
      
      
      ## get quadratic model and info
      
      # N
      modelquadN <- lm(ngN_cm3_N2O ~ poly(TimePt, 2, raw=TRUE))
      quadr2_N <- summary(modelquadN)$r.squared
      quadslope_N <- summary(modelquadN)$coefficients[2,1]
      quad2der_N <- summary(modelquadN)$coefficients[3,1]
      quadinterc_N <- summary(modelquadN)$coefficients[1,1]
      # quadratic flux
      # mass per area^2 per time
      quadfluxN <- chambervol * quadslope_N/chamberarea * 60
      
      # C
      modelquadC <- lm(ngC_cm3_CO2 ~ poly(TimePt, 2, raw=TRUE))
      quadr2_C <- summary(modelquadC)$r.squared
      quadslope_C <- summary(modelquadC)$coefficients[2,1]
      quad2der_C <- summary(modelquadC)$coefficients[3,1]
      quadinterc_C <- summary(modelquadC)$coefficients[1,1]
      # quadratic flux
      # mass per area^2 per time
      quadfluxC <- chambervol * quadslope_C/chamberarea * 60
      
      # CH
      modelquadCH <- lm(ngC_cm3_CH4 ~ poly(TimePt, 2, raw=TRUE))
      quadr2_CH <- summary(modelquadCH)$r.squared
      quadslope_CH <- summary(modelquadCH)$coefficients[2,1]
      quad2der_CH <- summary(modelquadCH)$coefficients[3,1]
      quadinterc_CH <- summary(modelquadCH)$coefficients[1,1]
      # quadratic flux
      # mass per area^2 per time
      quadfluxCH <- chambervol * quadslope_CH/chamberarea * 60
      
      ## put info into running output file
      
      # put into a df (N2O)
      outputdf <- data.frame(Site,LUtype,Chamber,SampleDate)
      outputdf$GasType <- c("N2O")
      outputdf$LinearR2 <- r2_N
      outputdf$LinearSlope <- tmpslope_N
      outputdf$LinearInt <- tmpintc_N
      outputdf$LinearFlux <- lmfluxN
      outputdf$QuadR2 <- quadr2_N
      outputdf$QuadSlope <- quadslope_N
      outputdf$Quad2der <- quad2der_N
      outputdf$QuadFlux <- quadfluxN
      outputdf$FluxID <- fluxhere
      # bind onto output table
      outputdffull <- rbind(outputdffull, outputdf)
      # put into a df (CO2)
      outputdf <- data.frame(Site,LUtype,Chamber,SampleDate)
      outputdf$GasType <- c("CO2")
      outputdf$LinearR2 <- r2_C
      outputdf$LinearSlope <- tmpslope_C
      outputdf$LinearInt <- tmpintc_C
      outputdf$LinearFlux <- lmfluxC
      outputdf$QuadR2 <- quadr2_C
      outputdf$QuadSlope <- quadslope_C
      outputdf$Quad2der <- quad2der_C
      outputdf$QuadFlux <- quadfluxC
      outputdf$FluxID <- fluxhere
      # bind onto output table
      outputdffull <- rbind(outputdffull, outputdf)
      # put into a df (CH4)
      outputdf <- data.frame(Site,LUtype,Chamber,SampleDate)
      outputdf$GasType <- c("CH4")
      outputdf$LinearR2 <- r2_CH
      outputdf$LinearSlope <- tmpslope_CH
      outputdf$LinearInt <- tmpintc_CH
      outputdf$LinearFlux <- lmfluxCH
      outputdf$QuadR2 <- quadr2_CH
      outputdf$QuadSlope <- quadslope_CH
      outputdf$Quad2der <- quad2der_CH
      outputdf$QuadFlux <- quadfluxCH
      outputdf$FluxID <- fluxhere
      # bind onto output table
      outputdffull <- rbind(outputdffull, outputdf)
      
      
      ## make plot images and save as pdf
      pdf(paste(pathsavefigs,"AppendFlux_", fluxhere, ".pdf", sep=""), width=11, height=3.5)
      par(mfrow=c(1,3),oma=c(0,0,2,0),mar = c(5.1, 4.1, 2.1, 2.1))
      
      # N
      # make the plot
      plot(TimePt, ngN_cm3_N2O)
      abline(lmfitN, col="darkslateblue", lwd=2)
      # smooth quad line
      lines(x=seq(min(TimePt), max(TimePt), len=100), y=predict(modelquadN, data.frame(TimePt=seq(min(TimePt), max(TimePt), len=100))), col="darkseagreen4", lwd=2)
      # lines(TimePt, predict(modelquadN)) # choppy quad line
      # add info to the plot
      tmplab = vector('expression',3)
      tmplab[1] = "Linear Fit"
      tmplab[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                             list(MYVALUE = format(r2_N,dig=4)))[2]
      tmplab[3] = paste("y = ", round(tmpslope_N,4), "x + ", round(tmpintc_N,4), sep="")  
      legend('topleft', legend = tmplab, bty = 'n',cex=0.7)
      tmplab2 = vector('expression',3)
      tmplab2[1] = "Quadratic Fit"
      tmplab2[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                              list(MYVALUE = format(quadr2_N,dig=4)))[2]
      tmplab2[3] = paste("y = ", round(quadslope_N,4), "x + ", round(quad2der_N,4), "x^2 + ", round(quadinterc_N,4), sep="")  
      legend('bottomright', legend = tmplab2, bty = 'n',cex=0.7)
      
      # C
      # make the plot
      plot(TimePt, ngC_cm3_CO2)
      abline(lmfitC, col="darkslateblue", lwd=2)
      # smooth quad line
      lines(x=seq(min(TimePt), max(TimePt), len=100), y=predict(modelquadC, data.frame(TimePt=seq(min(TimePt), max(TimePt), len=100))), col="darkseagreen4", lwd=2)
      # lines(TimePt, predict(modelquadC)) # choppy quad line
      # add info to the plot
      tmplab = vector('expression',3)
      tmplab[1] = "Linear Fit"
      tmplab[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                             list(MYVALUE = format(r2_C,dig=4)))[2]
      tmplab[3] = paste("y = ", round(tmpslope_C,4), "x + ", round(tmpintc_C,4), sep="")  
      legend('topleft', legend = tmplab, bty = 'n',cex=0.7)
      tmplab2 = vector('expression',3)
      tmplab2[1] = "Quadratic Fit"
      tmplab2[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                              list(MYVALUE = format(quadr2_C,dig=4)))[2]
      tmplab2[3] = paste("y = ", round(quadslope_C,4), "x + ", round(quad2der_C,4), "x^2 + ", round(quadinterc_C,4), sep="")  
      legend('bottomright', legend = tmplab2, bty = 'n',cex=0.7)
      
      # CH
      # make the plot
      plot(TimePt, ngC_cm3_CH4)
      abline(lmfitCH, col="darkslateblue", lwd=2)
      # smooth quad line
      lines(x=seq(min(TimePt), max(TimePt), len=100), y=predict(modelquadCH, data.frame(TimePt=seq(min(TimePt), max(TimePt), len=100))), col="darkseagreen4", lwd=2)
      # lines(TimePt, predict(modelquadCH)) # choppy quad line
      # add info to the plot
      tmplab = vector('expression',3)
      tmplab[1] = "Linear Fit"
      tmplab[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                             list(MYVALUE = format(r2_CH,dig=4)))[2]
      tmplab[3] = paste("y = ", round(tmpslope_CH,4), "x + ", round(tmpintc_CH,4), sep="")  
      legend('topleft', legend = tmplab, bty = 'n',cex=0.7)
      tmplab2 = vector('expression',3)
      tmplab2[1] = "Quadratic Fit"
      tmplab2[2] = substitute(expression(italic(R)^2 == MYVALUE), 
                              list(MYVALUE = format(quadr2_CH,dig=4)))[2]
      tmplab2[3] = paste("y = ", round(quadslope_CH,4), "x + ", round(quad2der_CH,4), "x^2 + ", round(quadinterc_CH,4), sep="")  
      legend('bottomright', legend = tmplab2, bty = 'n',cex=0.7)
      
      title(fluxhere, outer=TRUE)
      dev.off()
      
    } else {
      
      print(paste("there were more than two no pressure vials for ", fluxhere, ", so we skipped it", sep=""))
      
    }
    
  } else if(checklength == 3) { 
    
    print(paste("there were only three samples for ", fluxhere, "; skipping that chamber", sep=""))
    
  } else {
    
    print(paste("there weren't either three or four samples for ", fluxhere, "; skipping that chamber", sep=""))
    
  }
  
}




## save outputdffull as csv file
write.csv(outputdffull, file=paste(pathsavefiles, "fluxesfull.csv", sep = ""), row.names=FALSE)  


## save as pdf figures as one pdf doc
setwd(pathsavefigs)
ff <- dir(pattern="AppendFlux")
outFileName <- "Append_FluxFiguresFull.pdf"
## Make a system call to pdftk
system2(command = "pdftk", args = c(shQuote(ff), "cat output", shQuote(outFileName)))





########################################################################
# END LOOP





########################################################################
# NOTES AND TESTING

# ran this all the way through for "SM_A_2014.02.20" and works!
# some fluxes are slightly off (for instance, fluxhere <- "SM_A_2014.02.14") because in the excel it was temperature corrected to 25degC and in this code I temperature corrected everything to 20degC

# done for CH4 on 24-Feb-2015




