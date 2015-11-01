# Tables-TanguroN2OLosses-AnnualEst.R
# statistical models and tables to go in Tanguro trace gas paper 
# 
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in:
#


########################################################################
# BRING IN DATA / PREP

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)
library(reshape2)
library(tidyr)
library(magrittr)

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# get rid of random NA rows
fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/"


########################################################################
# SUMMARIZE DATA BY WET, DRY, OR POSTFERT PERIODS

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# fluxes, annual estimate summary
annualestsummary <- summarySE(data=fluxesfullmerge, measurevar="LinearFlux", groupvars=c("GasType","LUtype","annualest"), na.rm=TRUE)

# weird row where gas == NA
ok <- complete.cases(annualestsummary$GasType)
annualestsummary <- annualestsummary[ok,]
# get rid of table rows that are NA for annualest
tmp <- !is.na(annualestsummary$annualest)
annualestsummary <- annualestsummary[tmp,]


########################################################################
# GET ANNUAL AVERAGE

# forest: (wet season mean * 24 hr/day * wet season days) + (dry season mean * 24 hr/day * dry season days)
# soy: (wet season mean * 24 hr/day * wet season days) + (dry season mean * 24 hr/day * dry season days)
# maize: (wet season mean * 24 hr/day * (wet season days-15)) + (dry season mean * 24 hr/day * dry season days) + (postfert mean * 24 hr/day * 15 days)

# how long is the wet season vs the dry season?
wetseasondays <- 215
dryseasondays <- 150
postfertdays <- 15

# forest annual average
tabletmp <- subset(annualestsummary, annualestsummary$LUtype=="F")
# gas order
GasType <- unique(tabletmp$GasType)
# mid
dry <- tabletmp$LinearFlux[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)]
estmid <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# low est
dry <- tabletmp$LinearFlux[c(1,3,5)] - tabletmp$sd[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)] - tabletmp$sd[c(2,4,6)]
estlow <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# high est
dry <- tabletmp$LinearFlux[c(1,3,5)] + tabletmp$sd[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)] + tabletmp$sd[c(2,4,6)]
esthigh <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# put into running table
annualestcalc <- data.frame(LUtype="F",GasType,estmid,estlow,esthigh)

# soy annual average
tabletmp <- subset(annualestsummary, annualestsummary$LUtype=="S")
# gas order
GasType <- unique(tabletmp$GasType)
# mid
dry <- tabletmp$LinearFlux[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)]
estmid <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# low est
dry <- tabletmp$LinearFlux[c(1,3,5)] - tabletmp$sd[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)] - tabletmp$sd[c(2,4,6)]
estlow <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# high est
dry <- tabletmp$LinearFlux[c(1,3,5)] + tabletmp$sd[c(1,3,5)]; wet <- tabletmp$LinearFlux[c(2,4,6)] + tabletmp$sd[c(2,4,6)]
esthigh <- (wet * 24 * wetseasondays) + (dry * 24 * dryseasondays)
# put into running table
tmp <- data.frame(LUtype="S",GasType,estmid,estlow,esthigh)
annualestcalc <- rbind(annualestcalc,tmp)

# maize: (wet season mean * 24 hr/day * (wet season days-15)) + (dry season mean * 24 hr/day * dry season days) + (postfert mean * 24 hr/day * 15 days)

# maize annual average
tabletmp <- subset(annualestsummary, annualestsummary$LUtype=="M")
# gas order
GasType <- unique(tabletmp$GasType)
# mid
dry <- tabletmp$LinearFlux[c(1,4,7)]; wet <- tabletmp$LinearFlux[c(3,6,9)]; fert <- tabletmp$LinearFlux[c(2,5,8)]
estmid <- (wet * 24 * (wetseasondays-postfertdays)) + (dry * 24 * dryseasondays) + (fert * 24 * postfertdays)
# low est
dry <- tabletmp$LinearFlux[c(1,4,7)] - tabletmp$sd[c(1,4,7)]; wet <- tabletmp$LinearFlux[c(3,6,9)] - tabletmp$sd[c(3,6,9)]; fert <- tabletmp$LinearFlux[c(2,5,8)] - tabletmp$sd[c(2,5,8)]
estlow <- (wet * 24 * (wetseasondays-postfertdays)) + (dry * 24 * dryseasondays) + (fert * 24 * postfertdays)
# high est
dry <- tabletmp$LinearFlux[c(1,4,7)] + tabletmp$sd[c(1,4,7)]; wet <- tabletmp$LinearFlux[c(3,6,9)] + tabletmp$sd[c(3,6,9)]; fert <- tabletmp$LinearFlux[c(2,5,8)] + tabletmp$sd[c(2,5,8)]
esthigh <- (wet * 24 * (wetseasondays-postfertdays)) + (dry * 24 * dryseasondays) + (fert * 24 * postfertdays)
# put into running table
tmp <- data.frame(LUtype="M",GasType,estmid,estlow,esthigh)
annualestcalc <- rbind(annualestcalc,tmp)


########################################################################
# SAVE TABLES AS CSV

# seasonal summary info
write.csv(annualestsummary, paste(pathsavefigures,"annualestsummary.csv", sep=''))

# annual average estimate info
write.csv(annualestcalc, paste(pathsavefigures,"annualestcalc.csv", sep=''))


########################################################################
# MAKE BAR PLOTS WITH ANNUAL ESTIMATE

# bar graphs
factorcolors <- c("#636363","#6baed6","#3182bd") # colorbrewer qualitative

# N2O

# get data in correct form
tabletmp <- subset(annualestcalc, annualestcalc$GasType=="N2O")[,c(1,3,4,5)]
tabletmp.m <- melt(tabletmp, id.vars='LUtype')
# plot
p1 <- ggplot(data=tabletmp.m, aes(x=factor(LUtype), y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + ylab("Annual Flux N2O: ngN / cm^2 / y") + theme_bw() + scale_x_discrete(breaks=c("F", "S", "M"), labels=c("Forest", "Soybean", "Soybean/Maize")) + theme(axis.title.x = element_blank()) + scale_fill_manual(values = factorcolors, name="Annual Flux\nEstimate", breaks=c("estmid", "estlow", "esthigh"), labels=c("Mean Estimate", "Lower Bound Est.", "Higher Bound Est."))

# CO2

# get data in correct form
tabletmp <- subset(annualestcalc, annualestcalc$GasType=="CO2")[,c(1,3,4,5)]
tabletmp.m <- melt(tabletmp, id.vars='LUtype')
# plot
p2 <- ggplot(data=tabletmp.m, aes(x=factor(LUtype), y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + ylab("Annual Flux CO2: ugC / cm^2 / y") + theme_bw() + scale_x_discrete(breaks=c("F", "S", "M"), labels=c("Forest", "Soybean", "Soybean/Maize")) + theme(axis.title.x = element_blank()) + scale_fill_manual(values = factorcolors, name="Annual Flux\nEstimate", breaks=c("estmid", "estlow", "esthigh"), labels=c("Mean Estimate", "Lower Bound Est.", "Higher Bound Est."))

# CH4

# get data in correct form
tabletmp <- subset(annualestcalc, annualestcalc$GasType=="CH4")[,c(1,3,4,5)]
tabletmp.m <- melt(tabletmp, id.vars='LUtype')
# plot
p3 <- ggplot(data=tabletmp.m, aes(x=factor(LUtype), y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + ylab("Annual Flux CH4: ugC / cm^2 / y") + theme_bw() + scale_x_discrete(breaks=c("F", "S", "M"), labels=c("Forest", "Soybean", "Soybean/Maize")) + theme(axis.title.x = element_blank()) + scale_fill_manual(values = factorcolors, name="Annual Flux\nEstimate", breaks=c("estmid", "estlow", "esthigh"), labels=c("Mean Estimate", "Lower Bound Est.", "Higher Bound Est."))

# save figures

png(file = paste(pathsavefigures, "annual-estimate-N2O.png", sep=""),width=8,height=5,units="in",res=400)
p1
dev.off()

png(file = paste(pathsavefigures, "annual-estimate-CO2.png", sep=""),width=8,height=5,units="in",res=400)
p2
dev.off()

png(file = paste(pathsavefigures, "annual-estimate-CH4.png", sep=""),width=8,height=5,units="in",res=400)
p3
dev.off()

# all together
# versions with shared legend
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
png(file = paste(pathsavefigures, "annual-estimate-fluxes.png", sep=""),width=5,height=11,units="in",res=400)
grid_arrange_shared_legend(p1, p2, p3, nrow = 1, ncol = 3)
dev.off()



########################################################################
# NOTES AND TESTING





