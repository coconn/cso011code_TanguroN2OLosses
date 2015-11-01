# Tables-TanguroN2OLosses-StatModel-OneWayANOVALUtype.R
# statistical models and tables to go in Tanguro trace gas paper 
# 
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in:
#

# UNITS LABELING: 1 microgram (ug) = 1000 nanograms (ng), so micrograms are 1000 times bigger.  CO2 fluxes are in migrograms/cm2/h, N2O are in nanograms/cm2/h.


########################################################################
# BRING IN DATA / PREP

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)
library(tidyr)
library(magrittr)
library(lubridate)
library(xlsx)
library(nlme)      ## for lme()
library(multcomp)  ## for multiple comparison stuff
#library(scales)
#library(data.table)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# bring in site-date combination summary data (combines info from multiple chambers)
sitedatesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterSiteDateSummary.csv", stringsAsFactors=FALSE)

# get rid of random NA rows
sitedatesummary <- subset(sitedatesummary, sitedatesummary$easysitename!="NA")
fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/ANOVA figures/"
pathsavetab = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/ANOVA figures/"

# functions
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
source("~/Documents/GITHUB/RPersonalFunctionsChristine/regr_table.r")



########################################################################
# WHAT DEPENDENT VARIABLES TO LOOK AT?

# just search and replace whatever variable is currently in use

# these are the variables:
# 
# DONE: LinearFlux for each of the three gases, SoilMoisPercent, NO3_N_mgNg, NH4_N_mgNg, NO3_N_mgNg_FinalMinusInitial_perDay, NH4_N_mgNg_FinalMinusInitial_perDay

# lme can't handle columns with any NAs
fluxesfullmerge_noNA <- subset(fluxesfullmerge, !is.na(NH4_N_mgNg_FinalMinusInitial_perDay))

# get summary info to put into the table
statsummarytable <- summarySE(data=fluxesfullmerge_noNA, measurevar="NH4_N_mgNg_FinalMinusInitial_perDay", groupvars=c("LUtype"), na.rm=TRUE)


########################################################################
# LAND USE DIFFERENCES ONE-WAY REPEATED MEASURES ANOVA

# see these websites
#
# general how to do this
# http://www.r-bloggers.com/two-way-anova-with-repeated-measures/
# see http://ww2.coastal.edu/kingw/statistics/R-tutorials/repeated.html
# "in principle at least, we can see the "store" effect within each and every "subject" (grocery item)"
# aov.out = aov(price ~ store + Error(subject/store), data=groceries2)
#
# post hoc tests
# need to switch to lme package from aov()
# https://stats.stackexchange.com/questions/14078/post-hoc-test-after-anova-with-repeated-measures-using-r
# https://stats.stackexchange.com/questions/575/post-hocs-for-within-subjects-tests
# https://stat.ethz.ch/pipermail/r-help/2008-May/163433.html
# 
# what does error:within mean?
# see this example; looks like it's related to the sampletype pseudoreplication
# http://www.personality-project.org/r/r.anova.html
#

# fluxes summarized only by land use
#rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="LinearFlux", groupvars=c("GasType","LUtype"), na.rm=TRUE)

# get the data for gas of interest
# doesn't matter for the non-gas variables, but cuts the fact that each other var is reported 3 times
fluxesfullmerge_noNA_gas <- subset(fluxesfullmerge_noNA, GasType=="N2O")

# put the week in (repeated measures time unit)
fluxesfullmerge_noNA_gas$WeekReference <- week(fluxesfullmerge_noNA_gas$SampleDate2)
# get rid of NAs
fluxesfullmerge_noNA_gas <- subset(fluxesfullmerge_noNA_gas, !is.na(WeekReference))

# set factors
fluxesfullmerge_noNA_gas$WeekReference <- as.factor(fluxesfullmerge_noNA_gas$WeekReference)
fluxesfullmerge_noNA_gas$LUtype <- as.factor(fluxesfullmerge_noNA_gas$LUtype)
fluxesfullmerge_noNA_gas$Site <- as.factor(fluxesfullmerge_noNA_gas$Site)

#### gas fluxes are NOT log transformed

# lme version of the one-way test
# model with measures repeated by week (fixed), site (random) is nested within land use (fixed)
lme_onewayLUtype = lme(NH4_N_mgNg_FinalMinusInitial_perDay ~ LUtype, data=fluxesfullmerge_noNA_gas, random = ~1|WeekReference/Site)
# summary info; note different F values (apparently this is a known consequence)
anova(lme_onewayLUtype)

# post hoc test
posthoc_glht <- summary(glht(lme_onewayLUtype, linfct=mcp(LUtype = "Tukey")), test = adjusted(type = "bonferroni"))

# summary info
summary(posthoc_glht)

# save posthoc test as a nice table later
x <- posthoc_glht
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_onewayLUtype), file=paste(path.expand(pathsavetab), "stats-tables/NH4_N_mgNg_FinalMinusInitial_perDay_onewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/NH4_N_mgNg_FinalMinusInitial_perDay_onewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "oneway_NH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewayLUtype,col=fluxesfullmerge_noNA_gas$LUtype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "oneway_NH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewayLUtype),sqrt(abs(resid(lme_onewayLUtype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "oneway_NH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewayLUtype,col=fluxesfullmerge_noNA_gas$LUtype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "oneway_NH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewayLUtype,LUtype~resid(.))
dev.off()


#### gas fluxes ARE log transformed

# lme version of the one-way test
# model with measures repeated by week (fixed), site (random) is nested within land use (fixed)
fluxesfullmerge_noNA_gas$tmplogvar <- log(abs(fluxesfullmerge_noNA_gas$NH4_N_mgNg_FinalMinusInitial_perDay))
fluxesfullmerge_noNA_gas2 <- subset(fluxesfullmerge_noNA_gas, is.finite(fluxesfullmerge_noNA_gas$tmplogvar))
lme_onewayLUtype = lme(tmplogvar ~ LUtype, data=fluxesfullmerge_noNA_gas2, random = ~1|WeekReference/Site)
# summary info; note different F values (apparently this is a known consequence)
anova(lme_onewayLUtype)

# post hoc test
posthoc_glht <- summary(glht(lme_onewayLUtype, linfct=mcp(LUtype = "Tukey")), test = adjusted(type = "bonferroni"))

# summary info
summary(posthoc_glht)

# save posthoc test as a nice table later
x <- posthoc_glht
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_onewayLUtype), file=paste(path.expand(pathsavetab), "stats-tables/logNH4_N_mgNg_FinalMinusInitial_perDay_onewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/logNH4_N_mgNg_FinalMinusInitial_perDay_onewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "oneway_logNH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewayLUtype,col=fluxesfullmerge_noNA_gas$LUtype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "oneway_logNH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewayLUtype),sqrt(abs(resid(lme_onewayLUtype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "oneway_logNH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewayLUtype,col=fluxesfullmerge_noNA_gas$LUtype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "oneway_logNH4_N_mgNg_FinalMinusInitial_perDay_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewayLUtype,LUtype~resid(.))
dev.off()








# 
# 
# 
# ########################################################################
# # POST FERT SPIKE, ONE-WAY REPEATED MEASURES ANOVA
# 
# # go to each maize site and compare the post-fert period to the same weeks in soy and forest
# 
# 
# # what weeks are postfert in maize?
# tmp <- subset(fluxesfullmerge_noNA_gas, fluxesfullmerge_noNA_gas$postfertcat=="postfert")
# tmpweeks <- unique(tmp$WeekReference)
# # put in the post fertilization category
# fluxesfullmerge_noNA_gas$postfertcat2 <- "notpostfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[1]] <- "postfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[2]] <- "postfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[3]] <- "postfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[4]] <- "postfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[5]] <- "postfert"
# fluxesfullmerge_noNA_gas$postfertcat2[fluxesfullmerge_noNA_gas$WeekReference==tmpweeks[6]] <- "postfert"
# # get rid of NAs
# fluxesfullmerge_noNA_gas <- subset(fluxesfullmerge_noNA_gas, !is.na(postfertcat2))
# 
# # set factors
# fluxesfullmerge_noNA_gas$WeekReference <- as.factor(fluxesfullmerge_noNA_gas$WeekReference)
# fluxesfullmerge_noNA_gas$LUtype <- as.factor(fluxesfullmerge_noNA_gas$LUtype)
# fluxesfullmerge_noNA_gas$Site <- as.factor(fluxesfullmerge_noNA_gas$Site)
# fluxesfullmerge_noNA_gas$postfertcat2 <- as.factor(fluxesfullmerge_noNA_gas$postfertcat2)
# 
# 
# ########################################################################
# # LAND USE DIFFERENCES AND ROW-INTERROW COMPARISONS TWO-WAY REPEATED MEASURES ANOVA
# 
# 
# 
# 
# ########################################################################
# # LAND USE DIFFERENCES AND SEASONAL COMPARISONS TWO-WAY REPEATED MEASURES ANOVA
# 
# 
# 
# 
# 
# 
# # lme version of the two-way test (time measured by week)
# lme_twowayLUtypeWeekReference = lme(LinearFlux ~ LUtype + WeekReference, data=fluxesfullmerge_noNA_gas, random = ~1|Site)
# anova(lme_twowayLUtypeWeekReference)
# 
# # post hoc test
# posthoc_glht <- summary(glht(lme_twowayLUtypeWeekReference, linfct=mcp(WeekReference = "Tukey")), test = adjusted(type = "bonferroni"))
# 
# # summary info
# summary(posthoc_glht)
# 
# 
# 
# 
# # lme version of the two-way test (time measured by fertilization category)
# lme_twowayLUtypepostfertcat2 = lme(LinearFlux ~ LUtype * postfertcat2, data=fluxesfullmerge_noNA_gas, random = ~1|WeekReference/Site)
# anova(lme_twowayLUtypepostfertcat2)
# 
# # post hoc test
# posthoc_glht <- summary(glht(lme_twowayLUtypepostfertcat2, linfct=mcp(LUtype = "Tukey")), test = adjusted(type = "bonferroni"))
# 
# # summary info
# summary(posthoc_glht)
# 
# 
# 
# 
# postfertcat
# 
# 
# lme_twowaysampletype = lme(netmineralization_ugN_per_g_per_d ~ sampletype + depth, data=data2_noNA, random = ~1|month/depth/sampletype)
# 
# # summary info; note different F values (apparently this is a known consequence)
# summary(aov_twowaysampletype)
# anova(lme_twowaysampletype)
# 
# 
# 
# 
# # aov version of the one-way test
# aov_onewaysampletype = aov(LinearFlux ~ LUtype + Error(WeekReference/(LUtype)), data=fluxesfullmerge_noNA_gas)
# summary(aov_onewaysampletype)
# 
# # post hoc test
# posthoc_glht <- summary(glht(lme_onewaysampletype, linfct=mcp(sampletype = "Tukey")), test = adjusted(type = "bonferroni"))
# 
# # summary info
# summary(posthoc_glht)
# 
# # save posthoc test as a nice table later
# x <- posthoc_glht
# pq<-summary(x)$test
# mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
# error <- attr(pq$pvalues, "error")
# pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
# colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
# 
# 
# 
# 
# 
# ########################################################################
# # ONE-WAY REPEATED MEASURES ANOVA
# 
# # see these websites
# #
# # general how to do this
# # http://www.r-bloggers.com/two-way-anova-with-repeated-measures/
# # see http://ww2.coastal.edu/kingw/statistics/R-tutorials/repeated.html
# # "in principle at least, we can see the "store" effect within each and every "subject" (grocery item)"
# # aov.out = aov(price ~ store + Error(subject/store), data=groceries2)
# #
# # post hoc tests
# # need to switch to lme package from aov()
# # https://stats.stackexchange.com/questions/14078/post-hoc-test-after-anova-with-repeated-measures-using-r
# # https://stats.stackexchange.com/questions/575/post-hocs-for-within-subjects-tests
# # https://stat.ethz.ch/pipermail/r-help/2008-May/163433.html
# # 
# # what does error:within mean?
# # see this example; looks like it's related to the sampletype pseudoreplication
# # http://www.personality-project.org/r/r.anova.html
# #
# 
# # lme can't handle columns with any NAs
# data2_noNA <- subset(data2, !is.na(netmineralization_ugN_per_g_per_d))
# 
# #data2_noNA<-data2[is.na(data2$netmineralization_ugN_per_g_per_d)==F,]
# #data2_noNA <- subset(data2_noNA, netmineralization_ugN_per_g_per_d>0.000001) # why isn't NH4 ugN per gsoil working
# 
# # aov version of the one-way test
# aov_onewaysampletype = aov(netmineralization_ugN_per_g_per_d ~ sampletype + Error(month/(sampletype)), data=data2_noNA)
# # lme version of the one-way test
# lme_onewaysampletype = lme(netmineralization_ugN_per_g_per_d ~ sampletype, data=data2_noNA, random = ~1|month)
# 
# # summary info; note different F values (apparently this is a known consequence)
# summary(aov_onewaysampletype)
# anova(lme_onewaysampletype)
# 
# # post hoc test
# posthoc_glht <- summary(glht(lme_onewaysampletype, linfct=mcp(sampletype = "Tukey")), test = adjusted(type = "bonferroni"))
# 
# # summary info
# summary(posthoc_glht)
# 
# # save posthoc test as a nice table later
# x <- posthoc_glht
# pq<-summary(x)$test
# mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
# error <- attr(pq$pvalues, "error")
# pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
# colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
# 
# 
# 
# 
# 
# 
# ##### this analysis does not exist yet... all of the below is for the pit chapter, but it's here as a placeholder.
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ########################################################################
# # BRING IN DATA / PREP
# 
# library(ggplot2)
# library(gridExtra)
# library(scales)
# library(plyr)
# library(data.table)
# library(lubridate)
# library(reshape2)
# library(tidyr)
# library(magrittr)
# 
# # where to save outputs
# pathsavetab = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitTables/"
# pathsavefigs = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Analyses/PitGasFigures/"
# 
# # regr_table
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/regr_table.r")
# 
# # bring in stuff
# pitTDRsummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitTDRsummarytable.csv", stringsAsFactors=FALSE)
# 
# pitgassummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitgassummary.csv", stringsAsFactors=FALSE)
# 
# # get rid of rows with NA in sample depth
# pitTDRsummary <- subset(pitTDRsummary, !is.na(pitTDRsummary$sampledepth))
# 
# 
# ########################################################################
# # WIDEN pitTDRsummary SO IT MATCHES pitgassummary
# 
# # mean
# keep1 <- c("PitID", "YearMonth", "DataType", "measurement", "sampledepth")
# tmp <- subset(pitTDRsummary, select = keep1)
# tmp2 <- tmp %>% spread(DataType, measurement)
# colnames(tmp2)[4] <- "meandegC"; colnames(tmp2)[5] <- "meanVW"
# 
# # sd
# keep1 <- c("PitID", "YearMonth", "DataType", "sd", "sampledepth")
# tmp3 <- subset(pitTDRsummary, select = keep1)
# tmp4 <- tmp3 %>% spread(DataType, sd)
# colnames(tmp4)[4] <- "sddegC"; colnames(tmp4)[5] <- "sdVW"
# 
# # se
# keep1 <- c("PitID", "YearMonth", "DataType", "se", "sampledepth")
# tmp5 <- subset(pitTDRsummary, select = keep1)
# tmp6 <- tmp5 %>% spread(DataType, se)
# colnames(tmp6)[4] <- "sedegC"; colnames(tmp6)[5] <- "seVW"
# 
# # join
# pitTDRsummary2 <- join(x = tmp2, y = tmp4, by = c("PitID", "YearMonth", "sampledepth"))
# pitTDRsummary2 <- join(x = pitTDRsummary2, y = tmp6, by = c("PitID", "YearMonth", "sampledepth"))
# 
# # name compliance
# pitTDRsummary2$PitID[grepl("Mutum", pitTDRsummary2$PitID)] <- "MU"
# 
# 
# ########################################################################
# # GET READY TO MERGE, CREATE MODEL DATAFRAME
# 
# # get cols ready in pitgassummary
# 
# # year-month combo variable
# pitgassummary <- transform(pitgassummary, YearMonth = paste(year(pitgassummary$SampleDate),month(pitgassummary$SampleDate),sep="-"))
# 
# # need: PitID YearMonth sampledepth
# keep <- c(which(colnames(pitgassummary)=="pitID"),which(colnames(pitgassummary)=="YearMonth"),which(colnames(pitgassummary)=="sampledepth"))
# var1 <- which(colnames(pitgassummary)=="meanN2Oppm")
# varend <- which(colnames(pitgassummary)=="ciCH4ppm")
# pitgassummary <- pitgassummary[,c(keep,var1:varend)]
# 
# # names
# colnames(pitgassummary)[1] <- "PitID"
# 
# # merge
# pitmodeldf <- merge(x = pitTDRsummary2, y = pitgassummary, by = c("PitID", "YearMonth", "sampledepth"), all=T)
# 
# # get rid of rows with NA in N2O mean emissions
# pitmodeldf <- subset(pitmodeldf, !is.na(pitmodeldf$meanN2Oppm))
# 
# # add back Year and Month as vars
# pitmodeldf$Year <- substr(pitmodeldf$YearMonth, 1, 4)
# pitmodeldf$Month <- month(as.numeric(substr(pitmodeldf$YearMonth, 6, nchar(pitmodeldf$YearMonth))), label = TRUE)
# 
# # add forest label
# pitmodeldf$LUType <- "Forest"
# pitmodeldf$LUType[grepl("MU", pitmodeldf$PitID)] <- "Agriculture"
# 
# 
# 
# ########################################################################
# # TEMPERATURE MODEL
# 
# # temp subset
# tempsubset <- subset(pitTDRsummary, pitTDRsummary$DataType=="degC")
# 
# # model
# fit <- lm(measurement ~ PitID + Month + sampledepth, data=tempsubset)
# summary(fit) # show results
# 
# # regr_table
# fname = paste(pathsavetab, "regr_table_temp" ,sep="")
# regr_table(fit, fname)
# 
# 
# 
# ########################################################################
# # VWC MODEL
# 
# # VW subset
# vwsubset <- subset(pitTDRsummary, pitTDRsummary$DataType=="VW")
# 
# # model
# fit <- lm(measurement ~ PitID + Month + sampledepth, data=vwsubset)
# summary(fit) # show results
# 
# # regr_table
# fname = paste(pathsavetab, "regr_table_VWC" ,sep="")
# regr_table(fit, fname)
# 
# 
# 
# 
# ########################################################################
# # GAS MODELS
# 
# # these need lots of work - pretty arbitrary right now
# 
# # LU as factor
# pitmodeldf$LUType <- factor(pitmodeldf$LUType)
# 
# # N2O
# fit <- lm(meanN2Oppm ~ meandegC + meanVW + Month + sampledepth, data=pitmodeldf)
# summary(fit) # show results
# fname = paste(pathsavetab, "regr_table_N2O" ,sep="")
# regr_table(fit, fname)
# 
# # CO2
# fit <- lm(meanCO2ppm ~ meandegC + meanVW + Month, data=pitmodeldf)
# summary(fit) # show results
# fname = paste(pathsavetab, "regr_table_CO2" ,sep="")
# regr_table(fit, fname)
# 
# # CH4
# fit <- lm(log(meanCH4ppm) ~ meandegC + meanVW + Month + sampledepth, data=pitmodeldf)
# summary(fit) # show results
# fname = paste(pathsavetab, "regr_table_CH4" ,sep="")
# regr_table(fit, fname)
# 
# 
# 
# ########################################################################
# # ANOVAS WITH LUTYPE
# # diagnostic plots courtesy http://www.statmethods.net/stats/anova.html
# 
# ## Two Way Factorial Design 
# 
# # N2O
# fit <- aov(log(meanN2Oppm) ~ LUType*sampledepth, data=pitmodeldf)
# summary(fit) # show results
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# 
# # CO2
# fit <- aov(log(meanCO2ppm) ~ LUType*sampledepth, data=pitmodeldf)
# summary(fit) # show results
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# 
# # CH4
# fit <- aov(log(meanCH4ppm) ~ LUType*sampledepth, data=pitmodeldf)
# summary(fit) # show results
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# 
# 
# # One Way Anova
# 
# # N2O
# fit <- aov(log(meanN2Oppm) ~ LUType, data=pitmodeldf)
# summary(fit) # show results
# # diagnostics
# png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-N2O.png", sep=""),width=6,height=6,units="in",res=400)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# title("N2O ANOVA Diagnostics", line = -2, outer = TRUE)
# dev.off()
# # tukey hst
# TukeyHSD(fit)
# print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell
# 
# # CO2
# fit <- aov(log(meanCO2ppm) ~ LUType, data=pitmodeldf)
# summary(fit) # show results
# # diagnostics
# png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-CO2.png", sep=""),width=6,height=6,units="in",res=400)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# title("CO2 ANOVA Diagnostics", line = -2, outer = TRUE)
# dev.off()
# # tukey hst
# TukeyHSD(fit)
# print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell
# 
# # CH4
# fit <- aov(log(meanCH4ppm) ~ LUType, data=pitmodeldf)
# summary(fit) # show results
# # diagnostics
# png(file = paste(pathsavetab, "ANOVAdiagnostics/ANOVAdiagnostics-CH4.png", sep=""),width=6,height=6,units="in",res=400)
# layout(matrix(c(1,2,3,4),2,2)) # optional layout 
# plot(fit) # diagnostic plots
# title("CH4 ANOVA Diagnostics", line = -2, outer = TRUE)
# dev.off()
# # tukey hsd
# TukeyHSD(fit)
# print(model.tables(fit,"means"),digits=3)       #report the means and the number of subjects/cell
# 
# 
# 
# ########################################################################
# # BOXPLOTS
# 
# # only land use
# png(file = paste(pathsavefigs, "soilpit-gas-anovaboxplot-nodepth.png", sep=""),width=6,height=4,units="in",res=400)
# layout(matrix(c(1,2,3),1,3)) # optional layout
# # N2O
# boxplot(meanN2Oppm ~ LUType,data=pitmodeldf, ylab="N2O (ppm)")  
# # CO2
# boxplot(meanCO2ppm ~ LUType,data=pitmodeldf, ylab="CO2 (ppm)")  
# text(x=1, y=14600, "p < 0.001", pos=3, cex=0.9)
# text(x=1, y=13200, "***", pos=3, cex=1.4)
# # CH4
# boxplot(meanCH4ppm ~ LUType,data=pitmodeldf, ylab="CH4 (ppm)")  
# dev.off()
# 
# # depth and land use
# 
# # for all box plots
# atvec = c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19,20) # how to group boxes
# colorvec = c("light grey","white")
# namesvec = c("Ag 15","For 15","Ag 40","For 40","Ag 75","For 75","Ag 150","For 150","Ag 250","For 250","Ag 350","For 350","Ag 450","For 450")
# 
# png(file = paste(pathsavefigs, "soilpit-gas-anovaboxplot-depthgroup.png", sep=""),width=12,height=4,units="in",res=400)
# layout(matrix(c(1,2,3),1,3)) # optional layout
# # N2O
# boxplot(meanN2Oppm ~ LUType + sampledepth,data=pitmodeldf, ylab="N2O (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
# legend("topright", c("Agriculture", "Forest"), fill=colorvec)
# # CO2
# boxplot(meanCO2ppm ~ LUType + sampledepth,data=pitmodeldf, ylab="CO2 (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
# legend("topright", c("Agriculture", "Forest"), fill=colorvec)
# # CH4
# boxplot(meanCH4ppm ~ LUType + sampledepth,data=pitmodeldf, ylab="CH4 (ppm)", col=colorvec,  las = 2, at = atvec, names=namesvec)  
# legend("topright", c("Agriculture", "Forest"), fill=colorvec)
# dev.off()
# 
# 
# ########################################################################
# # NOTES AND TESTING
# 
# 
# 
# 
