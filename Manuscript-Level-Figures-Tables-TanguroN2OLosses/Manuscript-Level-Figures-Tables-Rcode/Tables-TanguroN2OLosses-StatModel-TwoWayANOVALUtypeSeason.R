# Tables-TanguroN2OLosses-StatModel-TwoWayANOVALUtypeSeason.R
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
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/ANOVA two way LUtype season/"
pathsavetab = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/ANOVA two way LUtype season/"

# functions
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
source("~/Documents/GITHUB/RPersonalFunctionsChristine/regr_table.r")



########################################################################
# WHAT DEPENDENT VARIABLES TO LOOK AT?

# just search and replace whatever variable is currently in use

# these are the variables:
# NO3_N_mgNg, NH4_N_mgNg, NO3_N_mgNg_FinalMinusInitial_perDay, NH4_N_mgNg_FinalMinusInitial_perDay
# DONE: LinearFlux for each of the three gases, SoilMoisPercent, 

#### why don't I have dry season labile N data????  where is this????


# lme can't handle columns with any NAs
fluxesfullmerge_noNA <- subset(fluxesfullmerge, !is.na(NO3_N_mgNg))

# get summary info to put into the table
statsummarytable <- summarySE(data=fluxesfullmerge_noNA, measurevar="NO3_N_mgNg", groupvars=c("LUtype","GasType","Season"), na.rm=TRUE)



########################################################################
# TWO-WAY REPEATED MEASURES ANOVA

# read these on lme() for two-way repeated measures
# http://www.researchgate.net/post/Has_anyone_performed_linear_mixed_model_with_repeated_measures
# http://www.jason-french.com/tutorials/repeatedmeasures.html

# get the data for gas of interest
# doesn't matter for the non-gas variables, but cuts the fact that each other var is reported 3 times
fluxesfullmerge_noNA_gas <- subset(fluxesfullmerge_noNA, GasType=="CH4")

# put the week in (repeated measures time unit)
fluxesfullmerge_noNA_gas$WeekReference <- week(fluxesfullmerge_noNA_gas$SampleDate2)
# get rid of NAs
fluxesfullmerge_noNA_gas <- subset(fluxesfullmerge_noNA_gas, !is.na(NO3_N_mgNg))

# set factors
fluxesfullmerge_noNA_gas$WeekReference <- as.factor(fluxesfullmerge_noNA_gas$WeekReference)
fluxesfullmerge_noNA_gas$LUtype <- as.factor(fluxesfullmerge_noNA_gas$LUtype)
fluxesfullmerge_noNA_gas$Site <- as.factor(fluxesfullmerge_noNA_gas$Site)
fluxesfullmerge_noNA_gas$Season <- as.factor(fluxesfullmerge_noNA_gas$Season)

#### gas fluxes are NOT log transformed

# lme version of the one-way test
lme_twoway = lme(NO3_N_mgNg ~ LUtype * Season, data=fluxesfullmerge_noNA_gas, random = ~1|WeekReference/Site, na.action = na.omit)

# summary info; note different F values (apparently this is a known consequence)
anova(lme_twoway)

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twoway, linfct=mcp(LUtype = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(lme_twoway, linfct=mcp(Season = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# post-hoc tests on interactions are dealt with at the end only for the tests in which they are needed

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twoway), file=paste(path.expand(pathsavetab), "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,col=fluxesfullmerge_noNA_gas$LUtype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twoway),sqrt(abs(resid(lme_twoway))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twoway,col=fluxesfullmerge_noNA_gas$LUtype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,LUtype~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/NO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,Season~resid(.))
dev.off()



#### gas fluxes ARE log transformed

# deal with the log transformation
fluxesfullmerge_noNA_gas$tmplogvar <- log(abs(fluxesfullmerge_noNA_gas$NO3_N_mgNg))
fluxesfullmerge_noNA_gas2 <- subset(fluxesfullmerge_noNA_gas, is.finite(fluxesfullmerge_noNA_gas$tmplogvar))
# lme version of the one-way test
lme_twoway = lme(tmplogvar ~ LUtype * Season, data=fluxesfullmerge_noNA_gas, random = ~1|WeekReference/Site, na.action = na.omit)

# summary info; note different F values (apparently this is a known consequence)
anova(lme_twoway)

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twoway, linfct=mcp(LUtype = "Tukey")), test = adjusted(type = "bonferroni"))
# this basically means nothing - see the individual depth one-way ANOVAs below
posthoc_glht_2 <- summary(glht(lme_twoway, linfct=mcp(Season = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twoway), file=paste(path.expand(pathsavetab), "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,col=fluxesfullmerge_noNA_gas$LUtype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twoway),sqrt(abs(resid(lme_twoway))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twoway,col=fluxesfullmerge_noNA_gas$LUtype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,LUtype~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/logNO3_N_mgNg_LUtypeSeason_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway,Season~resid(.))
dev.off()


#####################################################################
# INTERACTION TERMS

# plotting interaction
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/factorial.html

# how to interpret
# chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# possible test options
# chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/http://pages.uoregon.edu/stevensj/interaction.pdf

# code for the first test in the above doc, which I'll use here
# https://stats.stackexchange.com/questions/23234/how-to-perform-post-hoc-comparison-on-interaction-term-with-mixed-effects-model

# which tests had a significant interaction?
# list: LinearFlux (CO2)









# 
# 
# 
# 
# 
# # 
# # ########################################################################
# # # NOTES AND TESTING
# # 
# # 
# # 
# # 
