# Figures-TanguroN2OLosses-RowInterBar.R
# 
# manuscript ready figure, bar graphs comparing row and inter-row properties
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# UNITS LABELING: 1 microgram (ug) = 1000 nanograms (ng), so micrograms are 1000 times bigger.  CO2 fluxes are in migrograms/cm2/h, N2O are in nanograms/cm2/h.


########################################################################
# BRING IN DATA / PREP

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# bring in site-date combination summary data (combines info from multiple chambers)
sitedatesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterSiteDateSummary.csv", stringsAsFactors=FALSE)

# get rid of random NA rows
sitedatesummary <- subset(sitedatesummary, sitedatesummary$easysitename!="NA")
fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/"



########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - INCLUDE POST FERTILIZATION TIME CATEGORY

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# fluxes
rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="LinearFlux", groupvars=c("GasType","LUtype","RowInter", "postfertcat"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary2$RowInter)
rowintersummary2 <- rowintersummary2[tmp,]
# weird row where gas == NA
ok <- complete.cases(rowintersummary2$GasType)
rowintersummary2 <- rowintersummary2[ok,]
# get rid of table rows that are NA for postfertcat
tmp <- !is.na(rowintersummary2$postfertcat)
rowintersummary2 <- rowintersummary2[tmp,]

# bar graphs
factorcolors <- c("#66a61e","#1f78b4","#d95f02") # colorbrewer qualitative

# statistical tests between bars
source("~/Documents/GITHUB/RPersonalFunctionsChristine/gen_data_aov_onlymeansdN.r")


########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - N2O

# plot
barplotaRI <- ggplot(data=subset(rowintersummary2,GasType=="N2O"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="N2O")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux N2O: ngN / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- subset(rowintersummary2,GasType=="N2O")
simulated_data <- gen_data_aov_onlymeansdN(datasubset$LinearFlux, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(av) # diagnostic plots
# no bars are significantly different

# two-way anova
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
aovsubset <- subset(aovsubset, aovsubset$GasType=="N2O")
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# bar 1 and bar 5 are different to p<0.1

# # put stat test info into plot
# barplotaRI <- barplotaRI + annotate("text",x=.5, y=11,label="Two-way ANOVA:\np < 0.1 (Category)", hjust = 0)
# barplotaRI <- barplotaRI + annotate("text",x=1, y=3,label="a") + annotate("text",x=2, y=2,label="ab") + annotate("text",x=3, y=11,label="b") + annotate("text",x=4, y=3,label="ab")


########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - CO2

# plot
barplotbRI <- ggplot(data=subset(rowintersummary2,GasType=="CO2"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="CO2")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux CO2: ugC / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- subset(rowintersummary2,GasType=="CO2")
simulated_data <- gen_data_aov_onlymeansdN(datasubset$LinearFlux, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# bar 7 is different from every other bar; bars 1-6 are not significantly different

# two-way anova
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
aovsubset <- subset(aovsubset, aovsubset$GasType=="CO2")
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# soy row and forest differ, soy row and maize not post I differ, soy row and maize post I differ, soy R and soy I differ, soy R and maize not post R, soy R and maize post R differ

# put stat test info into plot
# barplotbRI <- barplotbRI + annotate("text",x=.5, y=55,label="Two-way ANOVA:\np < 0.01 (Category)\np < 0.001 (Row-Inter)\np < 0.01 (Interaction)", hjust = 0)
barplotbRI <- barplotbRI + annotate("text",x=c(1, 1.76, 2.22, 2.75, 3.24, 3.75, 4.22), y=c(22, 22, 32, 23, 36, 22, 61),label=c("a","a","a","a","a","a","b"))


########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - CH4

# plot
barplotcRI <- ggplot(data=subset(rowintersummary2,GasType=="CH4"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux-sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="CH4")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux CH4: ugC / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + xlab("Category")

# one-way anova
datasubset <- subset(rowintersummary2,GasType=="CH4")
simulated_data <- gen_data_aov_onlymeansdN(datasubset$LinearFlux, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# no bars are significantly different

# two-way anova
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
aovsubset <- subset(aovsubset, aovsubset$GasType=="CH4")
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# no significant differences


########################################################################
# SAVE ROW VS. INTERROW MEAN FLUX BAR GRAPHS

# grid.arrange and save
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat.png", sep=""),width=7,height=10,units="in",res=400)
grid_arrange_shared_legend(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)
dev.off()

# only N2O plot
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-N2O.png", sep=""),width=7,height=7,units="in",res=400)
barplotaRI
dev.off()

# only CO2 plot
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-CO2.png", sep=""),width=7,height=7,units="in",res=400)
barplotbRI
dev.off()

# only N2O plot
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-CH4.png", sep=""),width=7,height=7,units="in",res=400)
barplotcRI
dev.off()


########################################################################
# ROW VS. INTERROW SOIL N BAR GRAPHS - NO3

# NO3
rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="NO3_N_mgNg", groupvars=c("LUtype","RowInter", "postfertcat"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary2$RowInter)
rowintersummary2 <- rowintersummary2[tmp,]
# get rid of table rows that are NA for postfertcat
tmp <- !is.na(rowintersummary2$postfertcat)
rowintersummary2 <- rowintersummary2[tmp,]

# plot
barplotaRI <- ggplot(data=rowintersummary2, aes(x=factor(postfertcat), y=NO3_N_mgNg, fill=RowInter)) + geom_errorbar(aes(ymax=NO3_N_mgNg+sd, ymin=NO3_N_mgNg-0.1*NO3_N_mgNg), position=position_dodge(0.9),width=.25, data=rowintersummary2) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("NO3-N mgN/g") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- rowintersummary2
simulated_data <- gen_data_aov_onlymeansdN(datasubset$NO3_N_mgNg, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# who differs? 3-1, 5-1, 6-3, 7-3, 6-5, 7-5

# two-way anova
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
fit <- aov(NO3_N_mgNg ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# who differs? from postfertcat: postfert-forestpostfert, soypostfert-postfert

# put stat test info into plot
barplotaRI <- barplotaRI + annotate("text",x=1, y=0.0018,label="a") + annotate("text",x=2, y=0.0032,label="b") + annotate("text",x=3, y=0.009,label="b") + annotate("text",x=4, y=0.0022,label="a")


########################################################################
# ROW VS. INTERROW SOIL N BAR GRAPHS - NH4

# NH4
rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="NH4_N_mgNg", groupvars=c("LUtype","RowInter", "postfertcat"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary2$RowInter)
rowintersummary2 <- rowintersummary2[tmp,]
# get rid of table rows that are NA for postfertcat
tmp <- !is.na(rowintersummary2$postfertcat)
rowintersummary2 <- rowintersummary2[tmp,]

# plot this
barplotbRI <- ggplot(data=rowintersummary2, aes(x=factor(postfertcat), y=NH4_N_mgNg, fill=RowInter)) + geom_errorbar(aes(ymax=NH4_N_mgNg+sd, ymin=NH4_N_mgNg-0.1*NH4_N_mgNg), position=position_dodge(0.9),width=.25, data=rowintersummary2) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("NH4-N mgN/g") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- rowintersummary2
simulated_data <- gen_data_aov_onlymeansdN(datasubset$NH4_N_mgNg, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# who differs? 5-1, 6-5, 7-5

# two-way ANOVA
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# who differs?

# put stat test info into plot
barplotbRI <- barplotbRI + annotate("text",x=1, y=0.006,label="a") + annotate("text",x=2, y=0.002,label="b") + annotate("text",x=3, y=0.03,label="c") + annotate("text",x=4, y=0.0022,label="b")


########################################################################
# ROW VS. INTERROW SOIL N BAR GRAPHS - NET NITR

# NO3_N_mgNg_FinalMinusInitial_perDay
rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="NO3_N_mgNg_FinalMinusInitial_perDay", groupvars=c("LUtype","RowInter", "postfertcat"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary2$RowInter)
rowintersummary2 <- rowintersummary2[tmp,]
# get rid of table rows that are NA for postfertcat
tmp <- !is.na(rowintersummary2$postfertcat)
rowintersummary2 <- rowintersummary2[tmp,]

# plot this
barplotcRI <- ggplot(data=rowintersummary2, aes(x=factor(postfertcat), y=NO3_N_mgNg_FinalMinusInitial_perDay, fill=RowInter)) + geom_errorbar(aes(ymax=NO3_N_mgNg_FinalMinusInitial_perDay+sd, ymin=NO3_N_mgNg_FinalMinusInitial_perDay-0.1*NO3_N_mgNg_FinalMinusInitial_perDay), position=position_dodge(0.9),width=.25, data=rowintersummary2) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Net nitrification (NO3), area basis, mg N m-2 day-1") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- rowintersummary2
simulated_data <- gen_data_aov_onlymeansdN(datasubset$NO3_N_mgNg_FinalMinusInitial_perDay, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# who differs? 6-5, 7-5

# two-way ANOVA
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# who differs?

# put stat test info into plot
barplotcRI <- barplotcRI + annotate("text",x=c(1, 1.76, 2.22, 2.75, 3.24, 3.75, 4.22), y=c(0.015, 0.008, 0.008, 0.015, 0.032, 0.007, 0.007),label=c("ab","ab","ab","ab","b","a","a"))


########################################################################
# ROW VS. INTERROW SOIL N BAR GRAPHS - NET AMMON

# NH4_N_mgNg_FinalMinusInitial_perDay
rowintersummary2 <- summarySE(data=fluxesfullmerge, measurevar="NH4_N_mgNg_FinalMinusInitial_perDay", groupvars=c("LUtype","RowInter", "postfertcat"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary2$RowInter)
rowintersummary2 <- rowintersummary2[tmp,]
# get rid of table rows that are NA for postfertcat
tmp <- !is.na(rowintersummary2$postfertcat)
rowintersummary2 <- rowintersummary2[tmp,]

# plot 
barplotdRI <- ggplot(data=rowintersummary2, aes(x=factor(postfertcat), y=NH4_N_mgNg_FinalMinusInitial_perDay, fill=RowInter)) + geom_errorbar(aes(ymax=NH4_N_mgNg_FinalMinusInitial_perDay+sd, ymin=NH4_N_mgNg_FinalMinusInitial_perDay-0.1*NH4_N_mgNg_FinalMinusInitial_perDay), position=position_dodge(0.9),width=.25, data=rowintersummary2) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Net ammonification (NH4), area basis, mg N m-2 day-1") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

# one-way anova
datasubset <- rowintersummary2
simulated_data <- gen_data_aov_onlymeansdN(datasubset$NH4_N_mgNg_FinalMinusInitial_perDay, datasubset$sd,datasubset$N)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
# who differs? 5-1

# two-way ANOVA
tmp <- !is.na(fluxesfullmerge$postfertcat)
aovsubset <- fluxesfullmerge[tmp,]
fit <- aov(LinearFlux ~ postfertcat*RowInter, data=aovsubset)
summary(fit) # show results
TukeyHSD(fit)
# who differs?

# put stat test info into plot
barplotdRI <- barplotdRI + annotate("text",x=c(1, 1.76, 2.22, 2.75, 3.24, 3.75, 4.22), y=c(0.002, 0.0022, 0.0022, 0.006, 0.013, 0.002, 0.002),label=c("a","a","a","b","c","a","a"))


########################################################################
# SAVE ROW VS. INTERROW SOIL N BAR GRAPHS

# NO3
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-NO3.png", sep=""),width=7,height=7,units="in",res=400)
barplotaRI
dev.off()

# NH4
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-NH4.png", sep=""),width=7,height=7,units="in",res=400)
barplotbRI
dev.off()

# net ammon
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-nitr.png", sep=""),width=7,height=7,units="in",res=400)
barplotcRI
dev.off()

# net nitr
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-ammon.png", sep=""),width=7,height=7,units="in",res=400)
barplotdRI
dev.off()

# all four together
png(file = paste(pathsavefigures, "flux-rowinter-barplots-fertcat-soilN.png", sep=""),width=11,height=11,units="in",res=400)
grid_arrange_shared_legend(barplotaRI, barplotbRI, barplotcRI, barplotdRI, nrow = 1, ncol = 4)
dev.off()



########################################################################
# NOTES AND TESTING











########################################################################
# FIGURES I DECIDED NOT TO INCLUDE IN THE FINAL MANUSCRIPT FIG SET



########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - NO DATES OR SITE

## this does a bar graph comparison of F, M, S for row-inter-row, but doesn't separate by post-fertilization or not

# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# rowintersummary <- summarySE(data=fluxesfullmerge, measurevar="LinearFlux", groupvars=c("GasType","LUtype","RowInter"), na.rm=TRUE)
# 
# # get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
# pattern <- "R|I|F"
# tmp <- grep(pattern, rowintersummary$RowInter)
# rowintersummary <- rowintersummary[tmp,]
# # weird row where gas == NA
# ok <- complete.cases(rowintersummary$GasType)
# rowintersummary <- rowintersummary[ok,]
# 
# # bar graphs
# factorcolors <- c("#66a61e","#1f78b4","#d95f02") # colorbrewer qualitative
# 
# barplotaRI <- ggplot(data=subset(rowintersummary,GasType=="N2O"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="N2O")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux N2O: ngN / cm2 / h") + xlab("Land Use Type") + theme_bw() + theme(legend.position="none")
# 
# barplotbRI <- ggplot(data=subset(rowintersummary,GasType=="CO2"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CO2")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CO2: ugC / cm2 / h") + xlab("Land Use Type") + theme_bw() + theme(legend.position="none")
# 
# barplotcRI <- ggplot(data=subset(rowintersummary,GasType=="CH4"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymin=LinearFlux-sd, ymax=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CH4")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CH4: ugC / cm2 / h") + xlab("Land Use Type") + theme_bw() 
# 
# # grid.arrange and save
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
# png(file = paste(pathsavefigures, "flux-rowinter-barplots.png", sep=""),width=7,height=10,units="in",res=400)
# grid_arrange_shared_legend(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)
# dev.off()
# 
# #grid.arrange(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)





########################################################################
# ROW VS. INTERROW LINE GRAPHS

###### the below is line graph style - too hard to read, so switch to bar graphs

# # subset
# # add col that's row, inter, or main (then in ggplot you'll set color = RowInter or linetype = RowInter or point shape)
# # those get their own meanfluxN2Ol, sdfluxN2Ol
# # pretty sure I should use plyr / ddply for this
# 
# 
# ########################################################################
# # BRING IN DATA / PREP
# 
# # where to save figure
# pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/"
# 
# library(ggplot2)
# library(gridExtra)
# library(scales)
# library(plyr)
# library(data.table)
# 
# # bring in fluxes full csv
# fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)
# fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)
# 
# # bring in site-date combination summary data (combines info from multiple chambers)
# sitedatesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterSiteDateSummary.csv", stringsAsFactors=FALSE)
# sitedatesummary$SampleDate2 <- as.Date(sitedatesummary$SampleDate2)
# 
# # get rid of random NA rows
# sitedatesummary <- subset(sitedatesummary, sitedatesummary$easysitename!="NA")
# fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")
# 
# 
# ########################################################################
# # N2O GRAPH BY SITE WITH ROW-INTERROW COMPARISONS
# 
# # Run the functions length, mean, and sd on the value of "change" for each group, 
# # broken down by sex + condition
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# fluxesfullmergetmp <- subset(fluxesfullmerge, fluxesfullmerge$GasType=="N2O")
# # summarySE
# summarytab1 <- summarySE(data=fluxesfullmergetmp, measurevar="LinearFlux", c("RowInter", "SampleDate2", "Site", "LUtype", "LUname"), na.rm=TRUE, renameallcols=TRUE)
# 
# # get rid of dates with value NA
# summarytab1 <- subset(summarytab1, summarytab1$meanLinearFlux!="NaN")
# summarytab2 <- summarytab1[1:(dim(summarytab1)[1]-1),]
# 
# # N2O flux pattern for each site
# p1 <- ggplot(summarytab2, aes(x=SampleDate2, y=meanLinearFlux, color=LUname)) + geom_errorbar(aes(ymin=meanLinearFlux-sdLinearFlux, ymax=meanLinearFlux+sdLinearFlux), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + geom_line() + theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1)) 
# 
# # add in inter-row chambers
# p1 <- p1 + geom_line(data = tmp_inter, aes(x = SampleDate2, y = meanLinearFlux), linetype="dotted", color="black") + geom_point(data = tmp_inter, aes(x = SampleDate2, y = meanLinearFlux), colour="black") + geom_errorbar(data = tmp_inter, aes(x = SampleDate2, ymin=meanLinearFlux-sdLinearFlux, ymax=meanLinearFlux+sdLinearFlux), width=5, colour="black", alpha=0.6)
# 
# p1
# 
# 
# 
# 
# 
# 
# 
