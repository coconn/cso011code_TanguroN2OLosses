# Figures-TanguroN2OLosses-RowInterBar.R
# 
# manuscript ready figure, bar graphs comparing row and inter-row properties
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# UNITS LABELING: 1 microgram (ug) = 1000 nanograms (ng), so micrograms are 1000 times bigger.  CO2 fluxes are in migrograms/cm2/h, N2O are in nanograms/cm2/h.



### list of figures to make
# publication applicable fluxes over time (for each site, I reckon)
# publication applicable soil inorg N and moisture over time (for each site, I reckon)
# include fertilization events in these
# row-interrow bar graphs (whole period vs. only in the post-fert windows)
# correlaion table of log-transformed variables (simple scatterplots)



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
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - NO DATES OR SITE

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

rowintersummary <- summarySE(data=fluxesfullmerge, measurevar="LinearFlux", groupvars=c("GasType","LUtype","RowInter"), na.rm=TRUE)

# get rid of table rows that aren't row or interrow or forest (e.g., NA or blank)
pattern <- "R|I|F"
tmp <- grep(pattern, rowintersummary$RowInter)
rowintersummary <- rowintersummary[tmp,]
# weird row where gas == NA
ok <- complete.cases(rowintersummary$GasType)
rowintersummary <- rowintersummary[ok,]

# bar graphs
factorcolors <- c("#66a61e","#1f78b4","#d95f02") # colorbrewer qualitative

barplotaRI <- ggplot(data=subset(rowintersummary,GasType=="N2O"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="N2O")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux N2O: ngN / cm2 / h") + xlab("Land Use Type") + theme_bw() + theme(legend.position="none")

barplotbRI <- ggplot(data=subset(rowintersummary,GasType=="CO2"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CO2")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CO2: ugC / cm2 / h") + xlab("Land Use Type") + theme_bw() + theme(legend.position="none")

barplotcRI <- ggplot(data=subset(rowintersummary,GasType=="CH4"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymin=LinearFlux-sd, ymax=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CH4")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CH4: ugC / cm2 / h") + xlab("Land Use Type") + theme_bw() 

# grid.arrange and save
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
png(file = paste(pathsavefigures, "flux-rowinter-barplots.png", sep=""),width=7,height=10,units="in",res=400)
grid_arrange_shared_legend(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)
dev.off()

#grid.arrange(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)



########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS - INCLUDE POST FERTILIZATION TIME CATEGORY

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

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

# plot this
barplotaRI <- ggplot(data=subset(rowintersummary2,GasType=="N2O"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="N2O")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux N2O: ngN / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

barplotbRI <- ggplot(data=subset(rowintersummary2,GasType=="CO2"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="CO2")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux CO2: ugC / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

barplotcRI <- ggplot(data=subset(rowintersummary2,GasType=="CH4"), aes(x=factor(postfertcat), y=LinearFlux, fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux-sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary2,GasType=="CH4")) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = factorcolors) + ylab("Flux CH4: ugC / cm2 / h") + theme_bw() + scale_x_discrete(breaks=c("forestpostfert", "notpostfert", "postfert", "soypostfert"), labels=c("F", "M not within \n 15 days of fert ", "M =< 15 \n days post fert", "S")) + theme(axis.title.x = element_blank())

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
# NOTES AND TESTING

# if you run ANOVAs, go back and put the values on the graphs if any are significant



########################################################################
# POSSIBLE TO DO

###### line graph style - too hard to read, so switch to bar graphs

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
