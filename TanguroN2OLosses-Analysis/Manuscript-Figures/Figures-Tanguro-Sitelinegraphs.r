# Figures-Tanguro-Sitelinegraphs.R
# 
# manuscript ready figure, lines graphs for each site
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


# prep table that summarizes by site and date (combines info from multiple chambers)

# data.table
dtfluxes <- data.table(fluxesfullmerge)
# create site-date summary table
sitedatesummary <- dtfluxes[,list(
      SampleDate2=unique(SampleDate2), 
      Site=unique(Site), 
      color.use=unique(color.use), 
      LUname=unique(LUname),
      meanfluxN2Ol=mean(na.omit(LinearFlux[GasType=="N2O"])),
      meanfluxCO2l=mean(na.omit(LinearFlux[GasType=="CO2"])),
      meanfluxCH4l=mean(na.omit(LinearFlux[GasType=="CH4"])),
      sdfluxN2Ol=sd(na.omit(LinearFlux[GasType=="N2O"])),
      sdfluxCO2l=sd(na.omit(LinearFlux[GasType=="CO2"])),  
      sdfluxCH4l=sd(na.omit(LinearFlux[GasType=="CH4"])),  
      meanfluxN2Oq=mean(na.omit(QuadFlux[GasType=="N2O"])),
      meanfluxCO2q=mean(na.omit(QuadFlux[GasType=="CO2"])),
      meanfluxCH4q=mean(na.omit(QuadFlux[GasType=="CH4"])),
      sdfluxN2Oq=sd(na.omit(QuadFlux[GasType=="N2O"])),
      sdfluxCO2q=sd(na.omit(QuadFlux[GasType=="CO2"])),
      sdfluxCH4q=sd(na.omit(QuadFlux[GasType=="CH4"])),
      meanSoilMoisPercent=mean(na.omit(SoilMoisPercent)),
      sdSoilMoisPercent=sd(na.omit(SoilMoisPercent))),
      by=easysitename] # include as many variables as desired

# create site-date summary table with row-interrow option
sitedatesummaryRI <- dtfluxes[,list(
      SampleDate2=unique(SampleDate2), 
      Site=unique(Site), 
      color.use=unique(color.use), 
      LUname=unique(LUname),
      meanfluxN2Ol=mean(na.omit(LinearFlux[GasType=="N2O"])),
      meanfluxCO2l=mean(na.omit(LinearFlux[GasType=="CO2"])),
      meanfluxCH4l=mean(na.omit(LinearFlux[GasType=="CH4"])),
      sdfluxN2Ol=sd(na.omit(LinearFlux[GasType=="N2O"])),
      sdfluxCO2l=sd(na.omit(LinearFlux[GasType=="CO2"])),  
      sdfluxCH4l=sd(na.omit(LinearFlux[GasType=="CH4"])),  
      meanfluxN2OlR=mean(na.omit(LinearFlux[GasType=="N2O" & RowInter=="R"])),
      meanfluxCO2lR=mean(na.omit(LinearFlux[GasType=="CO2" & RowInter=="R"])),
      meanfluxCH4lR=mean(na.omit(LinearFlux[GasType=="CH4" & RowInter=="R"])),
      sdfluxN2OlR=sd(na.omit(LinearFlux[GasType=="N2O" & RowInter=="R"])),
      sdfluxCO2lR=sd(na.omit(LinearFlux[GasType=="CO2" & RowInter=="R"])),  
      sdfluxCH4lR=sd(na.omit(LinearFlux[GasType=="CH4" & RowInter=="R"])),  
      meanfluxN2OlI=mean(na.omit(LinearFlux[GasType=="N2O" & RowInter=="I"])),
      meanfluxCO2lI=mean(na.omit(LinearFlux[GasType=="CO2" & RowInter=="I"])),
      meanfluxCH4lI=mean(na.omit(LinearFlux[GasType=="CH4" & RowInter=="I"])),
      sdfluxN2OlI=sd(na.omit(LinearFlux[GasType=="N2O" & RowInter=="I"])),
      sdfluxCO2lI=sd(na.omit(LinearFlux[GasType=="CO2" & RowInter=="I"])),  
      sdfluxCH4lI=sd(na.omit(LinearFlux[GasType=="CH4" & RowInter=="I"]))),  
      by=easysitename] # include as many variables as desired


# move this code to tanguro-masterdatasheet.r and save as csv?
# include N, standard error and CI as per http://www.cookbook-r.com/Manipulating_data/Summarizing_data/


########################################################################
# MANUSCRIPT FLUX LINE GRAPHS BY SITE
# 

ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, color=color.use)) + geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + geom_line() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))

# this works, but wrong colors
ggplot(sitedatesummaryRI, aes(x=SampleDate2, y = value, color = variable)) + 
      geom_point(aes(y = meanfluxN2Ol, col = "mean all")) + geom_line(aes(y = meanfluxN2Ol, col = "mean all")) +
      geom_point(aes(y = meanfluxN2OlR, col = "mean row")) + geom_line(aes(y = meanfluxN2OlR, col = "mean row")) +
      geom_point(aes(y = meanfluxN2OlI, col = "mean interrow")) + geom_line(aes(y = meanfluxN2OlI, col = "mean interrow")) + 
      facet_wrap( ~ Site, ncol=3) + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
      xlab("Sampling Date") + 
      ylab("Flux N2O: ngN / cm2 / h") 
### i think what i need to do is make a new dataframe where one column is "all, row or interrow" and then the other cols are "mean" and "sd" - try a reshape::melt option



# example
x <- seq(0, 4 * pi, 0.1)
n <- length(x)
y1 <- 0.5 * runif(n) + sin(x)
y2 <- 0.5 * runif(n) + cos(x) - sin(x)
df <- data.frame(x, y1, y2)
# option 1
ggplot(df, aes(x, y = value, color = variable)) + 
      geom_point(aes(y = y1, col = "y1")) + 
      geom_point(aes(y = y2, col = "y2"))
# option 2
library(reshape)
# This creates a new data frame with columns x, variable and value
# x is the id, variable holds each of our timeseries designation
df.melted <- melt(df, id = "x")
ggplot(data = df.melted, aes(x = x, y = value, color = variable)) +
      geom_point()





########################################################################
# ABIOTIC FACTOR AND FLUXES VS. TIME PLOTS (site mean and std)
# gets saved as fluxes-factors-by-time-Nsitedate.png

# easysitename is the column that has the site-date code, sitedatesummary is the table to use

{
      # soil moisture
      # no faceting
      soilmois <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("% Soil Moisture") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      soilmoisfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("% Soil Moisture") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      soilmoisfacetloess <- soilmoisfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # n2o fluxes
      # no faceting
      n2olin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      n2olinfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      n2olinfacetloess <- n2olinfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # co2 fluxes
      # no faceting
      co2lin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      co2linfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      co2linfacetloess <- co2linfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
      
      # ch4 fluxes
      # no faceting
      ch4lin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("Flux CH4: ugC / cm2 / h") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      ch4linfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux CH4: ugC / cm2 / h") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      ch4linfacetloess <- ch4linfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
}


# where to save figure
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-facet-noloess.png", sep=""),width=10,height=14,units="in",res=400)
      grid.arrange(soilmoisfacet, n2olinfacet, co2linfacet, ch4linfacet, nrow = 4, ncol = 1)
      dev.off()
}


# versions with shared legend
source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-facet-noloess_legend.png", sep=""),width=16,height=14,units="in",res=400)
      grid_arrange_shared_legend(soilmoisfacet, n2olinfacet, co2linfacet, ch4linfacet, nrow = 4, ncol = 1)
      dev.off()
}





########################################################################
# SITE PATTERNS

# N2O flux pattern for each site
{
      # ggplot
      sitetrackingN2O <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, color=color.use)) + geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + geom_line() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))
      # where to save figure
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # save figure
      ggsave(file=paste(pathsavefigures, "sitetrackingN2O.png", sep=""),width=5,height=5,units="in",dpi=400)
}

# CO2 flux pattern for each site
{
      # ggplot
      sitetrackingCO2 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, color=color.use)) + geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h") + geom_line() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1))
      # where to save figure
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # save figure
      ggsave(file=paste(pathsavefigures, "sitetrackingCO2.png", sep=""),width=5,height=5,units="in",dpi=400)
}

# CH4 flux pattern for each site
{
      # ggplot
      sitetrackingCH4 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, color=color.use)) + geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux CH4: ugC / cm2 / h") + geom_line() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1))
      # where to save figure
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # save figure
      ggsave(file=paste(pathsavefigures, "sitetrackingCH4.png", sep=""),width=5,height=5,units="in",dpi=400)
}




########################################################################
# ROW VS. INTERROW MEAN FLUX BAR GRAPHS

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

barplotaRI <- ggplot(data=subset(rowintersummary,GasType=="N2O"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="N2O")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux N2O: ngN / cm2 / h") + xlab("Land Use Type") + theme(legend.position="none")

barplotbRI <- ggplot(data=subset(rowintersummary,GasType=="CO2"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymax=LinearFlux+sd, ymin=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CO2")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CO2: ugC / cm2 / h") + xlab("Land Use Type") + theme(legend.position="none")

barplotcRI <- ggplot(data=subset(rowintersummary,GasType=="CH4"), aes(x=LUtype, y=LinearFlux, group=RowInter,fill=RowInter)) + geom_errorbar(aes(ymin=LinearFlux-sd, ymax=LinearFlux-0.1*LinearFlux), position=position_dodge(0.9),width=.25, data=subset(rowintersummary,GasType=="CH4")) + geom_bar(position='dodge', stat='identity') + scale_fill_manual(values = factorcolors) + ylab("Flux CH4: ugC / cm2 / h") + xlab("Land Use Type") 

# where to save figure
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "flux-rowinter-factors-barplots.png", sep=""),width=20,height=6,units="in",res=400)
      grid.arrange(barplotaRI, barplotbRI, barplotcRI, nrow = 1, ncol = 3)
      dev.off()
}


########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO






