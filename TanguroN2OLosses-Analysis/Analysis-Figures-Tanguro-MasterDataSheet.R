# Analysis-Figures-Tanguro-MasterDataSheet.R
# master .R doc for making general analysis figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# things that are done in this file (chunks are folded to make it readable):
# panel fluxes and abiotic factors by time scatter plots - every chamber
# panel fluxes by abiotic factors scatter plots - every chamber

# TO DO
# panel fluxes by time - each site (incl. row and interrow)



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

# eventually add the soil variables
# move this code to tanguro-masterdatasheet.r and save as csv?
# include N, standard error and CI as per http://www.cookbook-r.com/Manipulating_data/Summarizing_data/


########################################################################
# ABIOTIC FACTOR AND FLUXES VS. TIME PLOTS (each chamber)
# gets saved as fluxes-factors-by-time-Nchambers.png

{
# soil temp
soiltmp <- ggplot(fluxesfullmerge, aes(x=SampleDate2, y=SoilTmpEnd, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("SoilTmpEnd")
soiltmploess <- soiltmp + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

# soil moisture
soilmois <- ggplot(fluxesfullmerge, aes(x=SampleDate2, y=SoilMoisPercent, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("SoilMoisPercent")
soilmoisloess <- soilmois + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

# N2O linear
no2lin <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux N2O")
no2linloess <- no2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

# CO2 Linear
co2lin <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux CO2")
co2linloess <- co2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))

# CH4 Linear
ch4lin <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux CH4")
ch4linloess <- ch4lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))

}

# where to save figure
{
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
# grid.arrange and save
png(file = paste(pathsavefigures, "fluxes-factors-by-time-Nchambers.png", sep=""),width=10,height=18,units="in",res=400)
grid.arrange(soiltmploess, soilmoisloess, no2linloess, co2linloess, ch4linloess, nrow = 5, ncol = 1)
dev.off()
}

# alternate commented out code for a similar figure
# grid arrange going by col not row
{
# # soil temp
# soiltmp <- ggplot(fluxesfullmerge, aes(x=SampleDate2, y=SoilTmpEnd, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("SoilTmpEnd")
# soiltmploess <- soiltmp + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
# 
# # soil moisture
# soilmois <- ggplot(fluxesfullmerge, aes(x=SampleDate2, y=SoilMoisPercent, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("SoilMoisPercent")
# soilmoisloess <- soilmois + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
# 
# # N2O linear
# no2lin <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("LinearFlux N2O")
# no2linloess <- no2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
# 
# # CO2 Linear
# co2lin <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("LinearFlux CO2")
# co2linloess <- co2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
# 
# # grid.arrange
# grid.arrange(soiltmploess, soilmoisloess, no2linloess, co2linloess, nrow = 1, ncol = 4)
# 
}

# alternate commented out code for a similar figure
# subset out wet season when calling ggplot
{
# # quad - facet by site
# # wet season only
# soilmoisfig1 <- ggplot(subset(fluxesfullmerge, SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=SoilMoisPercent, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("% Soil Moisture")
# soilmoisfig1loess <- soilmoisfig1 + geom_smooth(size = 1.5, fill="#333333", colour="black")
}



########################################################################
# ABIOTIC FACTOR AND FLUXES VS. TIME PLOTS (site mean and std)
# gets saved as fluxes-factors-by-time-Nsitedate.png

# easysitename is the column that has the site-date code, sitedatesummary is the table to use

{
# soil moisture
# no faceting
soilmois <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) +
  geom_point(size=2) + xlab("Sampling Date") + ylab("Soil Moisture Percent") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# faceting
soilmoisfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) +
  geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Soil Moisture Percent") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# loess smooth included on facets
soilmoisfacetloess <- soilmoisfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

# n2o fluxes
# no faceting
n2olin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
  geom_point(size=2) + xlab("Sampling Date") + ylab("LinearFlux N2O") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# faceting
n2olinfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
  geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux N2O") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# loess smooth included on facets
n2olinfacetloess <- n2olinfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

# co2 fluxes
# no faceting
co2lin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) +
  geom_point(size=2) + xlab("Sampling Date") + ylab("LinearFlux CO2") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# faceting
co2linfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, colour=color.use)) + 
  geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) +
  geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux CO2") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
# loess smooth included on facets
co2linfacetloess <- co2linfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))

# ch4 fluxes
# no faceting
ch4lin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, colour=color.use)) + 
      geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) +
      geom_point(size=2) + xlab("Sampling Date") + ylab("LinearFlux CH4") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# faceting
ch4linfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, colour=color.use)) + 
      geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) +
      geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux CH4") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
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


########################################################################
# ABIOTIC FACTOR VS. FLUX SCATTER PLOTS - NOTHING FANCY

# a bunch of flux by factor scatter plots
{
scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux N2O") + xlab("SoilMoisPercent") + geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Do or don't add shaded confidence region
              fullrange=T) # Extend regression lines beyond the domain of the data

scatter1b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux CO2") + xlab("SoilMoisPercent") + geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Do or don't add shaded confidence region
              fullrange=T) # Extend regression lines beyond the domain of the data

scatter1c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux CH4") + xlab("SoilMoisPercent") + geom_smooth(method=lm,   # Add linear regression lines                                                                                                                                                                                                                                         
      se=TRUE,    # Do or don't add shaded confidence region                                                                                
      fullrange=T) # Extend regression lines beyond the domain of the data

scatter2a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux N2O") + xlab("Soil Tmp") + geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Do or don't add shaded confidence region
              fullrange=T) # Extend regression lines beyond the domain of the data

scatter2b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux CO2") + xlab("Soil Tmp") + geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Do or don't add shaded confidence region
              fullrange=T) # Extend regression lines beyond the domain of the data

scatter2c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux CH4") + xlab("Soil Tmp") + geom_smooth(method=lm,   # Add linear regression lines
            se=TRUE,    # Do or don't add shaded confidence region
            fullrange=T) # Extend regression lines beyond the domain of the data
}

# where to save figure
{
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
# grid.arrange and save
png(file = paste(pathsavefigures, "flux-factors-scatterplots.png", sep=""),width=15,height=10,units="in",res=400)
grid.arrange(scatter1a, scatter1b, scatter1c, scatter2a, scatter2b, scatter2c, nrow = 2, ncol = 3)
dev.off()
}


########################################################################
# SITE PATTERNS

##### up next - include row and interrow as a different shape on these

# N2O flux pattern for each site
{
# ggplot
sitetrackingN2O <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, color=color.use)) + geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux N2O") + geom_line() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
# save figure
ggsave(file=paste(pathsavefigures, "sitetrackingN2O.png", sep=""),width=5,height=5,units="in",dpi=400)
}

# CO2 flux pattern for each site
{
# ggplot
sitetrackingCO2 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, color=color.use)) + geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux CO2") + geom_line() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
# save figure
ggsave(file=paste(pathsavefigures, "sitetrackingCO2.png", sep=""),width=5,height=5,units="in",dpi=400)
}

# Moisture patterns for each site
{
# ggplot
sitetrackingmois <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, color=color.use)) + geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Soil Moisture Percent") + geom_line() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
# save figure
ggsave(file=paste(pathsavefigures, "sitetrackingmois.png", sep=""),width=5,height=5,units="in",dpi=400)
}




########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4





