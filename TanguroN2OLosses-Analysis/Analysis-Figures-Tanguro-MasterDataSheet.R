# Analysis-Figures-Tanguro-MasterDataSheet.R
# master .R doc for making general analysis figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# things that are done in this file (chunks are folded to make it readable):
# panel fluxes and abiotic factors by time scatter plots - every chamber
# panel fluxes by abiotic factors scatter plots - every chamber

# TO DO
# panel fluxes by time - each site (incl. row and interrow)

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

# move this code to tanguro-masterdatasheet.r and save as csv?
# include standard error and CI as per http://www.cookbook-r.com/Manipulating_data/Summarizing_data/


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
      no2lin <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h")
      no2linloess <- no2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # CO2 Linear
      co2lin <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h")
      co2linloess <- co2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
      
      # CH4 Linear
      ch4lin <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("Flux CH4: ugC / cm2 / h")
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
      # no2lin <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h")
      # no2linloess <- no2lin + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))
      # 
      # # CO2 Linear
      # co2lin <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SampleDate2, y=LinearFlux, color=color.use, group=1)) + geom_point(size=2.5) + facet_wrap( ~ LUname, nrow=3) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h")
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
# ABIOTIC FACTOR AND FLUXES VS. TIME PLOTS (site mean and std) NO SOIL VARIABLES
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
# ABIOTIC FACTOR AND FLUXES VS. TIME PLOTS (site mean and std) YES SOIL N VARIABLES
# gets saved as fluxes-factors-by-time-sitedate-soilvars-facet-noloess.png

# easysitename is the column that has the site-date code, sitedatesummary is the table to use





{
      # meanNO3_N_mgNg
      # no faceting
      NO3_N <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg-sdNO3_N_mgNg, ymax=meanNO3_N_mgNg+sdNO3_N_mgNg), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("NO3-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      NO3_Nfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg-sdNO3_N_mgNg, ymax=meanNO3_N_mgNg+sdNO3_N_mgNg), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("NO3-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      NO3_Nfacetloess <- NO3_Nfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNH4_N_mgNg
      # no faceting
      NH4_N <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg-sdNH4_N_mgNg, ymax=meanNH4_N_mgNg+sdNH4_N_mgNg), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("NH4-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      NH4_Nfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg-sdNH4_N_mgNg, ymax=meanNH4_N_mgNg+sdNH4_N_mgNg), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("NH4-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      NH4_Nfacetloess <- NH4_Nfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNO3_N_NH4_N
      # no faceting
      NO3_N_NH4_N <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg-sdNO3_N_NH4_N_mgNg, ymax=meanNO3_N_NH4_N_mgNg+sdNO3_N_NH4_N_mgNg), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("NO3-N + NH4-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      NO3_N_NH4_Nfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg-sdNO3_N_NH4_N_mgNg, ymax=meanNO3_N_NH4_N_mgNg+sdNO3_N_NH4_N_mgNg), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("NO3-N + NH4-N mgN/g") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      NO3_N_NH4_Nfacetloess <- NO3_N_NH4_Nfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNO3_N_mgNg_FinalMinusInitial_perDay
      # no faceting
      meanNO3_N_mgNg_FinalMinusInitial_perDay <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg_FinalMinusInitial_perDay-sdNO3_N_mgNg_FinalMinusInitial_perDay, ymax=meanNO3_N_mgNg_FinalMinusInitial_perDay+sdNO3_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net nitrification rate, mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNO3_N_mgNg_FinalMinusInitial_perDayfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg_FinalMinusInitial_perDay-sdNO3_N_mgNg_FinalMinusInitial_perDay, ymax=meanNO3_N_mgNg_FinalMinusInitial_perDay+sdNO3_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net nitrification rate (NO3), mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNO3_N_mgNg_FinalMinusInitial_perDayfacetloess <- meanNO3_N_mgNg_FinalMinusInitial_perDayfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNH4_N_mgNg_FinalMinusInitial_perDay
      # no faceting
      meanNH4_N_mgNg_FinalMinusInitial_perDay <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg_FinalMinusInitial_perDay-sdNH4_N_mgNg_FinalMinusInitial_perDay, ymax=meanNH4_N_mgNg_FinalMinusInitial_perDay+sdNH4_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net ammonification rate (NH4), mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNH4_N_mgNg_FinalMinusInitial_perDayfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg_FinalMinusInitial_perDay-sdNH4_N_mgNg_FinalMinusInitial_perDay, ymax=meanNH4_N_mgNg_FinalMinusInitial_perDay+sdNH4_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net ammonification rate (NH4), mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNH4_N_mgNg_FinalMinusInitial_perDayfacetloess <- meanNH4_N_mgNg_FinalMinusInitial_perDayfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay
      # no faceting
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay-sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, ymax=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay+sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net mineralization rate (NO3 + NH4), mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDayfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay-sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, ymax=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay+sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net mineralization rate (NO3 + NH4), mg N g soil-1 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDayfacetloess <- NO3_N_NH4_Nfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis
      # no faceting
      meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net nitrificationrate area basis, mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net nitrificationrate area basis (NO3), mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacetloess <- meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis
      # no faceting
      meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net ammonificationrate area basis (NH4), mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net ammonificationrate area basis (NH4), mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacetloess <- meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
      # meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis
      # no faceting
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + xlab("Sampling Date") + ylab("net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # faceting
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, colour=color.use)) + 
            geom_errorbar(aes(ymin=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis-sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, ymax=meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis+sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis), width=5) +
            geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      # loess smooth included on facets
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacetloess <- NO3_N_NH4_Nfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
      
}


# where to save figure
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-soilvars-facet-noloess1.png", sep=""),width=12,height=15,units="in",res=400)
      grid.arrange(n2olinfacet, 
                   co2linfacet, 
                   ch4linfacet, 
                   nrow = 3, ncol = 1)
      dev.off()
      
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-soilvars-facet-noloess2.png", sep=""),width=12,height=15,units="in",res=400)
      grid.arrange(NO3_Nfacet, 
                   NH4_Nfacet, 
                   NO3_N_NH4_Nfacet, 
                   nrow = 3, ncol = 1)
      dev.off()
      
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-soilvars-facet-noloess3.png", sep=""),width=12,height=15,units="in",res=400)
      grid.arrange(meanNO3_N_mgNg_FinalMinusInitial_perDayfacet, 
                   meanNH4_N_mgNg_FinalMinusInitial_perDayfacet, 
                   meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDayfacet, 
                   nrow = 3, ncol = 1)
      dev.off()
      
      png(file = paste(pathsavefigures, "fluxes-factors-by-time-sitedate-soilvars-facet-noloess4.png", sep=""),width=12,height=15,units="in",res=400)
      grid.arrange(meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet, 
                   meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet, 
                   meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasisfacet, 
                   nrow = 3, ncol = 1)
      dev.off()
}




########################################################################
# ABIOTIC FACTOR VS. FLUX SCATTER PLOTS - NOTHING FANCY

# a bunch of flux by factor scatter plots
{
      scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("% Soil Moisture") + geom_smooth(method=lm,   # Add linear regression lines
                                                                                                                                                                                                                                                         se=TRUE,    # Do or don't add shaded confidence region
                                                                                                                                                                                                                                                         fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ugC / cm2 / h") + xlab("% Soil Moisture") + geom_smooth(method=lm,   # Add linear regression lines
                                                                                                                                                                                                                                                         se=TRUE,    # Do or don't add shaded confidence region
                                                                                                                                                                                                                                                         fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ugC / cm2 / h") + xlab("% Soil Moisture") + geom_smooth(method=lm,   # Add linear regression lines                                                                                                                                                                                                                                         
                                                                                                                                                                                                                                                         se=TRUE,    # Do or don't add shaded confidence region                                                                                
                                                                                                                                                                                                                                                         fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("Soil Tmp C") + geom_smooth(method=lm,   # Add linear regression lines
                                                                                                                                                                                                                                               se=TRUE,    # Do or don't add shaded confidence region
                                                                                                                                                                                                                                               fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ugC / cm2 / h") + xlab("Soil Tmp C") + geom_smooth(method=lm,   # Add linear regression lines
                                                                                                                                                                                                                                               se=TRUE,    # Do or don't add shaded confidence region
                                                                                                                                                                                                                                               fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=SoilTmpEnd, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ugC / cm2 / h") + xlab("Soil Tmp C") + geom_smooth(method=lm,   # Add linear regression lines
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
# ABIOTIC FACTOR VS. FLUX SCATTER PLOTS - SOIL N VARS

# a bunch of flux by factor scatter plots
{
      scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1b <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1c <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2b <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2c <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3b <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3c <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux N2O: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      

# where to save figure
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "flux-factors-scatterplots-soilvars1.png", sep=""),width=15,height=10,units="in",res=400)
      grid.arrange(scatter1a, scatter1b, scatter1c, scatter2a, scatter2b, scatter2c, scatter3a, scatter3b, scatter3c, nrow = 3, ncol = 3)
      dev.off()
}






# a bunch of flux by factor scatter plots
{
      scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1c <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2a <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2c <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3a <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3b <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3c <- ggplot(subset(fluxesfullmerge,GasType=="CO2"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CO2: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      
      # where to save figure
{
            pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
            # grid.arrange and save
            png(file = paste(pathsavefigures, "flux-factors-scatterplots-soilvars2.png", sep=""),width=15,height=10,units="in",res=400)
            grid.arrange(scatter1a, scatter1b, scatter1c, scatter2a, scatter2b, scatter2c, scatter3a, scatter3b, scatter3c, nrow = 3, ncol = 3)
            dev.off()
      }







# a bunch of flux by factor scatter plots
{
      scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1b <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter1c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_NH4_N_mgNg, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2a <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2b <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter2c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3a <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3b <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      scatter3c <- ggplot(subset(fluxesfullmerge,GasType=="CH4"), aes(x=NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("Flux CH4: ngN / cm2 / h") + xlab("NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis") + geom_smooth(method=lm, se=TRUE, fullrange=T) # Extend regression lines beyond the domain of the data
      
      
      # where to save figure
{
            pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
            # grid.arrange and save
            png(file = paste(pathsavefigures, "flux-factors-scatterplots-soilvars3.png", sep=""),width=15,height=10,units="in",res=400)
            grid.arrange(scatter1a, scatter1b, scatter1c, scatter2a, scatter2b, scatter2c, scatter3a, scatter3b, scatter3c, nrow = 3, ncol = 3)
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

# Moisture patterns for each site
{
      # ggplot
      sitetrackingmois <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanSoilMoisPercent, color=color.use)) + geom_errorbar(aes(ymin=meanSoilMoisPercent-sdSoilMoisPercent, ymax=meanSoilMoisPercent+sdSoilMoisPercent), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("% Soil Moisture") + geom_line() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1))
      # where to save figure
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # save figure
      ggsave(file=paste(pathsavefigures, "sitetrackingmois.png", sep=""),width=5,height=5,units="in",dpi=400)
}



########################################################################
# ROW VS. INTERROW PATTERNS WITH SOIL MOISTURE

# see "ABIOTIC FACTOR VS. FLUX SCATTER PLOTS - NOTHING FANCY" section above for similar plots

# need this otherwise because forest isn't represented here, the colors are all off by one
factorcolors <- c("#619CFF","#F8766D") # c("blue","orange")

scatter1aRI <- ggplot(subset(fluxesfullmerge,GasType=="N2O" & (RowInter=="R" | RowInter=="I")), aes(x=SoilMoisPercent, y=LinearFlux, color=LUtype)) + geom_point(shape=1) + facet_wrap( ~ RowInter, ncol=2) + ylab("Flux N2O: ngN / cm2 / h") + scale_colour_manual(values = factorcolors) + xlab("% Soil Moisture") + geom_smooth(method=lm, se=TRUE, fullrange=T) + theme(legend.position="none") # Extend regression lines beyond the domain of the data

scatter1bRI <- ggplot(subset(fluxesfullmerge,GasType=="CO2" & (RowInter=="R" | RowInter=="I")), aes(x=SoilMoisPercent, y=LinearFlux, color=LUtype)) + geom_point(shape=1) + facet_wrap( ~ RowInter, ncol=2) + ylab("Flux CO2: ugC / cm2 / h") + scale_colour_manual(values = factorcolors) + xlab("% Soil Moisture") + geom_smooth(method=lm, se=TRUE, fullrange=T) + theme(legend.position="none") # Extend regression lines beyond the domain of the data

scatter1cRI <- ggplot(subset(fluxesfullmerge,GasType=="CH4" & (RowInter=="R" | RowInter=="I")), aes(x=SoilMoisPercent, y=LinearFlux, color=LUtype)) + geom_point(shape=1) + facet_wrap( ~ RowInter, ncol=2) + ylab("Flux CH4: ugC / cm2 / h") + scale_colour_manual(values = factorcolors) + xlab("% Soil Moisture") + geom_smooth(method=lm, se=TRUE, fullrange=T) + theme(legend.position="none") # Extend regression lines beyond the domain of the data


# where to save figure
{
      pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/"
      # grid.arrange and save
      png(file = paste(pathsavefigures, "flux-rowinter-factors-scatterplots.png", sep=""),width=20,height=6,units="in",res=400)
      grid.arrange(scatter1aRI, scatter1bRI, scatter1cRI, nrow = 1, ncol = 3)
      dev.off()
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

###### don't forget to do all of this for CH4





