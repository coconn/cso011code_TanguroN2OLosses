# Pit-Figures.R
# taking trace gas vial data associated with soil pits and making a tidy data set
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in GC-Rcode-fileloop.R

# output products:
# pitgasfull.csv : master csv of soil pit gas sampling


########################################################################
# BRING IN DATA / PREP

pitgasfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Pit-Data-Rprocessed/pitgasfull.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Pit-Data-Analyses/PitGasFigures/"

# create a col to assign a color in the graphs
pitgasfull <- transform(pitgasfull, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
pitgasfull <- transform(pitgasfull, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))

# create a col to assign season
pitgasfull <- transform(pitgasfull, Month = month(pitgasfull$SampleDate, label=TRUE))


########################################################################
# SIMPLE SCATTER PLOT

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE
summarytab1 <- summarySE(data=pitgasfull, measurevar="N2Oppm", c("pitID", "sampledepth", "SampleDate", "LUtype", "LUname", "color.use", "Month"), na.rm=TRUE, renameallcols=TRUE)
summarytab2 <- summarySE(data=pitgasfull, measurevar="CO2ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)
summarytab3 <- summarySE(data=pitgasfull, measurevar="CH4ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)

# join
pitgassummary <- join(x = summarytab1, y = summarytab2, by = c("pitID", "sampledepth", "SampleDate", "N"))
pitgassummary <- join(x = jointmp, y = summarytab3, by = c("pitID", "sampledepth", "SampleDate", "N"))





########################################################################
# SIMPLE SCATTERPLOT

ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line() + coord_flip() + scale_x_reverse() # figure out how to parse this by season and by pit (facet between seasons?)


qplot(depths, values, geom="line", group=sites) + coord_flip()




scatter1a <- ggplot(subset(fluxesfullmerge,GasType=="N2O"), aes(x=SoilMoisPercent, y=LinearFlux, color=color.use)) + geom_point(shape=1) + theme(legend.position="none") + ylab("LinearFlux N2O") + xlab("SoilMoisPercent") + geom_smooth(method=lm,   # Add linear regression lines
                                                                                                                                                                                                                                          se=TRUE,    # Do or don't add shaded confidence region
                                                                                                                                                                                                                                          fullrange=T) # Extend regression lines beyond the domain of the data




n2olin <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
      geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
      geom_point(size=2) + xlab("Sampling Date") + ylab("LinearFlux N2O") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# faceting
n2olinfacet <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + 
      geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) +
      geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("LinearFlux N2O") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))
# loess smooth included on facets
n2olinfacetloess <- n2olinfacet + geom_smooth(size = 1.5, fill="#333333", colour="black") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))









########################################################################
# SAVE CSV

# save as csv
write.csv(pitgassummary, file=paste(pathsavefiles, "pitgassummary.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING




