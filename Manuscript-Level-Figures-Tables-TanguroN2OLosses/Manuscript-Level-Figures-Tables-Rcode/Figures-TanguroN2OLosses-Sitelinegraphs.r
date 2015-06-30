# Figures-TanguroN2OLosses-Sitelinegraphs.R
# 
# manuscript ready figure, lines graphs for each site
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# UNITS LABELING: 1 microgram (ug) = 1000 nanograms (ng), so micrograms are 1000 times bigger.  CO2 fluxes are in migrograms/cm2/h, N2O are in nanograms/cm2/h.


### list of figures to make
# publication applicable fluxes over time (for each site, I reckon)
# include fertilization events in these
# row-interrow bar graphs (whole period vs. only in the post-fert windows)


########################################################################
# BRING IN DATA / PREP

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/"

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)

# bring in fluxes full csv
fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)
fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# bring in site-date combination summary data (combines info from multiple chambers)
sitedatesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterSiteDateSummary.csv", stringsAsFactors=FALSE)
sitedatesummary$SampleDate2 <- as.Date(sitedatesummary$SampleDate2)

# get rid of random NA rows
sitedatesummary <- subset(sitedatesummary, sitedatesummary$easysitename!="NA")
fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")


########################################################################
# LINE GRAPH, FLUXES VS. TIME BY LAND USE

# easysitename is the column that has the site-date code, sitedatesummary is the table to use

# if you want to put the sample date xlabels below the N2O and CO2 plots
# + (legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)

## N2O
ylabel <- "Flux N2O: ngN / cm2 / h"
p1 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, colour=color.use)) + geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) + geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab(ylabel) + theme_bw() + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

## CO2
ylabel <- "Flux CO2: ugC / cm2 / h"
p2 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, colour=color.use)) + geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) + geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab(ylabel) + theme_bw() + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x = element_blank()) + scale_x_date(labels = date_format("%m-%Y"))

## CH4
ylabel <- "Flux CH4: ugC / cm2 / h"
p3 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, colour=color.use)) + geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) + geom_point(size=2) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab(ylabel) + theme_bw() + theme(legend.position="none", axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_x_date(labels = date_format("%m-%Y"))



########################################################################
# FLUX LAND USE PLOT INTO GRID ARRANGE AND SAVE

## make sizing even; left align plots
# see: http://stackoverflow.com/questions/13656642/r-align-plot-areas-in-ggplot, AmazonTOs_table_bargraph_v3.r

# grid.arrange is wonky since the left hand side alignment is off
# grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)

# Get the widths
gA <- ggplot_gtable(ggplot_build(p1))
gB <- ggplot_gtable(ggplot_build(p2))
gC <- ggplot_gtable(ggplot_build(p3))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3])
maxHeight = unit.pmax(gA$height[2:3], gB$height[2:3], gC$height[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth

# Set the heights
gA$height[2:3] <- maxHeight
gB$height[2:3] <- maxHeight
gC$height[2:3] <- maxHeight # how to make this larger so the x-axis doesn't squish the CH4 graph?

# get legend (how to do this without it messing up the height of the three graph rows?)
#g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
#legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#lheight <- sum(legend$height)
#legend$height[2:3] <- lheight

# grid.arrange and save
png(file = paste(pathsavefigures, "fluxes_LUtype.png", sep=""),width=10,height=10,units="in",res=400)
grid.arrange(gA, gB, gC, nrow = 3, ncol = 1)
dev.off()

# note that grid_arrange_shared_legend won't work here because it doesn't like the gA objects


########################################################################
# LINE GRAPH, N2O FLUXES VS. TIME AT EACH SITE

# get rid of random F1 point that has an NA flux value
sitedatesummary <- subset(sitedatesummary, sitedatesummary$meanfluxN2Ol!="NA")

# N2O flux pattern for each site
p1 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxN2Ol, color=color.use)) + geom_errorbar(aes(ymin=meanfluxN2Ol-sdfluxN2Ol, ymax=meanfluxN2Ol+sdfluxN2Ol), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux N2O: ngN / cm2 / h") + geom_line() + theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))

# save
png(file = paste(pathsavefigures, "sitetrackingN2O.png", sep=""),width=5,height=5,units="in",res=400)
p1
dev.off()

# put the CO2 and CH4 figures in the supplement

# CO2 flux pattern for each site
p1 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCO2l, color=color.use)) + geom_errorbar(aes(ymin=meanfluxCO2l-sdfluxCO2l, ymax=meanfluxCO2l+sdfluxCO2l), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux CO2: ugC / cm2 / h") + geom_line() + theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# save
png(file = paste(pathsavefigures, "sitetrackingCO2.png", sep=""),width=5,height=5,units="in",res=400)
p1
dev.off()

# CH4 flux pattern for each site
p1 <- ggplot(sitedatesummary, aes(x=SampleDate2, y=meanfluxCH4l, color=color.use)) + geom_errorbar(aes(ymin=meanfluxCH4l-sdfluxCH4l, ymax=meanfluxCH4l+sdfluxCH4l), width=5) + geom_point(size=1) + facet_wrap( ~ Site, ncol=3) + xlab("Sampling Date") + ylab("Flux CH4: ugC / cm2 / h") + geom_line() + theme_bw() + theme(legend.position="none",axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# save
png(file = paste(pathsavefigures, "sitetrackingCH4.png", sep=""),width=5,height=5,units="in",res=400)
p1
dev.off()

# add in vertical lines when there was a fertilization event




########################################################################
# NOTES AND TESTING






