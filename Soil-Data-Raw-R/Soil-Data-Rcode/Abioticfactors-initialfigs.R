# Abioticfactors-initialfigs.R
# initial soil data to preliminary figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN DATA / PREP

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Combo-Datasets/fluxesfullmerge.csv", stringsAsFactors=FALSE)






########################################################################
# ABIOTIC FACTOR VS. TIME PLOTS - NOTHING FANCY

require(ggplot2)
require(gridExtra)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

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

# grid.arrange
grid.arrange(soiltmploess, soilmoisloess, no2linloess, co2linloess, nrow = 4, ncol = 1)


# grid arrange going by col not row

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



########################################################################
# ABIOTIC FACTOR VS. FLUX SCATTER PLOTS - NOTHING FANCY








########################################################################
# SITE PATTERNS - FERT EFFECT CAPTURED?




########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




