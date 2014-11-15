# Soil-Data-Rcode.R
# processing soil data from the field
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN ABIOTIC DATA / PREP

# set files to process and path to folder with standardized naming info
abioticfactors <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/abioticfactors.txt", stringsAsFactors=FALSE, na.strings="NA")

# create a col to assign a color in the graphs
abioticfactors <- transform(abioticfactors, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
abioticfactors <- transform(abioticfactors, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))

# summary by site and date to df so R likes it
abioticfactors$SampleDate <- gsub("/14","/2014", abioticfactors$SampleDate , fixed=TRUE)
abioticfactors$SampleDate2 <- gsub("[.]","/",abioticfactors$SampleDate)
abioticfactors$SampleDate2 <- as.Date(abioticfactors$SampleDate2, format="%Y/%m/%d")

# add column for site-date combo name (to find site-date summary stats)
abioticfactors$easysitename <- do.call(paste, c(abioticfactors[c("Site", "SampleDate")], sep = "_")) 

# new name so can save as other file
abioticfactorsprocessed <- abioticfactors

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/"
write.csv(abioticfactorsprocessed, file=paste(pathsavefiles, "abioticfactorsprocessed.csv", sep = ""), row.names=FALSE)  



########################################################################
# MAKE HUGE DATASET WITH FLUXES AND ABIOTIC FACTORS

# bring in flux data
fluxesfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxesfull.csv", stringsAsFactors=FALSE)

# merge
fluxesfullmerge <- merge(abioticfactors, fluxesfull, all=TRUE)

# sort by date
library(plyr)
fluxesfullmerge2 <- arrange(fluxesfullmerge,desc(SampleDate))

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Combo-Datasets/"
write.csv(fluxesfullmerge2, file=paste(pathsavefiles, "fluxesfullmerge.csv", sep = ""), row.names=FALSE)  


########################################################################
# MAKE HUGE DATASET WITH FLUXES AND ABIOTIC FACTORS - INFO PER SITE-DATE








########################################################################
# LAND USE COMPARISONS - ONLY WET SEASON

require(ggplot2)
require(gridExtra)

# subset out wet season when calling ggplot

# quad - facet by site
soilmoisfig1 <- ggplot(subset(abioticfactors, SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=SoilMoisPercent, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("% Soil Moisture")
soilmoisfig1loess <- soilmoisfig1 + geom_smooth(size = 1.5, fill="#333333", colour="black")




########################################################################
# NOTES AND TESTING





########################################################################
# POSSIBLE TO DO




