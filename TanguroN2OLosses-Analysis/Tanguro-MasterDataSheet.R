# Tanguro-MasterDataSheet.R
# create master data sheet by combining flux and soil and management data into one huge csv
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# reproduceable after any input csv files change

# output product:
# Tanguro-MasterDataSheet.csv



########################################################################
# BRING IN DATA / PREP

### bring in soil/abiotic factors data 
# (this is the one I hand write based on my notebooks)
abioticfactors <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/abioticfactors.txt", stringsAsFactors=FALSE, na.strings="NA")

# summary by site and date to df so R likes it
abioticfactors$SampleDate <- gsub("/14","/2014", abioticfactors$SampleDate , fixed=TRUE)
abioticfactors$SampleDate2 <- gsub("[.]","/",abioticfactors$SampleDate)
abioticfactors$SampleDate2 <- as.Date(abioticfactors$SampleDate2, format="%Y/%m/%d")

# new name so you never accidentally write over the original, hand-entered file
abioticfactorsprocessed <- abioticfactors

### bring in master fluxes doc
# (this is the one created in FluxCalcs-Rcode.R)
fluxesfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxesfull.csv", stringsAsFactors=FALSE)


########################################################################
# MAKE HUGE DATASET WITH FLUXES AND ABIOTIC FACTORS, Tanguro-MasterDataSheet.csv

# merge
fluxesfullmerge <- merge(abioticfactors, fluxesfull, all=TRUE)


# sort by date
library(plyr)
fluxesfullmerge2 <- arrange(fluxesfullmerge,desc(SampleDate))

# make sure that no sites are still labeled SD or SM
#site
fluxesfullmerge2$Site <- gsub("SM","S3", fluxesfullmerge2$Site, fixed=TRUE)
fluxesfullmerge2$Site <- gsub("SD","S2", fluxesfullmerge2$Site, fixed=TRUE)
# easysitename
fluxesfullmerge2$easysitename <- gsub("SM_","S3_", fluxesfullmerge2$easysitename, fixed=TRUE)
fluxesfullmerge2$easysitename <- gsub("SD_","S2_", fluxesfullmerge2$easysitename, fixed=TRUE)
#fluxid
fluxesfullmerge2$FluxID <- gsub("SM_","S3_", fluxesfullmerge2$FluxID, fixed=TRUE)
fluxesfullmerge2$FluxID <- gsub("SD_","S2_", fluxesfullmerge2$FluxID, fixed=TRUE)

# add column for site-date combo name (to find site-date summary stats)
fluxesfullmerge2$easysitename <- do.call(paste, c(fluxesfullmerge2[c("Site", "SampleDate")], sep = "_")) 

# create a col to assign a color in the graphs
fluxesfullmerge2 <- transform(fluxesfullmerge2, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
fluxesfullmerge2 <- transform(fluxesfullmerge2, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))



########################################################################
# APPEND THE LEAKAGE CORRECTION ANALYSES







########################################################################
# SAVE Tanguro-MasterDataSheet.csv

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/"
write.csv(fluxesfullmerge2, file=paste(pathsavefiles, "Tanguro-MasterDataSheet.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING

# should I also include the overall site information (totC, totN, BD, etc. as columns here?)



########################################################################
# POSSIBLE TO DO





