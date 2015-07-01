# Tanguro-MasterDataSheet.R
# create master data sheet by combining flux and soil and management data into one huge csv
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# reproduceable after any input csv files change

# output product:
# Tanguro-MasterDataSheet.csv



########################################################################
# BRING IN DATA / PREP

library(lubridate)

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
#fluxid
fluxesfullmerge2$FluxID <- gsub("SM_","S3_", fluxesfullmerge2$FluxID, fixed=TRUE)
fluxesfullmerge2$FluxID <- gsub("SD_","S2_", fluxesfullmerge2$FluxID, fixed=TRUE)

# add column for site-date combo name that the soil N data likes
# do this before you switch the site name to M4
z<-ymd(fluxesfullmerge2$SampleDate)
fluxesfullmerge2$DateAlt <- paste(month(z),day(z),substring(as.character(year(z)[2]),3, 4), sep = "/")
fluxesfullmerge2$easycallname_TMDS_Soil <- do.call(paste, c(fluxesfullmerge2[c("Site", "Chamber", "DateAlt")], sep = "_")) 

# relabel S3 as an M land use, since it did in fact have corn in wet season 2014
fluxesfullmerge2$LUtype[grep("S3", fluxesfullmerge2$Site)] <- "M"
fluxesfullmerge2$Site <- gsub("S3","M4", fluxesfullmerge2$Site, fixed=TRUE)
fluxesfullmerge2$FluxID <- gsub("S3_","M4_", fluxesfullmerge2$FluxID, fixed=TRUE)

# add column for site-date combo name (to find site-date summary stats)
fluxesfullmerge2$easysitename <- do.call(paste, c(fluxesfullmerge2[c("Site", "SampleDate")], sep = "_")) 

# create a col to assign a color in the graphs
fluxesfullmerge2 <- transform(fluxesfullmerge2, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
fluxesfullmerge2 <- transform(fluxesfullmerge2, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))

# create a col to establish a post-fertilization time category variable
fluxesfullmerge2$postfertcat <- NA
fluxesfullmerge2$postfertcat[grepl("M", fluxesfullmerge2$LUtype)] <- "notpostfert"
fluxesfullmerge2$postfertcat[fluxesfullmerge2$FertTimeElapsedMaizePlanting<=15 & fluxesfullmerge2$FertTimeElapsedMaizePlanting>-1] <- "postfert"
fluxesfullmerge2$postfertcat[fluxesfullmerge2$FertTimeElapsedMaizeBroadcast<=15 & fluxesfullmerge2$FertTimeElapsedMaizeBroadcast>-1] <- "postfert"
fluxesfullmerge2$postfertcat[grepl("F", fluxesfullmerge2$LUtype)] <- "forestpostfert"
fluxesfullmerge2$postfertcat[grepl("S", fluxesfullmerge2$LUtype)] <- "soypostfert"

# create a col to establish an annual estimate variable
fluxesfullmerge2$annualest <- NA
fluxesfullmerge2$annualest[grepl("F", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Wet"] <- "F_wet"
fluxesfullmerge2$annualest[grepl("F", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Dry"] <- "F_dry"
fluxesfullmerge2$annualest[grepl("S", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Wet"] <- "S_wet"
fluxesfullmerge2$annualest[grepl("S", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Dry"] <- "S_dry"
fluxesfullmerge2$annualest[grepl("M", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Wet"] <- "M_wet"
fluxesfullmerge2$annualest[grepl("M", fluxesfullmerge2$LUtype) & fluxesfullmerge2$postfertcat=="postfert"] <- "M_postfert"
fluxesfullmerge2$annualest[grepl("M", fluxesfullmerge2$LUtype) & fluxesfullmerge2$Season=="Dry"] <- "M_dry"


########################################################################
# APPEND THE SOIL N POOLS AND RATES INFO

# bring in extractionDF
# see ExtractionCalcs-Rcode.R
extractionDF <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rprocessed/extractionDF_processed.csv", stringsAsFactors=FALSE)

# extract only the relevant columns
extractionDFmerge <-  subset(extractionDF, extractionDF$extinc=="ext" & extractionDF$extlazytest=="24") # ensure no repeats
extractionDFmerge <-  subset(extractionDFmerge, select=c("NO3_N_mgNg", 
                                                         "NH4_N_mgNg", 
                                                         "NO3_N_NH4_N_mgNg", 
                                                         "NO3_N_mgNg_FinalMinusInitial_perDay", 
                                                         "NH4_N_mgNg_FinalMinusInitial_perDay", 
                                                         "NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay", 
                                                         "NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis", 
                                                         "NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis", 
                                                         "NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis", 
                                                         "easycallname_TMDS_Soil"))

# merge based on easycallname_TMDS_Soil
fluxesfullmerge2 <- merge(fluxesfullmerge2,extractionDFmerge,by="easycallname_TMDS_Soil", all=TRUE)
fluxesfullmerge2 <- arrange(fluxesfullmerge2,desc(SampleDate))



########################################################################
# ADD SITE SPECIFIC INFO

# bulk density

# bring in bulk density data
pathbringin = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/"
BDdata <- read.csv(paste(pathbringin, "Soil-Data-Rprocessed/soilbulkdensity_processed.csv", sep = ""), stringsAsFactors=FALSE)

BD <- BDdata[BDdata$SampleType=="combo",]
BD <- BD[c(1,13,14)]
addon <- BD[BD$Site=="S3",]; addon[1] <- "M4"
BD <- rbind(BD,addon)

fluxesfullmerge2 <- merge(fluxesfullmerge2, BD, all=TRUE)
fluxesfullmerge2 <- arrange(fluxesfullmerge2,desc(SampleDate))


########################################################################
# SAVE Tanguro-MasterDataSheet.csv

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/"
write.csv(fluxesfullmerge2, file=paste(pathsavefiles, "Tanguro-MasterDataSheet.csv", sep = ""), row.names=FALSE)  



########################################################################
# NOTES AND TESTING

# should I also include the overall site information (totC, totN, BD, etc. as columns here?)
# there's currently no leakage test analysis inclusion


########################################################################
# POSSIBLE TO DO





