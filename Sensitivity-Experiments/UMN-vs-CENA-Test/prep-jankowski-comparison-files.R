# prep-jankowski-comparison-files.R
# get everything together to send on to KathiJo, Chris and Eric re: comparison the O'Connell and Jankowski methods
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# COPY FILES THAT KATHIJO MIGHT WANT INTO A FOLDER FOR HER

# copy everything to folder for KathiJo

# raw GC data
targetdir <- c("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/UMN-vs-CENA-Test/InfoForJankowskiComparison/Raw-GC-Data/")
origindir <- c("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-RawFolders/2014 10 Nov Jankowski Comparison")
filestocopy <- c(paste(origindir, "Run Sheets Jankowski Comparison/Christine 14Novimg001.jpg", sep = "/"),
                 paste(origindir, "Run Sheets Jankowski Comparison/Christine14Novimg002.jpg", sep = "/"),
                 paste(origindir, "Runs Jankowski Comparison/ecd.txt", sep = "/"),
                 paste(origindir, "Runs Jankowski Comparison/fid.txt", sep = "/"),
                 paste(origindir, "Runs Jankowski Comparison/tcd.txt", sep = "/"))
file.copy(from=filestocopy, to=targetdir, copy.mode = TRUE)

# vial processing (processing GC data)
targetdir <- c("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/UMN-vs-CENA-Test/InfoForJankowskiComparison/Vial-Processing/")
origindir <- c("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed")
filestocopy <- c(paste(origindir, "ambinfoDF_20141201_JankowskiA.csv", sep = "/"),
                 paste(origindir, "ambinfoDF_20141201_JankowskiB.csv", sep = "/"),
                 paste(origindir, "timezeroDF_20141201_JankowskiA.csv", sep = "/"),
                 paste(origindir, "timezeroDF_20141201_JankowskiB.csv", sep = "/"),
                 paste(origindir, "vialDF_20141201_JankowskiA.csv", sep = "/"),
                 paste(origindir, "vialDF_20141201_JankowskiB.csv", sep = "/"),
                 paste(origindir, "warnings_20141201_JankowskiA.csv", sep = "/"),
                 paste(origindir, "warnings_20141201_JankowskiB.csv", sep = "/"))
file.copy(from=filestocopy, to=targetdir, copy.mode = TRUE)

# descriptive files (rusheets and field notebook scan)
targetdir <- c("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/UMN-vs-CENA-Test/InfoForJankowskiComparison/Descriptive-Info/")
filestocopy <- c("~/Desktop/RESEARCH PROJECTS/cso011_TanguroN2OLosses/fieldnotebook_2014novtrip_wetseason_notebookpartial_tanguro.pdf",
                 "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-standardizedfilenames/runsheets-standardizedfilenames/runsheet_20141201_JankowskiA.xlsx",                 
                 "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-standardizedfilenames/runsheets-standardizedfilenames/runsheet_20141201_JankowskiB.xlsx")
file.copy(from=filestocopy, to=targetdir, copy.mode = TRUE)



########################################################################
# CUT DOWN fluxesfull.csv

# both of these files are all of the fluxes I've ever looked at.  Cut them down to only the relevant data for an O'Connell-Jankowski protocol comparison test

# fluxesfull.csv
fluxesfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxesfull.csv", stringsAsFactors=FALSE)

# subset out the leakage study vials
fluxes2014Nov <- subset(fluxesfull, SampleDate>"2014.11.01")
fluxes2014Nov$Site <- gsub("SM","S3", fluxes2014Nov$Site, fixed=TRUE) # fix Christine shorthand labeling
fluxes2014Nov$Site <- gsub("SD","S2", fluxes2014Nov$Site, fixed=TRUE) # fix Christine shorthand labeling

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/UMN-vs-CENA-Test/InfoForJankowskiComparison/"
write.csv(fluxes2014Nov, file=paste(pathsavefiles, "fluxes2014Nov.csv", sep = ""), row.names=FALSE)  




########################################################################
# CUT DOWN Tanguro-MasterDataSheet.csv

# Tanguro-MasterDataSheet.csv
mastersheet <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

# subset out the leakage study vials
mastersheet2014Nov <- subset(mastersheet, SampleDate>"2014.11.01")
mastersheet2014Nov$Site <- gsub("SM","S3", mastersheet2014Nov$Site, fixed=TRUE) # fix Christine shorthand labeling
mastersheet2014Nov$Site <- gsub("SD","S2", mastersheet2014Nov$Site, fixed=TRUE) # fix Christine shorthand labeling

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/UMN-vs-CENA-Test/InfoForJankowskiComparison/"
write.csv(mastersheet2014Nov, file=paste(pathsavefiles, "mastersheet2014Nov.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4





