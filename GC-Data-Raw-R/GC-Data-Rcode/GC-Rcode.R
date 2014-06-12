# processing GC data - fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE


## pick a particular GC run
## bring in data

# bring in info about the GC run
runsheet <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/n2o_runsheet_U.txt")

# bring in GC data
ecdN2O <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/ecd.txt", header = FALSE)
fidCH4 <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/fid.txt", header = FALSE)
tcdCO2 <- read.delim("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/PRACTICE/U/tcd.txt", header = FALSE)


## start vial info and conversions dataframe

N2Oraw <- ecdN2O$V6
CO2raw <- tcdCO2$V6
CH4raw <- fidCH4$V6
SampleName <- runsheet$SampleName
Site <- runsheet$Site
LUtype <- runsheet$LUtype
Chamber <- runsheet$Chamber
TimePt <- runsheet$TimePt
TimePtSq <- runsheet$TimePtSq
SampleDate <- runsheet$SampleDate

vialDF <- data.frame(N2Oraw, CO2raw, CH4raw, SampleName, Site, LUtype, Chamber, TimePt, TimePtSq, SampleDate)

vialDF


## solve for ambient-corrected
## solve for ppm corrected
## solve for ngN_cm3 and ngC_cm3

# standards curves



# ambient correction info



# check on time 0 values inside chambers



# issue report: is anything wrong?









## stds curves

# build standards curve - N2O

# build standards curve - CO2

# build standards curve - CH4







