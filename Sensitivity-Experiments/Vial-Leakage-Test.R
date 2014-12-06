# Vial-Leakage-Test.R
# testing how much the vials leak over time, based on leakage test experiment done in collaboration done with the Venterea lab
# disrupted N project
# CS O'Connell, UMN EEB/IonE


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# go to vialdffull
vialdffull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/vialDFfull.csv", stringsAsFactors=FALSE)

# subset out the leakage study vials
leakagetestvials <- subset(vialdffull, grepl("Leak", GCrun))

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
write.csv(leakagetestvials, file=paste(pathsavefiles, "leakagetestvials.csv", sep = ""), row.names=FALSE)  



#### these results are messed up.  is it possible that these are crazy because the standards that were drawn that day aren't the ones that GC-Rcode-fileloop.R is pointing to?
####### yes - that's what was happening.  fixed now by changing the vial names so only the ones filled the day of the run are called Mix1, etc.


########################################################################
# Organize data, create leakagetestsummary

# in the final thing that feed into ggplot:
# mean and sd, se as columns; mix1leak, mix2leak as the rows
# use ddply

require(plyr)

se <- function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))

# ddply for summarizing/subsetting
leakagetestsummary <- ddply(leakagetestvials, .(GCrun, SampleName), summarize,
      mean = mean(ngN_cm3_N2O),
      sd = sd(ngN_cm3_N2O),
      se = se(ngN_cm3_N2O),
      n = length(ngN_cm3_N2O),
      SampleDate = unique(SampleDate))

# add column that meakes it easier to label figures
leakagetestsummary <- transform(leakagetestsummary, 
                                LeakageTime = ifelse(GCrun=="20140606_Leak0", as.character("Leakage Test Week 0"), 
                                                     ifelse(GCrun=="20140613_Leak1", as.character("Leakage Test Week 1"), 
                                                            ifelse(GCrun=="20140701_Leak4", as.character("Leakage Test Week 4"), 
                                                                   ifelse(GCrun=="20140806_Leak8", as.character("Leakage Test Week 8"), 
                                                                          as.character("Leakage Test Week 12"))))))

# time since day leakage test vials were filled (2014.06.05) 
# date started leakage experiment
leakagetestsummary <- transform(leakagetestsummary, 
                                daterun = ifelse(GCrun=="20140606_Leak0", as.character("2014.06.06"), 
                                                     ifelse(GCrun=="20140613_Leak1", as.character("2014.06.13"), 
                                                            ifelse(GCrun=="20140701_Leak4", as.character("2014.07.02"), 
                                                                   ifelse(GCrun=="20140806_Leak8", as.character("2014.08.06"), 
                                                                          as.character("2014.09.05"))))))
leakagetestsummary$daterun <- gsub("[.]","/",leakagetestsummary$daterun)
leakagetestsummary$daterun <- as.Date(leakagetestsummary$daterun, format="%Y/%m/%d")
# reformat date of GC run
leakagetestsummary$SampleDate <- gsub("[.]","/",leakagetestsummary$SampleDate)
leakagetestsummary$SampleDate <- as.Date(leakagetestsummary$SampleDate, format="%Y/%m/%d")
# days since start
leakagetestsummary$datediff <- leakagetestsummary$daterun - leakagetestsummary$SampleDate

# capped by hand or capped with auto crimper?
AUrows <- grepl("^AU", leakagetestsummary$SampleName)
leakagetestsummary <- transform(leakagetestsummary, crimpstyle = ifelse(AUrows==TRUE, as.character("auto"), as.character("hand")))

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
write.csv(leakagetestsummary, file=paste(pathsavefiles, "leakagetestsummary.csv", sep = ""), row.names=FALSE)  



########################################################################
# Bar graphs looking at each time point (Leak0 - Leak12)








########################################################################
# Line graph over days vials were sitting (Day 0 to Day 92)








########################################################################
# NOTES AND TESTING

# see notes in evernote from mtg with Eric Davidson
# come up with way to rigorously deal with linear vs. quad, etc.



########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




