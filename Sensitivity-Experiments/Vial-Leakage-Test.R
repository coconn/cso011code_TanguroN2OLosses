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
# summarySE

# summarySE
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%)
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
      require(plyr)
      
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm=FALSE) {
            if (na.rm) sum(!is.na(x))
            else       length(x)
      }
      
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- ddply(data, groupvars, .drop=.drop,
                     .fun = function(xx, col) {
                           c(N    = length2(xx[[col]], na.rm=na.rm),
                             mean = mean   (xx[[col]], na.rm=na.rm),
                             sd   = sd     (xx[[col]], na.rm=na.rm)
                           )
                     },
                     measurevar
      )
      
      # Rename the "mean" column    
      datac <- rename(datac, c("mean" = measurevar))
      
      datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
      
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval: 
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
      datac$ci <- datac$se * ciMult
      
      return(datac)
}




########################################################################
# Summary info

## n2o leakage summary
dt <- leakagetestvials
dtn <- summarySE(leakagetestvials, measurevar="N2Oppm", groupvars=c("GCrun","SampleName"), na.rm=TRUE)

# Use GCrun as a factor rather than numeric
dtn2 <- dtn
dtn2$GCrun <- factor(dtn2$GCrun)

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
write.csv(dtn2, file=paste(pathsavefiles, "leakagetestsummary_n2o_byrun.csv", sep = ""), row.names=FALSE)  

## co2 leakage summary
dtc <- summarySE(leakagetestvials, measurevar="CO2ppm", groupvars=c("GCrun","SampleName"), na.rm=TRUE)

# Use GCrun as a factor rather than numeric
dtc2 <- dtc
dtc2$GCrun <- factor(dtc2$GCrun)

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
write.csv(dtc2, file=paste(pathsavefiles, "leakagetestsummary_co2_byrun.csv", sep = ""), row.names=FALSE)  


########################################################################
# Bar graph looking at each time point (Leak0 - Leak12)

pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"

## n2o

# png and save
png(file = paste(pathsavefigures, "leakagetest-bargraph-n2o-se.png", sep=""),width=8,height=6,units="in",res=400)
# Error bars represent standard error of the mean
ggplot(dtn2, aes(x=GCrun, y=N2Oppm, fill=SampleName)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=N2Oppm-se, ymax=N2Oppm+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
dev.off()

# png and save
png(file = paste(pathsavefigures, "leakagetest-bargraph-n2o-ci.png", sep=""),width=8,height=6,units="in",res=400)
# Use 95% confidence intervals instead of SEM
ggplot(dtn2, aes(x=GCrun, y=N2Oppm, fill=SampleName)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=N2Oppm-ci, ymax=N2Oppm+ci),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
dev.off()



## co2

# png and save
png(file = paste(pathsavefigures, "leakagetest-bargraph-co2-se.png", sep=""),width=8,height=6,units="in",res=400)
# Error bars represent standard error of the mean
ggplot(dtc2, aes(x=GCrun, y=CO2ppm, fill=SampleName)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=CO2ppm-se, ymax=CO2ppm+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
dev.off()

# png and save
png(file = paste(pathsavefigures, "leakagetest-bargraph-co2-ci.png", sep=""),width=8,height=6,units="in",res=400)
# Use 95% confidence intervals instead of SEM
ggplot(dtc2, aes(x=GCrun, y=CO2ppm, fill=SampleName)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=CO2ppm-ci, ymax=CO2ppm+ci),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
dev.off()








# # summarySE
# # Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%)
# # http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#   require(plyr)
#   
#   # New version of length which can handle NA's: if na.rm==T, don't count them
#   length2 <- function (x, na.rm=FALSE) {
#     if (na.rm) sum(!is.na(x))
#     else       length(x)
#   }
#   
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, and sd
#   datac <- ddply(data, groupvars, .drop=.drop,
#                  .fun = function(xx, col) {
#                    c(N    = length2(xx[[col]], na.rm=na.rm),
#                      mean = mean   (xx[[col]], na.rm=na.rm),
#                      sd   = sd     (xx[[col]], na.rm=na.rm)
#                    )
#                  },
#                  measurevar
#   )
#   
#   # Rename the "mean" column    
#   datac <- rename(datac, c("mean" = measurevar))
#   
#   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval: 
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#   datac$ci <- datac$se * ciMult
#   
#   return(datac)
# }
# 
# df <- ToothGrowth
# dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
# 
# library(ggplot2)
# 
# # Use dose as a factor rather than numeric
# dfc2 <- dfc
# dfc2$dose <- factor(dfc2$dose)
# 
# # Error bars represent standard error of the mean
# ggplot(dfc2, aes(x=dose, y=len, fill=supp)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=len-se, ymax=len+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# 
# 
# # Use 95% confidence intervals instead of SEM
# ggplot(dfc2, aes(x=dose, y=len, fill=supp)) + 
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))






########################################################################
# Line graph over days vials were sitting (Day 0 to Day 92)








########################################################################
# NOTES AND TESTING

# see notes in evernote from mtg with Eric Davidson
# come up with way to rigorously deal with linear vs. quad, etc.



########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




