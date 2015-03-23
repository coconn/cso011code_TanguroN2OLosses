# Vial-Leakage-Test.R
# testing how much the vials leak over time, based on leakage test experiment done in collaboration done with the Venterea lab
# disrupted N project
# CS O'Connell, UMN EEB/IonE


# to do
# organize the data without grouping but also have col for days elapsed
# scatter with log y axis
# add methane


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# go to vialdffull
vialdffull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/vialDFfull.csv", stringsAsFactors=FALSE)

# subset out the leakage study vials
leakagetestvials <- subset(vialdffull, grepl("Leak", GCrun))


#### these results are messed up.  is it possible that these are crazy because the standards that were drawn that day aren't the ones that GC-Rcode-fileloop.R is pointing to?
####### yes - that's what was happening.  fixed now by changing the vial names so only the ones filled the day of the run are called Mix1, etc.


########################################################################
# Add in useful additional variables

# label by week
leakagetestvials <- transform(leakagetestvials, 
                                LeakageTime = ifelse(GCrun=="20140606_Leak0", as.character("Leakage Test Week 0"), 
                                                     ifelse(GCrun=="20140613_Leak1", as.character("Leakage Test Week 1"), 
                                                            ifelse(GCrun=="20140701_Leak4", as.character("Leakage Test Week 4"), 
                                                                   ifelse(GCrun=="20140806_Leak8", as.character("Leakage Test Week 8"), 
                                                                          as.character("Leakage Test Week 12"))))))

# time since day leakage test vials were filled (2014.06.05) 
# date started leakage experiment
leakagetestvials <- transform(leakagetestvials, 
                                daterun = ifelse(GCrun=="20140606_Leak0", as.character("2014.06.06"), 
                                                 ifelse(GCrun=="20140613_Leak1", as.character("2014.06.13"), 
                                                        ifelse(GCrun=="20140701_Leak4", as.character("2014.07.02"), 
                                                               ifelse(GCrun=="20140806_Leak8", as.character("2014.08.06"), 
                                                                      as.character("2014.09.05"))))))
leakagetestvials$daterun <- gsub("[.]","/",leakagetestvials$daterun)
leakagetestvials$daterun <- as.Date(leakagetestvials$daterun, format="%Y/%m/%d")
# reformat date of GC run
leakagetestvials$SampleDate <- gsub("[.]","/",leakagetestvials$SampleDate)
leakagetestvials$SampleDate <- as.Date(leakagetestvials$SampleDate, format="%Y/%m/%d")
# days since start
leakagetestvials$datediff <- leakagetestvials$daterun - leakagetestvials$SampleDate
leakagetestvials$datediff <- as.numeric(leakagetestvials$datediff)

# capped by hand or capped with auto crimper?
AUrows <- grepl("^AU", leakagetestvials$SampleName)
leakagetestvials <- transform(leakagetestvials, crimpstyle = ifelse(AUrows==TRUE, as.character("auto"), as.character("hand")))

# initial concentrations
leakagetestvials <- transform(leakagetestvials, 
                              initialcon = ifelse(SampleName=="Mix1Leak" | SampleName=="Mix1" | SampleName=="AU-Mix1Leak", as.character("Mix1"), 
                                               ifelse(SampleName=="Mix2Leak" | SampleName=="Mix2" | SampleName=="AU-Mix2Leak", as.character("Mix2"), 
                                                      ifelse(SampleName=="3N2OLeak" | SampleName=="3N2O" | SampleName=="AU-3N2OLeak", as.character("3N2O"), 
                                                             ifelse(SampleName=="10N2OLeak" | SampleName=="10N2O" | SampleName=="AU-10N2OLeak", as.character("10N2O"), 
                                                                    as.character("3KCO2"))))))

#### Q for Rod - are these concentrations right???
# # ppm values in the standards
# ppmNstds <- c(0.301,1.57,3) # Mix1, Mix2, 3N2O
# ppmCstds <- c(600,1000,3000) # Mix1, Mix2, 3KCO2
# ppmCHstds <- c(1,1.7,0) # Mix1, Mix2, (0,0) - see email "two quick questions: CH4 in standards? and using 10N2O"

# C_0 N2O
leakagetestvials <- transform(leakagetestvials, 
                              initialconN2O = ifelse(SampleName=="Mix1Leak" | SampleName=="Mix1" | SampleName=="AU-Mix1Leak", as.numeric(0.301), 
                                                  ifelse(SampleName=="Mix2Leak" | SampleName=="Mix2" | SampleName=="AU-Mix2Leak", as.numeric(1.57), 
                                                         ifelse(SampleName=="3N2OLeak" | SampleName=="3N2O" | SampleName=="AU-3N2OLeak", as.numeric(3), 
                                                                ifelse(SampleName=="10N2OLeak" | SampleName=="10N2O" | SampleName=="AU-10N2OLeak", as.numeric(10), 
                                                                       as.numeric(0))))))

# C_0 CO2
leakagetestvials <- transform(leakagetestvials, 
                              initialconCO2 = ifelse(SampleName=="Mix1Leak" | SampleName=="Mix1" | SampleName=="AU-Mix1Leak", as.numeric(600), 
                                                  ifelse(SampleName=="Mix2Leak" | SampleName=="Mix2" | SampleName=="AU-Mix2Leak", as.numeric(1000), 
                                                         ifelse(SampleName=="3N2OLeak" | SampleName=="3N2O" | SampleName=="AU-3N2OLeak", as.numeric(0), 
                                                                   ifelse(SampleName=="10N2OLeak" | SampleName=="10N2O" | SampleName=="AU-10N2OLeak", as.numeric(0), 
                                                                          as.numeric(3000))))))

# C_0 CH4
leakagetestvials <- transform(leakagetestvials, 
                              initialconCH4 = ifelse(SampleName=="Mix1Leak" | SampleName=="Mix1" | SampleName=="AU-Mix1Leak", as.numeric(1), 
                                                     ifelse(SampleName=="Mix2Leak" | SampleName=="Mix2" | SampleName=="AU-Mix2Leak", as.numeric(1.7), 
                                                            ifelse(SampleName=="3N2OLeak" | SampleName=="3N2O" | SampleName=="AU-3N2OLeak", as.numeric(0), 
                                                                   ifelse(SampleName=="10N2OLeak" | SampleName=="10N2O" | SampleName=="AU-10N2OLeak", as.numeric(0), 
                                                                          as.numeric(0))))))


########################################################################
# Add exponential decay ratio column

# C_t / C_0 (concentration measured over concentration initial/theoretical)
# C_t is N2Oppm, CO2ppm, CH4ppm columns
# C_0 is initialconN2O, initialconCO2, initialconCH4 columns

lnCtC0N2O <- leakagetestvials$N2Oppm/leakagetestvials$initialconN2O
lnCtC0N2O[is.infinite(lnCtC0N2O)] <- NA 
lnCtC0N2O <- log(lnCtC0N2O)
leakagetestvials <- transform(leakagetestvials, lnCtC0N2O=lnCtC0N2O)

lnCtC0CO2 <- leakagetestvials$CO2ppm/leakagetestvials$initialconCO2
lnCtC0CO2[is.infinite(lnCtC0CO2)] <- NA 
lnCtC0CO2 <- log(lnCtC0CO2)
leakagetestvials <- transform(leakagetestvials, lnCtC0CO2=lnCtC0CO2)

lnCtC0CH4 <- leakagetestvials$CH4ppm/leakagetestvials$initialconCH4
lnCtC0CH4[is.infinite(lnCtC0CH4)] <- NA 
lnCtC0CH4 <- log(lnCtC0CH4)
leakagetestvials <- transform(leakagetestvials, lnCtC0CH4=lnCtC0CH4)


########################################################################
# Save as csv

pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
write.csv(leakagetestvials, file=paste(pathsavefiles, "leakagetestvials.csv", sep = ""), row.names=FALSE)  


########################################################################
# Scatter plots
library(gridExtra)

# where to save plots
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/Leakage Time Figures/"
setwd(pathsavefiles)

# lm line info
source("~/Documents/GITHUB/RPersonalFunctionsChristine/lm_eqn.r")

# N2O graphs
p1 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0N2O)) + geom_point(aes(color=initialcon)) + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration") + annotate(geom="text", x = Inf, y = Inf, vjust=1.25, hjust=1.03, label = lm_eqn(lm(lnCtC0N2O ~ datediff, leakagetestvials)), parse = TRUE)

p2 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0N2O, color=initialcon)) + geom_point() + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration")

png(file = "leakageN2O.png",width=10,height=10,units="in",res=300) # tiff(file=...) also an option
grid.arrange(p1, p2, ncol=1, main=textGrob("Leakage Test, N2O",just="top"))
dev.off()


# CO2 graphs
p1 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0CO2)) + geom_point(aes(color=initialcon)) + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration") + annotate(geom="text", x = Inf, y = Inf, vjust=1.25, hjust=1.03, label = lm_eqn(lm(lnCtC0CO2 ~ datediff, leakagetestvials)), parse = TRUE)

p2 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0CO2, color=initialcon)) + geom_point() + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration")

png(file = "leakageCO2.png",width=10,height=10,units="in",res=300) # tiff(file=...) also an option
grid.arrange(p1, p2, ncol=1, main=textGrob("Leakage Test, CO2",just="top"))
dev.off()


# CH4 graphs
p1 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0CH4)) + geom_point(aes(color=initialcon)) + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration") + annotate(geom="text", x = Inf, y = Inf, vjust=1.25, hjust=1.03, label = lm_eqn(lm(lnCtC0CH4 ~ datediff, leakagetestvials)), parse = TRUE)

p2 <- ggplot(leakagetestvials, aes(x=datediff, y=lnCtC0CH4, color=initialcon)) + geom_point() + geom_smooth(method=lm) + xlab("Days Elapsed Before GC Run") + ylab("ln(C_t / C_0)") + scale_colour_discrete(name="Initial\nConcentration")

png(file = "leakageCH4.png",width=10,height=10,units="in",res=300) # tiff(file=...) also an option
grid.arrange(p1, p2, ncol=1, main=textGrob("Leakage Test, CH4",just="top"))
dev.off()





# 
# ########################################################################
# # Organize data, create leakagetestsummary
# 
# # in the final thing that feed into ggplot:
# # mean and sd, se as columns; mix1leak, mix2leak as the rows
# # use ddply
# 
# require(plyr)
# 
# se <- function(x) sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))
# 
# # ddply for summarizing/subsetting
# leakagetestsummary <- ddply(leakagetestvials, .(GCrun, SampleName), summarize,
#                             mean = mean(ngN_cm3_N2O),
#                             sd = sd(ngN_cm3_N2O),
#                             se = se(ngN_cm3_N2O),
#                             n = length(ngN_cm3_N2O),
#                             SampleDate = unique(SampleDate))
# 
# # add column that meakes it easier to label figures
# leakagetestsummary <- transform(leakagetestsummary, 
#                                 LeakageTime = ifelse(GCrun=="20140606_Leak0", as.character("Leakage Test Week 0"), 
#                                                      ifelse(GCrun=="20140613_Leak1", as.character("Leakage Test Week 1"), 
#                                                             ifelse(GCrun=="20140701_Leak4", as.character("Leakage Test Week 4"), 
#                                                                    ifelse(GCrun=="20140806_Leak8", as.character("Leakage Test Week 8"), 
#                                                                           as.character("Leakage Test Week 12"))))))
# 
# # time since day leakage test vials were filled (2014.06.05) 
# # date started leakage experiment
# leakagetestsummary <- transform(leakagetestsummary, 
#                                 daterun = ifelse(GCrun=="20140606_Leak0", as.character("2014.06.06"), 
#                                                  ifelse(GCrun=="20140613_Leak1", as.character("2014.06.13"), 
#                                                         ifelse(GCrun=="20140701_Leak4", as.character("2014.07.02"), 
#                                                                ifelse(GCrun=="20140806_Leak8", as.character("2014.08.06"), 
#                                                                       as.character("2014.09.05"))))))
# leakagetestsummary$daterun <- gsub("[.]","/",leakagetestsummary$daterun)
# leakagetestsummary$daterun <- as.Date(leakagetestsummary$daterun, format="%Y/%m/%d")
# # reformat date of GC run
# leakagetestsummary$SampleDate <- gsub("[.]","/",leakagetestsummary$SampleDate)
# leakagetestsummary$SampleDate <- as.Date(leakagetestsummary$SampleDate, format="%Y/%m/%d")
# # days since start
# leakagetestsummary$datediff <- leakagetestsummary$daterun - leakagetestsummary$SampleDate
# 
# # capped by hand or capped with auto crimper?
# AUrows <- grepl("^AU", leakagetestsummary$SampleName)
# leakagetestsummary <- transform(leakagetestsummary, crimpstyle = ifelse(AUrows==TRUE, as.character("auto"), as.character("hand")))
# 
# # save as csv
# pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
# write.csv(leakagetestsummary, file=paste(pathsavefiles, "leakagetestsummary.csv", sep = ""), row.names=FALSE)  
# 
# 
# 
# 
# 
# ########################################################################
# # summarySE
# 
# # summarySE
# # Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%)
# # http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#       require(plyr)
#       
#       # New version of length which can handle NA's: if na.rm==T, don't count them
#       length2 <- function (x, na.rm=FALSE) {
#             if (na.rm) sum(!is.na(x))
#             else       length(x)
#       }
#       
#       # This does the summary. For each group's data frame, return a vector with
#       # N, mean, and sd
#       datac <- ddply(data, groupvars, .drop=.drop,
#                      .fun = function(xx, col) {
#                            c(N    = length2(xx[[col]], na.rm=na.rm),
#                              mean = mean   (xx[[col]], na.rm=na.rm),
#                              sd   = sd     (xx[[col]], na.rm=na.rm)
#                            )
#                      },
#                      measurevar
#       )
#       
#       # Rename the "mean" column    
#       datac <- rename(datac, c("mean" = measurevar))
#       
#       datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#       
#       # Confidence interval multiplier for standard error
#       # Calculate t-statistic for confidence interval: 
#       # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#       ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#       datac$ci <- datac$se * ciMult
#       
#       return(datac)
# }
# 
# 
# 
# 
# ########################################################################
# # Summary info
# 
# ## n2o leakage summary
# dt <- leakagetestvials
# dtn <- summarySE(leakagetestvials, measurevar="N2Oppm", groupvars=c("GCrun","SampleName"), na.rm=TRUE)
# 
# # Use GCrun as a factor rather than numeric
# dtn2 <- dtn
# dtn2$GCrun <- factor(dtn2$GCrun)
# 
# # save as csv
# pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
# write.csv(dtn2, file=paste(pathsavefiles, "leakagetestsummary_n2o_byrun.csv", sep = ""), row.names=FALSE)  
# 
# ## co2 leakage summary
# dtc <- summarySE(leakagetestvials, measurevar="CO2ppm", groupvars=c("GCrun","SampleName"), na.rm=TRUE)
# 
# # Use GCrun as a factor rather than numeric
# dtc2 <- dtc
# dtc2$GCrun <- factor(dtc2$GCrun)
# 
# # save as csv
# pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
# write.csv(dtc2, file=paste(pathsavefiles, "leakagetestsummary_co2_byrun.csv", sep = ""), row.names=FALSE)  
# 
# 
# ########################################################################
# # Bar graph looking at each time point (Leak0 - Leak12)
# 
# pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Sensitivity-Experiments/Vial-Leakage-Test/"
# 
# ## n2o
# 
# # png and save
# png(file = paste(pathsavefigures, "leakagetest-bargraph-n2o-se.png", sep=""),width=8,height=6,units="in",res=400)
# # Error bars represent standard error of the mean
# ggplot(dtn2, aes(x=GCrun, y=N2Oppm, fill=SampleName)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       geom_errorbar(aes(ymin=N2Oppm-se, ymax=N2Oppm+se),
#                     width=.2,                    # Width of the error bars
#                     position=position_dodge(.9)) + 
#       theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# dev.off()
# 
# # png and save
# png(file = paste(pathsavefigures, "leakagetest-bargraph-n2o-ci.png", sep=""),width=8,height=6,units="in",res=400)
# # Use 95% confidence intervals instead of SEM
# ggplot(dtn2, aes(x=GCrun, y=N2Oppm, fill=SampleName)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       geom_errorbar(aes(ymin=N2Oppm-ci, ymax=N2Oppm+ci),
#                     width=.2,                    # Width of the error bars
#                     position=position_dodge(.9)) + 
#       theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# dev.off()
# 
# 
# 
# ## co2
# 
# # png and save
# png(file = paste(pathsavefigures, "leakagetest-bargraph-co2-se.png", sep=""),width=8,height=6,units="in",res=400)
# # Error bars represent standard error of the mean
# ggplot(dtc2, aes(x=GCrun, y=CO2ppm, fill=SampleName)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       geom_errorbar(aes(ymin=CO2ppm-se, ymax=CO2ppm+se),
#                     width=.2,                    # Width of the error bars
#                     position=position_dodge(.9)) + 
#       theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# dev.off()
# 
# # png and save
# png(file = paste(pathsavefigures, "leakagetest-bargraph-co2-ci.png", sep=""),width=8,height=6,units="in",res=400)
# # Use 95% confidence intervals instead of SEM
# ggplot(dtc2, aes(x=GCrun, y=CO2ppm, fill=SampleName)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       geom_errorbar(aes(ymin=CO2ppm-ci, ymax=CO2ppm+ci),
#                     width=.2,                    # Width of the error bars
#                     position=position_dodge(.9)) + 
#       theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# # # summarySE
# # # Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%)
# # # http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
# # summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
# #                       conf.interval=.95, .drop=TRUE) {
# #   require(plyr)
# #   
# #   # New version of length which can handle NA's: if na.rm==T, don't count them
# #   length2 <- function (x, na.rm=FALSE) {
# #     if (na.rm) sum(!is.na(x))
# #     else       length(x)
# #   }
# #   
# #   # This does the summary. For each group's data frame, return a vector with
# #   # N, mean, and sd
# #   datac <- ddply(data, groupvars, .drop=.drop,
# #                  .fun = function(xx, col) {
# #                    c(N    = length2(xx[[col]], na.rm=na.rm),
# #                      mean = mean   (xx[[col]], na.rm=na.rm),
# #                      sd   = sd     (xx[[col]], na.rm=na.rm)
# #                    )
# #                  },
# #                  measurevar
# #   )
# #   
# #   # Rename the "mean" column    
# #   datac <- rename(datac, c("mean" = measurevar))
# #   
# #   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
# #   
# #   # Confidence interval multiplier for standard error
# #   # Calculate t-statistic for confidence interval: 
# #   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
# #   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
# #   datac$ci <- datac$se * ciMult
# #   
# #   return(datac)
# # }
# # 
# # df <- ToothGrowth
# # dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
# # 
# # library(ggplot2)
# # 
# # # Use dose as a factor rather than numeric
# # dfc2 <- dfc
# # dfc2$dose <- factor(dfc2$dose)
# # 
# # # Error bars represent standard error of the mean
# # ggplot(dfc2, aes(x=dose, y=len, fill=supp)) + 
# #   geom_bar(position=position_dodge(), stat="identity") +
# #   geom_errorbar(aes(ymin=len-se, ymax=len+se),
# #                 width=.2,                    # Width of the error bars
# #                 position=position_dodge(.9))
# # 
# # 
# # # Use 95% confidence intervals instead of SEM
# # ggplot(dfc2, aes(x=dose, y=len, fill=supp)) + 
# #   geom_bar(position=position_dodge(), stat="identity") +
# #   geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
# #                 width=.2,                    # Width of the error bars
# #                 position=position_dodge(.9))
# 
# 
# 
# 
# 
# 
# ########################################################################
# # Line graph over days vials were sitting (Day 0 to Day 92)
# 
# 
# 
# 
# 
# 
# 
# 
# ########################################################################
# # NOTES AND TESTING
# 
# # see notes in evernote from mtg with Eric Davidson
# # come up with way to rigorously deal with linear vs. quad, etc.
# 
# 
# 
# ########################################################################
# # POSSIBLE TO DO
# 
# ###### don't forget to do all of this for CH4
# 
# 


