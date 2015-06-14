# Figures-TanguroN2OLosses-scatterplotmatrix.R
# 
# manuscript ready figure, scatterplot matrix of covariates
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# UNITS LABELING: 1 microgram (ug) = 1000 nanograms (ng), so micrograms are 1000 times bigger.  CO2 fluxes are in migrograms/cm2/h, N2O are in nanograms/cm2/h.



### list of figures to make
# publication applicable fluxes over time (for each site, I reckon)
# publication applicable soil inorg N and moisture over time (for each site, I reckon)
# include fertilization events in these
# row-interrow bar graphs (whole period vs. only in the post-fert windows)
# correlaion table of log-transformed variables (simple scatterplots)





########################################################################
# BRING IN DATA / PREP

# where to save figure
pathsavefigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-RProcessed/"


library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# bring in site-date combination summary data (combines info from multiple chambers)
sitedatesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterSiteDateSummary.csv", stringsAsFactors=FALSE)

# correlation matrix
corrdf <- data.frame(fluxesfullmerge$Site[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$LUtype[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$Chamber[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$RowInter[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$SampleDate[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$LinearFlux[(fluxesfullmerge$GasType)=="N2O"], 
                     fluxesfullmerge$LinearFlux[(fluxesfullmerge$GasType)=="CO2"], 
                     fluxesfullmerge$LinearFlux[(fluxesfullmerge$GasType)=="CH4"], 
                     fluxesfullmerge$FertTimeElapsedMaizePlanting[(fluxesfullmerge$GasType)=="N2O"], 
                     fluxesfullmerge$FertTimeElapsedMaizeBroadcast[(fluxesfullmerge$GasType)=="N2O"], 
                     fluxesfullmerge$SoilMoisPercent[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$NO3_N_mgNg[(fluxesfullmerge$GasType)=="N2O"], 
                     fluxesfullmerge$NH4_N_mgNg[(fluxesfullmerge$GasType)=="N2O"], 
                     fluxesfullmerge$NO3_N_NH4_N_mgNg[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis[(fluxesfullmerge$GasType)=="N2O"],
                     fluxesfullmerge$NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis[(fluxesfullmerge$GasType)=="N2O"])
names(corrdf) <- c("Site","LUtype","Chamber","RowInter",
                   "SampleDate","LinearFluxN2O","LinearFluxCO2","LinearFluxCH4",
                   "FertTimeElapsedMaizePlanting","FertTimeElapsedMaizeBroadcast","SoilMoisPercent",
                   "NO3_N_mgNg","NH4_N_mgNg","NO3_N_NH4_N_mgNg",
                   "NO3_netnitr_perDay_AreaBasis",
                   "NH4_netamm_FinalMinusInitial_perDay_AreaBasis",
                   "NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")

# get rid of random NA rows
sitedatesummary <- subset(sitedatesummary, sitedatesummary$easysitename!="NA")
fluxesfullmerge <- subset(fluxesfullmerge, fluxesfullmerge$easysitename!="NA")
corrdf <- subset(corrdf, corrdf$Site!="NA")


########################################################################
# FUNCTION FOR SCATTERPLOT GGPLOTS

# applies to all the graphs
factorcolors <- c("#00BA38","#619CFF","#F8766D") # c("#1f78b4","#33a02c") # c("#619CFF","#F8766D")
rhosize <- 4

# function for correlation df for reporting
corfun<-function(x, y) {corr=(cor.test(x, y))}

# function for printing the nice labels
cococodepath = "~/Documents/GITHUB/RPersonalFunctionsChristine/"
source(paste(cococodepath, "cor_stars_info.R", sep=""))

oconnelldiss_tanuro_scatterplots <- function(corrdf,colnames,xlab,ylab,corinfo) {

      plotdf <- corrdf[,colnames]
      
      p <- ggplot(plotdf, aes(x=plotdf[,3], y=plotdf[,2], color=plotdf[,1]), environment = environment()) 
      p <- p + geom_point(shape=1) 
      p <- p + scale_colour_manual(values = factorcolors) 
      p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE) 
      p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE, fullrange=TRUE, lty = 2) 
      p <- p + theme_bw() 
      p <- p + ylab(ylabel) 
      p <- p + xlab(xlabel) 
      p <- p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.2, label = corinfo$infotmp[1], parse = T) 
      p <- p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.4, label = corinfo$infotmp[2], parse = T) 
      p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=3.6, label = corinfo$infotmp[3], parse = T) + theme(legend.position="none")
      
}


########################################################################
# BUILD SCATTERPLOTS

## N2O
ylabel <- "Flux N2O: ngN / cm2 / h"

colnames <- c("LUtype","LinearFluxN2O","SoilMoisPercent")
xlabel <- "% Soil Moisture"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(SoilMoisPercent,LinearFluxN2O)$estimate,
                 pval=corfun(SoilMoisPercent,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(SoilMoisPercent,LinearFluxN2O,unique(as.character(LUtype))))
p1n <- oconnelldiss_tanuro_scatterplots(corrdf=corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxN2O","NO3_N_mgNg")
xlabel <- "NO3-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_mgNg,LinearFluxN2O)$estimate,
                 pval=corfun(NO3_N_mgNg,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
p2n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxN2O","NH4_N_mgNg")
xlabel <- "NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_N_mgNg,LinearFluxN2O)$estimate,
                 pval=corfun(NH4_N_mgNg,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
p3n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxN2O","NO3_N_NH4_N_mgNg")
xlabel <- "NO3-N + NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_NH4_N_mgNg,LinearFluxN2O)$estimate,
                 pval=corfun(NO3_N_NH4_N_mgNg,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_NH4_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
p4n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxN2O","NO3_netnitr_perDay_AreaBasis")
xlabel <- "Net nitrificationrate (NO3), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O)$estimate,
                 pval=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
p5n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxN2O","NH4_netamm_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net ammonificationrate (NH4), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$estimate,
                 pval=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
p6n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxN2O","NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$estimate,
                 pval=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
p7n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)



## CO2
ylabel <- "Flux CO2: ugC / cm2 / h"

colnames <- c("LUtype","LinearFluxCO2","SoilMoisPercent")
xlabel <- "% Soil Moisture"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(SoilMoisPercent,LinearFluxCO2)$estimate,
                 pval=corfun(SoilMoisPercent,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(SoilMoisPercent,LinearFluxCO2,unique(as.character(LUtype))))
p1co <- oconnelldiss_tanuro_scatterplots(corrdf=corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCO2","NO3_N_mgNg")
xlabel <- "NO3-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_mgNg,LinearFluxCO2)$estimate,
                 pval=corfun(NO3_N_mgNg,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
p2co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCO2","NH4_N_mgNg")
xlabel <- "NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_N_mgNg,LinearFluxCO2)$estimate,
                 pval=corfun(NH4_N_mgNg,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
p3co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCO2","NO3_N_NH4_N_mgNg")
xlabel <- "NO3-N + NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_NH4_N_mgNg,LinearFluxCO2)$estimate,
                 pval=corfun(NO3_N_NH4_N_mgNg,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_NH4_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
p4co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCO2","NO3_netnitr_perDay_AreaBasis")
xlabel <- "Net nitrificationrate (NO3), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2)$estimate,
                 pval=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
p5co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCO2","NH4_netamm_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net ammonificationrate (NH4), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$estimate,
                 pval=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
p6co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCO2","NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$estimate,
                 pval=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
p7co <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


## CH4
ylabel <- "Flux CH4: ugC / cm2 / h"

colnames <- c("LUtype","LinearFluxCH4","SoilMoisPercent")
xlabel <- "% Soil Moisture"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(SoilMoisPercent,LinearFluxCH4)$estimate,
                 pval=corfun(SoilMoisPercent,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(SoilMoisPercent,LinearFluxCH4,unique(as.character(LUtype))))
p1ch <- oconnelldiss_tanuro_scatterplots(corrdf=corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCH4","NO3_N_mgNg")
xlabel <- "NO3-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_mgNg,LinearFluxCH4)$estimate,
                 pval=corfun(NO3_N_mgNg,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_mgNg,LinearFluxCH4,unique(as.character(LUtype))))
p2ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCH4","NH4_N_mgNg")
xlabel <- "NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_N_mgNg,LinearFluxCH4)$estimate,
                 pval=corfun(NH4_N_mgNg,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_N_mgNg,LinearFluxCH4,unique(as.character(LUtype))))
p3ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)

colnames <- c("LUtype","LinearFluxCH4","NO3_N_NH4_N_mgNg")
xlabel <- "NO3-N + NH4-N mgN/g"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_NH4_N_mgNg,LinearFluxCH4)$estimate,
                 pval=corfun(NO3_N_NH4_N_mgNg,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_N_NH4_N_mgNg,LinearFluxCH4,unique(as.character(LUtype))))
p4ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCH4","NO3_netnitr_perDay_AreaBasis")
xlabel <- "Net nitrificationrate (NO3), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCH4)$estimate,
                 pval=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_netnitr_perDay_AreaBasis,LinearFluxCH4,unique(as.character(LUtype))))
p5ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCH4","NH4_netamm_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net ammonificationrate (NH4), area basis, mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4)$estimate,
                 pval=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4,unique(as.character(LUtype))))
p6ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)


colnames <- c("LUtype","LinearFluxCH4","NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")
xlabel <- "Net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1"
corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4)$estimate,
                 pval=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4)$p.value,
                 n=length(LUtype), infotmp=cor_stars_info(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCH4,unique(as.character(LUtype))))
p7ch <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)




########################################################################
# SCATTERPLOTS INTO GRID ARRANGE AND SAVE

# grid.arrange and save
png(file = paste(pathsavefigures, "scatterplots1.png", sep=""),width=30,height=5,units="in",res=400)
grid.arrange(p1n, p2n, p3n, p4n, p5n, p6n, p7n, nrow = 1, ncol = 7)
dev.off()

# grid.arrange and save
png(file = paste(pathsavefigures, "scatterplots2.png", sep=""),width=30,height=5,units="in",res=400)
grid.arrange(p1co, p2co, p3co, p4co, p5co, p6co, p7co, nrow = 1, ncol = 7)
dev.off()

# grid.arrange and save
png(file = paste(pathsavefigures, "scatterplots3.png", sep=""),width=30,height=5,units="in",res=400)
grid.arrange(p1ch, p2ch, p3ch, p4ch, p5ch, p6ch, p7ch, nrow = 1, ncol = 7)
dev.off()

# grid.arrange and save
png(file = paste(pathsavefigures, "scatterplots4.png", sep=""),width=30,height=20,units="in",res=400)
grid.arrange(p1n, p2n, p3n, p4n, p5n, p6n, p7n, p1co, p2co, p3co, p4co, p5co, p6co, p7co, p1ch, p2ch, p3ch, p4ch, p5ch, p6ch, p7ch, nrow = 3, ncol = 7)
dev.off()


# # versions with shared legend
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/grid_arrange_shared_legend.r")
# this is acting weird for some reason
# png(file = paste(pathsavefigures, "scatterplots1_alt.png", sep=""),width=30,height=5,units="in",res=400)
# grid_arrange_shared_legend(p1n, p2n, p3n, p4n, p5n, p6n, p7n, nrow = 1, ncol = 7)
# dev.off()









########################################################################
# LOOP TO BUILD SCATTERPLOT GGPLOTS

##### fuck this, do this later
# 
# soilvarlist <- c("NO3_N_mgNg")
# 
# for (i in 1:length(soilvarlist)) {
#  
#       
#       
# }





















########################################################################
# ROWS ARE GASES, COLUMNS ARE SOIL VARS








# add labels to each plot (Figure 3 a-i, etc.)
plot1 <- plot1 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "a", size=6)
plot2 <- plot2 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "b", size=6)
plot3 <- plot3 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "c", size=6)
plot4 <- plot4 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "d", size=6)
plot5 <- plot5 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "e", size=6)
plot6 <- plot6 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "f", size=6)
plot7 <- plot7 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "g", size=6)
plot8 <- plot8 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "h", size=6)
plot9 <- plot9 + annotate(geom="text", x = Inf, y = Inf, hjust=2.4, vjust=1.5, colour = factorcolors[1], label = "i", size=6)


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3, main=textGrob("Impacts when you vary how services are prioritized",gp=gpar(fontsize=20,font=1),just="top"))






########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO




########################################################################
# SCATTERPLOT MATRIX, SOME OPTIONS WHEN I WAS EXPLORING

# some options

# # bring in fancy correlation graph code
# cococodepath = "~/Documents/GITHUB/RPersonalFunctionsChristine/"
# source(paste(cococodepath, "correlation-graphs-fancy.R", sep=""))
# cols <- c(1,2,3,6,7,8)
# pairs.panels(corrdf[,cols], smooth=TRUE, scale=FALSE,main="Correlation Matrix")
# 
# # ggpairs
# library(GGally)
# 
# cols <- c(match("LinearFlux",names(fluxesfullmerge)), 
#           match("SoilMoisPercent",names(fluxesfullmerge)),
#           match("NO3_N_mgNg",names(fluxesfullmerge)),
#           match("NH4_N_mgNg",names(fluxesfullmerge)),
#           match("NO3_N_NH4_N_mgNg",names(fluxesfullmerge)))
# 
# ggpairs(fluxesfullmerge[,cols], color = "GasType")
# 
# ggpairs(corrdf[,3:5],
# 
# 
# # scatterplotmatrix
# library(car)
# scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars,main="Three Cylinder Options")





