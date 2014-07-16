# SiteSummaryStatsTanguro-initialfigs.R
# initial site data to figures
# disrupted N project
# CS O'Connell, UMN EEB/IonE



########################################################################
# BRING IN DATA / PREP

fluxessitesummary <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/fluxessitesummary.csv", stringsAsFactors=FALSE)

# create a col to assign a color in the graphs
fluxessitesummary <- transform(fluxessitesummary, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
fluxessitesummary <- transform(fluxessitesummary, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))

# summary by site and date to df so R likes it
fluxessitesummary$SampleDate2 <- gsub("[.]","/",fluxessitesummary$SampleDate)
fluxessitesummary$SampleDate2 <- as.Date(fluxessitesummary$SampleDate2, format="%Y/%m/%d")

# where to save
pathsavefluxfigures = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Flux-Data-Rprocessed/FluxAnalysis/"


########################################################################
# LAND USE COMPARISONS

require(ggplot2)
require(gridExtra)

# quad - facet by site
p1n <- ggplot(fluxessitesummary, aes(x=SampleDate2, y=meanfluxN2Oq, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("ngN/cm2/hr (quad fit)")
p1nloess <- p1n + geom_smooth(size = 1.5, fill="#333333", colour="black")

p1c <- ggplot(fluxessitesummary, aes(x=SampleDate2, y=meanfluxCO2q, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("ngC/cm2/hr (quad fit)")
p1cloess <- p1c + geom_smooth(size = 1.5, fill="#333333", colour="black")


# linear - facet by site
p2n <- ggplot(fluxessitesummary, aes(x=SampleDate2, y=meanfluxN2Ol, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("ngN/cm2/hr (linear fit)")
p2nloess <- p2n + geom_smooth(size = 1.5, fill="#333333", colour="black")

p2c <- ggplot(fluxessitesummary, aes(x=SampleDate2, y=meanfluxCO2l, color=color.use)) + geom_point(size=2.5) + facet_wrap( ~ LUname, ncol=3) + xlab("Sampling Date") + ylab("ngC/cm2/hr (linear fit)")
p2cloess <- p2c + geom_smooth(size = 1.5, fill="#333333", colour="black")


# grid.arrange
grid.arrange(p1n, p1c, nrow = 2, ncol = 1) # great, this works!  looks a little ugly, but this is the right thing and I'll play with the 
grid.arrange(p1nloess, p1cloess, nrow = 2, ncol = 1) # great, this works!  looks a little ugly, but this is the right thing and I'll play with the 
grid.arrange(p2n, p2c, nrow = 2, ncol = 1) # great, this works!  looks a little ugly, but this is the right thing and I'll play with the 
grid.arrange(p2nloess, p2cloess, nrow = 2, ncol = 1) # great, this works!  looks a little ugly, but this is the right thing and I'll play with the 





########################################################################
# SITE PATTERNS - FERT EFFECT CAPTURED?

plot_1 = ggplot(subset(fluxessitesummary, LUtype %in% c("M") & SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=meanfluxN2Ol)) + geom_point(size=2.5) + geom_line() + facet_grid(. ~ Site) + labs(title = "Possibly captured fertilizer effects", x = "Sampling Date", y = "ngN/cm2/hr (linear fit)")
plot_1

plot_2 = ggplot(subset(fluxessitesummary, Site %in% c("SM") & SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=meanfluxN2Ol)) + geom_point(size=2.5) + geom_line() + labs(title = "", x = "Sampling Date", y = "ngN/cm2/hr (linear fit)")
plot_2

png(file = paste(pathsavefluxfigures, "ferteffects_possible.png", sep=""),width=7,height=6,units="in",res=400)
grid.arrange(plot_1, plot_2, nrow = 2, ncol = 1)
dev.off()


# CO2 fert effect?
plot_3 = ggplot(subset(fluxessitesummary, LUtype %in% c("M") & SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=meanfluxCO2l)) + geom_point(size=2.5) + geom_line() + facet_grid(. ~ Site) + labs(title = "CO2 fertilizer effects?  (comparison)", x = "Sampling Date", y = "ngC/cm2/hr (linear fit)")
plot_3

plot_4 = ggplot(subset(fluxessitesummary, Site %in% c("SM") & SampleDate2 > "2013/11/01"), aes(x=SampleDate2, y=meanfluxCO2l)) + geom_point(size=2.5) + geom_line() + labs(title = "", x = "Sampling Date", y = "ngC/cm2/hr (linear fit)")
plot_4

png(file = paste(pathsavefluxfigures, "ferteffects_carbon_possible.png", sep=""),width=7,height=6,units="in",res=400)
grid.arrange(plot_3, plot_4, nrow = 2, ncol = 1)
dev.off()


########################################################################
# TRASH CODE



# grouped by land use type
plot(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="S"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="S"],ylim=c(-5,10),col="darkblue",xlab="Date", ylab="ngN/cm2/hr")
points(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="M"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="M"],col="darkorange")
points(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="F"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="F"],col="darkgreen")





# how to do axis breaks
y1<-1+rnorm(10)/5
y2<-3+rnorm(10)/5
y3<-4+rnorm(10)/5
y4<-397+rnorm(10)/5

library(plotrix) 
plot(y1,ylim=c(0,10),axes=FALSE,main="Big range plot",ylab="Y values") 
points(y2) 
points(y3) 
box() 
axis(2,at=c(1,2,3,4,6,7,8,9),labels=c("1","2","3","4","396","397","398","399")) 
axis.break(2,5) 
par(new=TRUE) 
plot(y4,ylim=c(390,400),axes=FALSE,main="",ylab="",xlab="")


# alterante ex
x <- c(9.45, 8.78, 0.93, 0.47, 0.24, 0.12)
y <- c(10.72, 10.56, 10.35, 10.10, 9.13, 6.72)
z <- c(7.578, 7.456, 6.956, 6.712, 4.832, 3.345)

library(plotrix)
par(bty="n") # deleting the box
gap.plot(x,y, gap=c(1.5,7.5), gap.axis="x", pch=16,
         col="blue", ylim=range(c(y,z)),
         xtics=c(0:3,8:10), xticlab=c(0:3,8:10))

gap.plot(x,z, gap=c(1.5,7.5), gap.axis="x", pch=17,
         col="red", ylim=range(c(y,z)), add=TRUE); axis(2)

abline(v=seq(1.49,2.09,.001), col="white")  # hiding vertical lines
axis.break(1,1.5,style="slash")               # plotting slashes for breakpoints




plot(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="S"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="S"],ylim=c(-5,10),col="darkblue",xlab="Date", ylab="ngN/cm2/hr")
points(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="M"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="M"],col="darkorange")
points(fluxessitesummary$SampleDate2[fluxessitesummary$LUtype=="F"], fluxessitesummary$meanfluxN2Oq[fluxessitesummary$LUtype=="F"],col="darkgreen")




#example of plot grouped by data
#plot(year[sex=="M"], record[sex=="M"])




########################################################################
# NOTES AND TESTING




########################################################################
# POSSIBLE TO DO

###### don't forget to do all of this for CH4




