# tanguro C/N ratio data

wdname <- getwd()

# create the data frame
data <- read.delim(paste(wdname, "/CNdata_tanguro.txt",sep = ""))
attach(data)
data$type <- c( "S", "S", "S", "S", "S", "S", "M", "M", "M", "M", "M", "M",  "F", "F", "F", "F", "F", "F")
data


# function for error bars
# http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


# calc the averages for each site
Amostrafac <- as.factor(Amostra); Amostra; Amostrafac; 
is.factor(Amostrafac) #just to make sure it's categorical
Cmeanssite <- tapply(percent_Carbono, Amostra, mean)
Cstdsite <- tapply(percent_Carbono, Amostra, sd)

# barplot by site
barsite <- barplot(Cmeanssite, ylab="Percent C", xlab="Site", main="Barplot")
error.bar(barsite,Cmeanssite, Cstdsite)
# save fig
png(filename="~/Dropbox/cso011_TanguroN2OLosses/Soil Data/percentC_bysite.png")
barsite <- barplot(Cmeanssite, ylab="Percent C", xlab="Site", main="Barplot")
error.bar(barsite,Cmeanssite, Cstdsite)
dev.off()
# save fig
png(filename="/Users/oconn568/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data/CN-ratio-percents/percentC_bysite.png")
barsite <- barplot(Cmeanssite, ylab="Percent C", xlab="Site", main="Barplot")
error.bar(barsite,Cmeanssite, Cstdsite)
dev.off()



# per type
typefac <- as.factor(data$type); data$type; typefac; 
is.factor(typefac) #just to make sure it's categorical
Cmeanstype <- tapply(percent_Carbono, data$type, mean)
Cmeanstype
Cstdtype <- tapply(percent_Carbono, data$type, sd)
Cstdtype

# barplot by land use type
bartype <- barplot(Cmeanstype, names.arg=c("Floresta", "Milho", "Soya"), ylab="Percent C", xlab="Land Use Type", main="Barplot", ylim=c(0,3.25))
error.bar(bartype,Cmeanstype, Cstdtype)
# save fig
png(filename="~/Dropbox/cso011_TanguroN2OLosses/Soil Data/percentC_byLUtype.png")
bartype <- barplot(Cmeanstype, names.arg=c("Floresta", "Milho", "Soya"), ylab="Percent C", xlab="Land Use Type", main="Barplot", ylim=c(0,3.25))
error.bar(bartype,Cmeanstype, Cstdtype)
dev.off()
# save fig
png(filename="/Users/oconn568/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data/CN-ratio-percents/percentC_byLUtype.png")
bartype <- barplot(Cmeanstype, names.arg=c("Floresta", "Milho", "Soya"), ylab="Percent C", xlab="Land Use Type", main="Barplot", ylim=c(0,3.25))
error.bar(bartype,Cmeanstype, Cstdtype)
dev.off()





