# This code shows a simple example of how to import SPSS
# files from R and how to do K-means cluster analysis. 
# Data correspond to a small sample of
# different cars' makes and models with their corresponding
# fatures: acceleration, horsepower, moment, etc.
#
#  IMPORTANT COMMENT: The data set used is very small. This code has the 
#  main intention of showing how to import SPSS files into R and how 
#  to do basic K-means cluster analysis.
#
# The rough sequence of this code is:
#
#  1. Imports the SPSS file "Cars.sav"
#  2. Loads data into a data frame for maniuplation
#  3. Prints same data into a csv file 
#  4. Performs a cluster analysis for different 
#     cluster sizes and plots vs. cluser sizes
#  5. Plot generated in 4 is printed to plot1.png
#  6. Some labels are added to plot1.png and the updated
#     version is printed to plot2.png
#  7. For three clusters, three plots are generated to observe
#     the clustering in terms of pairs of features.
#     These plots are saved to plot3.png, plot4.png and plot5.png.
#  8. A general cluster plot is generated (for 3 clusters)
#     and is printed to plot6.png
#
# Some line executions are only suggestions and are commented
# for that reason.
#
# ============================================================
# We invoke "foregin"; package to read SPSS files...
require(foreign)
# We read the .sav (SPSS) file. 
# IMPORTANT: The file "Cars.sav" has to be in the same folder 
# where this code is being run.
# The data are saved into the cars_csv data frame
cars_csv<-read.spss("Cars.sav")
#
#names(cars_csv)
# We visualize the data frame
#View(cars_csv)
# We save, for the sake of further comparisons, the data frame into a 
# csv file: cars.csv
write.csv(cars_csv, file = "cars.csv")
#
# HERE IS WHERE THE ACTUAL R ANALYSIS BEGINS
# We read data from the .csv file and save it to MyData data frame
CarsDF<-read.csv("cars.csv")
# We make sure there is nothing in the "MyData" data frame
rm(MyData)
# We copy the data file into the MyData data frame.
MyData<-CarsDF
# We check the data
View(MyData)
# We have an extra "X" column; we get rid if it
MyData$X<-NULL
#
#View(MyData)
# To do cluster analysis we also get rid of the "name" column
# so that we end up having only numerical values
MyData$Name<-NULL
# We check we only have numerical values
View(MyData)
# We attach the data
attach(MyData)
# And check the final names we have
names(MyData)
#
# We do some basic descriptive statistics
summary(MyData)
# and get the correlation matrix
cor(MyData)
# In case there are "NA" values, we get rid of them
MyData <- na.omit(MyData) 
# Standarize variables in order to make a reliable 
# K-cluster analysis
MyData <- scale(MyData) 
#
View(MyData)
# We do some basic descriptive statistics
summary(MyData)
# and get the correlation matrix
cor(MyData)
# The following lines are coded in order to SHOW the number of
# clusters suggested by the generated plot.
# A plot of the within groups sum of squares by number of clusters
# extracted can help determine the appropriate number of clusters.
# This analysis looks for a bend in the plot. 
# We make sure the wss variable is free
rm(wss)
wss <- (nrow(MyData)-1)*sum(apply(MyData,2,var))
# 
for (i in 1:10){
    # We determine the within sums for evey cluser analysis
    # executed (in this case, with i centers) and
    # store it in the ith component of wss
    wss[i] <- sum(kmeans(MyData,centers=i)$withinss)
}
# We plot the value of within-cluster sum of squares
# versus the number of clusters.
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# We save the plot to file "plot1.png"
dev.copy(png, file="plot1.png", height=480, width=480)
## and switch off the device
dev.off()
#
# We plot, once again, the value of within-cluster sum of squares
# versus the number of clusters. This time we add the value
# of the sum of squares as a label to each point in the graph...
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# We add values of points to make the plot clearer
# For doing so wee need the library "calibrate"
library(calibrate)
# We do the actual labeling
textxy(1:10, wss, signif(wss,digits=5), cex=0.8, xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# We save the plot to file "plot2.png"
dev.copy(png, file="plot2.png", height=580, width=580)
## and switch off the device
dev.off()
# From the plot generated we notice there is a possible "elbow" at 4 or 5.
# We however begin with 3 clusters to see exemplify the analysis.
# A similar analysis cane be done for more clusters.
# ========================
# We now do K-means for t clusters, where t will be chosen in 
# what follows.
# We choose three clusters
t <-3
# We make sure the "ClusterResults" data frame is "free"
rm(ClusterResults)
ClusterResults<-kmeans(MyData,t)
#ClusterResults
#names(ClusterResults)
# We print out the number of elements in each cluster
ClusterResults$size
# We recall the names of the data set analysed
names(CarsDF)
#table(CarsDF$displacement,ClusterResults$cluster)
# We generate a table that displays to what cluster each car
# belongs to.
table(CarsDF$Name,ClusterResults$cluster)
# We generate a plot to see a "clustering" in terms of horsepower and displacement
plot(CarsDF[c("displacement","horsepower")],col=ClusterResults$cluster)
# We save the plot to file "plot3.png"
dev.copy(png, file="plot3.png", height=500, width=500)
## and switch off the device
dev.off()
# We generate a plot to see a "clustering" in terms of displacement and weight
plot(CarsDF[c("displacement","weight")],col=ClusterResults$cluster)
# We save the plot to file "plot4.png"
dev.copy(png, file="plot4.png", height=500, width=500)
## and switch off the device
dev.off()
# We do something similar for acceleration and speed...
plot(CarsDF[c("acceleration","speed")],col=ClusterResults$cluster)
# We save the plot to file "plot5.png"
dev.copy(png, file="plot5.png", height=500, width=500)
## and switch off the device
dev.off()

# We now do K-means cluster analysis and aggregate 
# results of the clustering to the orgiinal data
# so that they show to what cluster each car belongs to.
rm(fit)
# We get cluster means and store the 3 cluster solution
# into fit
fit <- kmeans(MyData, 3) 
# We aggregate..
aggregate(MyData,by=list(fit$cluster),FUN=mean)
# And we append cluster assignment to each element of the table
rm(MyDataAggr)
#MyDataAggr <- data.frame(MyData, fit$cluster)
MyDataAggr <- data.frame(CarsDF, fit$cluster)
# We get rid of "X" column
MyDataAggr$X <- NULL
# View the final aggregated data
View(MyDataAggr)
# And print it out to a csv file "CarsClustered.csv"
write.csv(MyDataAggr,"CarsClustered.csv")
#
# ===================================================
# We now recall we have done clustering with 3 clusters and 
# proceed to do a cluster plot against 1st 2 principal components
# vary parameters for a more readable graph.
# For this we need the "cluster" library...
library(cluster) 
# We generate the cluser plot itself...
clusplot(MyData, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# We save the plot to file "plot6.png"
dev.copy(png, file="plot6.png", height=500, width=500)
## and switch off the device
dev.off()

# We can also show Centroid Plot against first two discriminant 
# functions.
#library(fpc)
#plotcluster(MyData, fit$cluster)
