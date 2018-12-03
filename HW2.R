# Setting working directory
setwd("E:/BA with R/HW2")
getwd()

#Reading the dataset
dungaree.df <- read.csv("dungaree.csv",header = TRUE)
View(dungaree.df)

#Set rownames
row.names(dungaree.df) <- dungaree.df[,1]
#Remove variable StoreID and total Sales
dungaree.df <- dungaree.df[, -1]


#Summary of data
summary(dungaree.df)
sapply(dungaree.df, class)
class(dungaree.df)

#Data Preprocessing 
#1. Removing Sales Total as mentioned in requirement
dungaree.df$SALESTOT <- NULL
#3 Checking NA values
any(is.na.data.frame(dungaree.df))


#checking for outliers and imputing outliers by winzorising
boxplot(dungaree.df$FASHION, ylab = "Fashion")
boxplot(dungaree.df$LEISURE,ylab="Leisure")
boxplot(dungaree.df$STRETCH,ylab="Stretch")
boxplot(dungaree.df$ORIGINAL,ylab="Original")



# Feature Scaling of the data
dungaree.df.norm <- sapply(dungaree.df, scale)
View(dungaree.df.norm)
# Again checking for missing values
any(is.na.data.frame(dungaree.df.norm))

#K-means clustering
library(NbClust)
set.seed(42)
devAskNewPage(ask=TRUE)
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")
# Perform k-means cluster analysis
fit.km <- kmeans(dungaree.df.norm, 7, nstart=25)
fit.km$size
# calcualte cluster centroidsfit.km$centers
fit.km$centers

#wssplot
wssplot <- function(data, nc=10, seed=42){
   wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
     set.seed(seed)
     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}
 wssplot(dungaree.df.norm,nc=10,seed=42)


#K-mean with 6-cluster
set.seed(42)
devAskNewPage(ask=TRUE)
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=6, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Perform k-means cluster analysis
fit.km <- kmeans(dungaree.df.norm, 5, nstart=25)
fit.km$size
fit.km$centers

#WSS plot
wssplot(dungaree.df.norm,nc=6,seed=42)



#Hclustering
pharma.df<-read.csv('Pharmaceuticals.csv',header = TRUE)
row.names(pharma.df) <- pharma.df[,1]
View(pharma.df)
any(is.na.data.frame(pharma.df))

pharma.df <- pharma.df[,-c(1,2,12,13,14)]
pharma.norm<-sapply(pharma.df, scale)
any(is.na.data.frame(pharma.norm))
View(pharma.norm)

set.seed(42)
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(pharma.norm, distance="euclidean", min.nc=2, max.nc=10, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")
d <- dist(pharma.norm)
fit.average <- hclust(d, method="average")
plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering")
clusters <- cutree(fit.average, k=3)
table(clusters)
aggregate(pharma.norm, by=list(cluster=clusters), median)
rect.hclust(fit.average, k=3)

#Kmeans with K = 6

set.seed(42)
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(pharma.norm, min.nc=2, max.nc=6, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")
# Perform k-means cluster analysis
fit.km <- kmeans(pharma.norm, 2, nstart=15)
fit.km$size
# calcualte cluster centroidsfit.km$centers
fit.km$centers
#WSS plot
wssplot(pharma.norm,nc=6,seed=42)
