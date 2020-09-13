#CLEARING ENVIRONMENT
rm(list = ls())

# LOAD PACKAGE
library(kknn)
library(class)
library(ggplot2)

# DATA LOADING
data <- read.table('/Users/pc/iCloudDrive/EDX/Introduction to Analytics Modelling/Fall2020hw2/data 4.2/iris.txt', header = T,stringsAsFactors = T) 

# QUICK LOOK FOR LOADED DATA
head(data)

#SETTING RANDOM NUM GENERATOR SEED FOR REPRODUCIBILITY
set.seed(1)

#DROP SPECIES FOR MODELLING 
data.features = data
data.features$Species <- NULL
head(data.features)

#MAP SPECIES INTO NUMBERS
convert <- c("setosa"=1,"versicolor"=2,"virginica"=3)
numcat <- convert[data$Species]
head(numcat)

#NORMALIZE FEATURES FOR K-MEANS
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_n <- as.data.frame(lapply(data.features, normalize))
head(data_n)

#USING NORMALIZED FEATURES TO PERFORM CLUSTERING - ALL FEATURES USED
m2 <- kmeans(data_n,centers= 2, nstart = 5)
m3 <- kmeans(data_n,centers= 3, nstart = 5)
m4 <- kmeans(data_n,centers= 4, nstart = 5)


table(m2$cluster, data$Species)
table(m3$cluster, data$Species)
table(m4$cluster, data$Species)

accuracym2 = sum(m2[[1]] == numcat)
accuracym2
accuracym3 = sum(m3[[1]] == numcat)
accuracym3
accuracym4 = sum(m4[[1]] == numcat)
accuracym4

#USING NORMALIZED FEATURES TO PERFORM CLUSTERING - ONLY (Petal Length & Width) USED

m2 <- kmeans(data_n[,3:4],centers= 2, nstart = 1)
m3 <- kmeans(data_n[,3:4],centers= 3, nstart = 1)
m4 <- kmeans(data_n[,3:4],centers= 4, nstart = 1)


table(m2$cluster, data$Species)
table(m3$cluster, data$Species)
table(m4$cluster, data$Species)


accuracym2 = sum(m2[[1]] == numcat) / nrow(data)
accuracym2
accuracym3 = sum(m3[[1]] == numcat) / nrow(data)
accuracym3
accuracym4 = sum(m4[[1]] == numcat) / nrow(data)
accuracym4


ggplot(data, aes(Petal.Length, Petal.Width, color = m3$cluster)) + geom_point(color = m3$cluster)


