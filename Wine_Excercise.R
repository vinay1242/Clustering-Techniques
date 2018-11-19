# read and understand the data
# validate for null / NA values 
# identify categorical / factor variables and convert to numeric
# Identify attributes with near zero variance
# normalize the data 
# determine columns with high correlation and remove 
# create cluster using k-means 
# identify elbow and determine k value 
# interpret results..

#Understanding data
#         b. Data pre-processing
#               i)   Creating dummy variables
#               ii)  Removing columns with zero-variance
#               iii) Scaling and centering data
#               iv)  Discover and remove higly correlated variables
#                v)  Box-cox transformation
#         c. Segmentation using k-means
#         d. Plotting results




rm(list = ls()) ; gc()
library(dplyr)    # For data manipulation ,filtering etc
library(caret)    # For dummy variables, nearZeroVar(), findCorrleation()
library(ggplot2)  # For plotting

library(factoextra)     # get_clust_tendency() assesses hopkins stat
library(clustertend)
library(vcd)     # For association plots
library(readr)   # read_csv() is fast read facility
library(mclust)
library(NbClust)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)


# Set working directory

setwd("C:\\Users\\vikandul\\Desktop\\BIG DATA - FORE SCHOOL COURSE\\Class Notes and Supporting documentation\\Week_06022018\\clustering\\Excercise")

# read .csv file user read.csv and store it to wine_data data frame.
wine_data<- read.csv("winequality.csv", header=TRUE)

#udertsand the data by retrieving few observations of the data usinf head() or review complete data set using View() function.
head(wine_data)
View(wine_data)

# check how many observations and attributes exists in the data set 
dim(wine_data)

# check and derive attribute/ column names of wine data set
names(wine_data)

# determine data types of each attribute of the wine data set
str(wine_data)

# we can see that out of 14 attribtues 11 are of type numeric and 2 attributes are of type integer and one is a factor variable with 2 levels

# derive summary statistics for individual attributes if the wine data set using summary()) function
summary(wine_data)


# Lets try to plot some graphs to understand the data better.

# histogram to plot the distribution of continuous attributes of wine data set using a histogram :
#Here are the following attributes considered to plot histogram : 
#attributes fixed.acidity, volatile.acidity, citric.acid, residual sugar  of wine 

par(mfrow=c(2,2)) 
ggplot(wine_data, aes(x=fixed.acidity, fill =color))+geom_histogram(alpha=.8, position="dodge", binwidth = 1)
ggplot(wine_data, aes(x=volatile.acidity, fill =color))+geom_histogram(alpha=.8, position="dodge", binwidth = 1)
ggplot(wine_data, aes(x=citric.acid, fill =color))+geom_histogram(alpha=.8, position="dodge", binwidth = 1)
ggplot(wine_data, aes(x=residual.sugar, fill =color))+geom_histogram(alpha=.8, position="dodge", binwidth = 1)






# for each quality level plot  chlorides levels for 2 types of wines(red and white) distinguish by shape 

p <- ggplot(wine_data, aes( quality,chlorides, shape = (color)))
p + geom_point(aes(colour = (color)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)+ labs(x="Wine Quality level", y= "chlorides levels")


# for each quality level plot  free.sulfur.dioxide levels for 2 types of wines(red and white) distinguish by shape 

q <- ggplot(wine_data, aes( quality,free.sulfur.dioxide, shape = factor(good)))
q + geom_point(aes(colour = factor(good)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)+ labs(x="Wine Quality level", y= "free Sulpher Dioxide levels")


my.color<- c("red", "green", "indigo", "violet","blue","grey","orange")

q <- ggplot(wine_data, aes(color, alcohol, fill= quality))
q+geom_boxplot(outlier.colour = "red", outlier.shape = 1,  colour = "#3366FF")+facet_grid(.~quality)+ labs(x = "Wine Type(Color)",
                                                                                                           y = "Alcohol levels ") + 
  theme(axis.title.x = element_text(size=15, colour="black"),
        axis.title.y = element_text(size=15, colour="black"),
        axis.text.x = element_text(size=10, colour ="black"),
        axis.text.y = element_text(size=10, colour ="black")
        )



  ggplot(wine_data, aes(x = as.factor(color))) +
  geom_density(aes(fill = as.factor(quality)), alpha = 0.2)+  labs(x="Wine Color ", y= "Density")





  levels(wine_data$color) <-c( 1, 0)  
  wine_data$color<-as.numeric(wine_data$color) -1
  View(wine_data)
  str(wine_data)
  
  
  model_wine_data<- wine_data[,-14]
  norm_model_data <- sapply(model_wine_data[,c(1,4,6,7,11,12)], function(x) (x - min(x))/(max(x) - min(x)))
  norm_model_data <- data.frame(norm_model_data)    # norm_data is a 'matrix'
  head(norm_model_data)
  summary(norm_model_data)  
  model_wine_data$fixed.acidity  <- norm_model_data$fixed.acidity
  model_wine_data$residual.sugar<- norm_model_data$residual.sugar
  model_wine_data$free.sulfur.dioxide <- norm_model_data$free.sulfur.dioxide
  model_wine_data$total.sulfur.dioxide<- norm_model_data$total.sulfur.dioxide
  model_wine_data$alcohol<- norm_model_data$alcohol
  model_wine_data$quality<- norm_model_data$quality
  
  head(model_wine_data)  
  total_wsos<- c()
  for (i in 1:10)
  {
    wine_cluster<- kmeans(model_wine_data, centers=i)
    total_wsos[i]<- wine_cluster$tot.withinss
  } 
  total_wsos
  plot(x=1:10,                         # x= No of clusters, 1 to 15
       y=total_wsos,                      # tot_wss for each
       type="b",                       # Draw both points as also connect them
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")  
  
  
 w_cluster = kmeans(model_wine_data, 2, iter.max = 140 , algorithm="Lloyd", nstart=100)
  w_cluster
  
  #Create a dataframe of k (2) rows with only one column
  #  column name is 'id' 
  cluster_mean<-data.frame(id=c(1:k))   # One row to describe each cluster
  cluster_mean
  
  w_cluster$betweenss / w_cluster$totss
  
  #K-Means always try to find mimimun sum of square distances between clusters and maximum sum of square distance between clusters
  
  w_cluster$withinss / w_cluster$betweenss
  
 distance <- get_dist(model_wine_data)
  
  
    View(w_cluster$centers)
  
  clusplot(model_wine_data, w_cluster$cluster, color=TRUE, shade=TRUE, 
           labels=2, lines=0)
  
  
  ?clusplot
  
  #silhoutte coefficients
  
  sil_coeff <- kmeans(model_wine_data, 2)

  dis <- dist(model_wine_data)^2  
  sil = silhouette (sil_coeff$cluster, dis)
  windows()
  plot(sil) 
  
  View(sil)
  
  fviz_nbclust(model_wine_data, kmeans, method = "silhouette")
  
  
  list.files(path = "../input")
  
  
    