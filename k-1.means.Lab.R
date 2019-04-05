######################################
# section 4.2.3
######################################

# Using R to Perform a K-Means Analysis

# install packages, if necessary
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

# Import the student grades
grade_input = as.data.frame(read.csv("~/LIS4805_Predictive_Analytics/grades_km_input.csv"))

##Q1: Excecute "nrow()" to find the sample size of "grade_input" object. How many students are included in this dataset?
nrow(grade_input)
# 620 students are included in the dataset. 

kmdata_orig = as.matrix(grade_input[,c("Student","English", "Math","Science")])
kmdata <- kmdata_orig[,2:4]
kmdata[1:10,]
#Q2: What is the English score of the the seventh student? You must solve this by writing r commends. Hint: Subset the Student ID with value 7 or subset the 7th row.
kmdata[7,]
# English 100, Math 96, Science 97

# Calculate WSS of each of the 15 clusters.
wss <- numeric(15) 
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 

set.seed(1)
km = kmeans(kmdata,3, nstart=25)
km
#Q3:How many clusters did you just create in "km = kmeans(kmdata,3, nstart=25)"? **use ?kmeans() to learn about this function. #What is the reason to produce this number of K based on the text book explanation?
# Three clusters were created. 
c(wss[3] , sum(km$withinss) )

# Prepare the student data and clustering results for plotting
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1= ggplot(data=df, aes(x=English, y=Math, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g2 =ggplot(data=df, aes(x=English, y=Science, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=English,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) + 
  list(
    geom_point(),
    geom_point(data=centers, aes(x=Math,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE),
    NULL
  )

tmp = ggplot_gtable(ggplot_build(g1)) 

grid.arrange(g1, g2, g3, ncol=1)

##Q4: Answer this question based on the three graphs you just created.How many students are strugling with science 
#(aquired grades lower than 80) but excel in English (higher than or equal to 90)?
# 3 students. 

