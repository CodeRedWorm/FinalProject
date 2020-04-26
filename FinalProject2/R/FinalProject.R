# Final Project

# I'm not creative enough to think of something unique and original to do
# for this project. So with that said I grabbed essentially a random dataset
# from the vincentarelbundock GitHub. It's the same source for datasets that was
# used in Module #9.

# The dataset is comprised of 619 students and their individual scores
# in Math, Science and English (scores are out of 100).

# I wasn't really sure at first what to do with this dataset, but ultimately I
# decided to do a K-Means clustering analysis that will showcase the student's
# performance in 2 subjects at the same time.

# K-Means is just going to be abbreviated as km from here on out. In general km
# clustering allows for a convenient way to designate elements within a data set
# as belonging to a particular grouping.

# This method of classification can help show structures of the data that otherwise
# may have gone unnoticed. All this will hopefully make some sense when looking at 
# the charts.

# Install these packages
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

# Import the dataset and create a matrix
grade_input = as.data.frame(read.csv("FinalProject.csv"))
kmdata_orig = as.matrix(grade_input[,c("Student","English", "Math","Science")])
kmdata <- kmdata_orig[,2:4]

# Perform a km clustering using With-in-Sum-of-Squares (WSS) to figure out how many
# clusters should be used.
wss <- numeric(15) 
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)

# Create plot and determine what # the "elbow" is.
plot(1:15, wss, type="b", xlab="# of Clusters", ylab="WSS") 

# The "elbow" looks like it's at 3, so that's what I went with.
km = kmeans(kmdata,3, nstart=25)
km
c(wss[3] , sum(km$withinss))

# Get the student data and clustering results for plotting
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers = as.data.frame(km$centers)

# Plot #1 comparing English and Math.
# Put the cluster legend on the right of each plot.
g1 = ggplot(data=df, aes(x=English, y=Math, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

# Plot #2 comparing English and Science.
g2 = ggplot(data=df, aes(x=English, y=Science, color=cluster)) + 
  geom_point() + 
  geom_point(data=centers, aes(x=English,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

# Plot #3 comparing Math and Science.
g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster)) + 
  list(
    geom_point(),
    geom_point(data=centers, aes(x=Math,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE),
    NULL
  )

tmp = ggplot_gtable(ggplot_build(g1)) 

# Create plots #1, #2 and #3.
grid.arrange(g1, g2, g3, ncol=1)

# Woo-hoo!
