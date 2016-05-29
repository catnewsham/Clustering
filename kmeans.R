# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already
install.packages("RGtk2")
install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df<-subset(wine, select = -c(Type))
df<-scale(df)
head(df)
str(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 
library(NbClust)
wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)
#
# Exercise 2:
#   * How many clusters does this method suggest?
#between 3-4
#   * Why does this method work? What's the intuition behind it?
#loweing the SSE in each cluster means that the clusters are more similar. Perfect would be 0 SSE, but
# this would be overfitting. A good compromise is a low SSE in a 'good' (based on the problem) number 
#clusters where the cluslters contain enough data points to be accurate.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
k=3
set.seed(1)
fit.km <- kmeans(df, centers=k, nstart=25)

str(fit.km)
fit.km$size

clusters<-fit.km$cluster

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
aggregate(df,by=list(fit.km$cluster),FUN=mean)
table(wine$Type, fit.km$cluster)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster) 
clusplot(df, fit.km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
 
clusplot(wine, fit.km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#good clustering
getwd()