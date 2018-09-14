# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

################################################# Exercise 0: Install these packages if you don't have them already
#install.packages(c("cluster", "rattle.data","NbClust"))
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)
str(wine)


###################################################### Exercise 1: Remove the first column from the data and scale
# it using the scale() function, scale function standerdizes the data, whic is vital for clustering
df <- scale(wine[-1]) 
head(df)


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.


# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 
wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }
wssplot(df)


#Since the variables vary in range, they are standardized prior to clustering (#1). 
#Next, the number of clusters is determined using 


###################################################################################################### Exercise 2:

#   * How many clusters does this method suggest? 
#     We have a distinct drop from clusters 1 to 3. A 3 cluster solution may be a good fit on the data. 
# Plot the within groups sums of squares vs. the number of clusters extracted. 
# The sharp decreases from 1 to 3 clusters (with little decrease after) suggest a 3-cluster solution.

#   * Why does this method work?  *What's the intuition behind it?
#     There is not much improvment after three clusters, ie the variation within groups has been reduced
#     so therefore the observations in each group are closely related. Going from 3 clusters to four clusters 
#     does not reduce the ssw enogh to justify adding another cluster. 


#  * Look at the code for wssplot() and figure out how it works
#break up function
# number of rows in data - 1 (degrees of freedom)  * the sum of the variance of each column, 
#this is equal to the sqaure of sums within each group, it explain part of thhe variation in 
# the SST or the total sum of sqaures
wss <- (nrow(df)-1)*sum(apply(df,2,var))
wss
#break up for loop function, for each column of data 2 thru 15, (set seed for reproducible research) , 
wss[1] <- sum(kmeans(df, centers=1)$withinss)
wss[2] <- sum(kmeans(df, centers=2)$withinss)
wss[3] <- sum(kmeans(df, centers=3)$withinss)

#then plot the wss vector against the assigned number of clusters

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest? 
# Three clusters
# Recommended number of clusters using 26 criteria provided by the NbClust package

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3, nstart=25)
fit.km

fit.km$centers #cluster centroids 

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean) #view centroid means in there original metric

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type)

ct.km <- table(wine$Type, fit.km$cluster)
ct.km
#The k-means clustering clustered all of wine type 1 and 3 into there repective group
#It did well with wine type 2 as well
library(flexclust)
randIndex(ct.km)
#a value of 0.89, the agreement type is strong between the two partitions

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
#The kmeans clustering is validated once gain visualy. 
#This strengthens the argument that the k means clustering worked.
library(cluster)
clusplot(df, fit.km$cluster, main = 'Cusplot')
