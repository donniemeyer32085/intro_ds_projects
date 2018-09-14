data(wine, package="rattle.data")
head(wine)
str(wine)

df <- scale(wine[-1]) 
head(df)


wssplot <- function (data, nc=15, seed=1234) {
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2:nc){
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

########################################examples of the apply function
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
######################################################################


#break up function

# number of rows in data - 1 (degrees of freedom)  * the sum of the variance of each column, 
#this is equal to the sqaure of sums within each group, it explain part of thhe variation in 
# the SST or the total sum of sqaures
wss <- (nrow(df)-1)*sum(apply(df,2,var))
wss

#for loop function, for each column of data 2 thru 15, (set seed for reproducible research) , 

wss[1] <- sum(kmeans(df, centers=1)$withinss)
wss[2] <- sum(kmeans(df, centers=2)$withinss)
wss[3] <- sum(kmeans(df, centers=3)$withinss)

for (i in 2:nc){
  set.seed(seed) 
  wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }




