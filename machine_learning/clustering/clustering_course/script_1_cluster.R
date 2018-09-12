library(tidyverse)
setwd("/home/donniemeyer32085/git/springboard_projects/machine_learning/clustering/clustering_course/")

### Recommendations worth a million: Intro to clustering ###
netflix <- read.table("movie.txt", header = FALSE, sep = "|", quote = "\"")

#add col names
colnames(netflix) <- c("ID", "title", "release_date", "video_release_date", "IMDB", "unkown", "action", "advebture", "animation", "childrens", "comedy", "crime", "documentary", "drama", "fantasy", "filmnoir", "horror", "musical", "mystery", "romance", "scifi", "thriller", "war", "western")

netflix$ID <- NULL
netflix$release_date <- NULL
netflix$video_release_date <- NULL
netflix$IMDB <- NULL

#remove duplicate entries
netflix <- unique(netflix)

#make hierarchical clusters, two steps, 1. compuste distances betwen all data points, then cluster the points
distances = dist(netflix[2:20], method = "euclidean")
cluster_netflix <- hclust(distances, method = "ward.D")
plot(cluster_netflix)


cluster_group <- cutree(cluster_netflix, k = 10)

tapply(netflix$action, cluster_group, mean)
tapply(netflix$romance, cluster_group, mean)

subset(netflix, title == "Men in Black (1997)")

cluster_group[257]

cluster_2 <- subset(netflix, cluster_group == 2)
head(cluster_2, 10)
