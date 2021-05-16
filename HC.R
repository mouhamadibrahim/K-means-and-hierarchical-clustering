#Task-1
#reading the labels fie
label <- read.table('C:\\Users\\kali\\Desktop\\DA3 assignment\\label.txt',strip.white = TRUE)

#getting number of rows
number_of_rows <- nrow(unique(label))

#converting list data to double so they can be used later
label <- as.numeric(unlist(label))


#setting seed to MMDD
set.seed(0804)

#reading data file
data=read.table('C:\\Users\\kali\\Desktop\\DA3 assignment\\nci.data.txt', header = FALSE, sep = "", dec = ".")

#transpose data object 
data=t(data)

#normalizing data
data <- scale(data)

#distances function to be used
distances <- function(data,method_used)
{ 
  
  if (method_used == 'centroid'){
    centroid_algorithm <- centroid_function(data_set, number_of_rows, 'centroid')
  }
  else{
    #we have different methods and in here we are deciding between average,single,complete
    switch(method_used,'average'={method_used='mean'},'single'={method_used='min'},'complete'={method_used='max'})
    
    #getting the distance 
    matrix_distances_value <- as.matrix(dist(data))
    
    #setting 0 in here as inf in the matrix distance we are working with
    diag(matrix_distances_value) <- Inf
    
    Number_of_data <- dim(data)[1]
    
    #setting the column names and rows
    colnames(matrix_distances_value) <- -(1:Number_of_data)
    rownames(matrix_distances_value) <- -(1:Number_of_data)
    
    diag(matrix_distances_value) <- Inf
    merge_data_value <- matrix(0, Number_of_data-1, 2)
    height_of_values <- vector(length = Number_of_data-1)
    
    for (i in 1:(Number_of_data-1)) { 
      column_names <- colnames(matrix_distances_value)
      #getting the index of the distance which is minimal
      minimum_distance_i <- which(matrix_distances_value == min(matrix_distances_value), arr.ind = TRUE)[1,,drop=FALSE]
      
      #this is the value of the actual pair a distance that is minimal
      height_of_values[i] <- min(matrix_distances_value) 
      
      #merging the row and column position 
      merge_data_value[i,] <- as.numeric(column_names[minimum_distance_i])
      #getting the clusters
      clustering <- c(minimum_distance_i, which(column_names %in% column_names[minimum_distance_i[1, column_names[minimum_distance_i] > 0]]))
      colnames(matrix_distances_value)[clustering] <- i
      
      #merging the closest pairs
      closest_pair <- apply(matrix_distances_value[minimum_distance_i,], 2, method_used)
      matrix_distances_value[min(minimum_distance_i),] <- closest_pair
      matrix_distances_value[,min(minimum_distance_i)] <- closest_pair
      
      #setting the minimum distance of the pair to infinitie so we dont have to use it
      diag(matrix_distances_value) <- Inf
      
      #the last iteration all elements will be set to Inf
      matrix_distances_value[max(minimum_distance_i),] <- Inf
      matrix_distances_value[,max(minimum_distance_i)] <- Inf
    }
    
    #getting the clusters
    get_clusters <- list("merged_datapoints"=merge_data_value,"dendrogram_height"=height_of_values,"clustering"=clustering)
    return(cutree(get_clusters,14))
  }
}

#Task-2
#calling the function to apply it on the dataset
single_algorithm <- distances(data,'single')
average_algorithm <- distances(data,'average')
complete_algorithm <- distances(data,'complete')
centroid_algorithm <- distances(data,'centroid')

#creating plots for the methods used that we have here
plot(complete_algorithm,main="Complete",xlab="",sub="",cex=1,pch=20,col=complete_algorithm+1)
plot(average_algorithm,main="Average",xlab="",sub="",cex=1,pch=20,col=average_algorithm+1)
plot(single_algorithm,main="Single",xlab="",sub="",cex=1,pch=20,col=single_algorithm+1)


#Task-3
#checking the performance of the hierarchical agglomerative clustering using mclust
library(mclust)
cat("the single linkage similarity: ",adjustedRandIndex(label,single_algorithm))
cat("the average linkage similarity: ",adjustedRandIndex(label,average_algorithm))
cat("the complete linkage similarity: ",adjustedRandIndex(label,complete_algorithm))
cat("the centroid linkage similarity: ",adjustedRandIndex(label,centroid_algorithm))

#Task-4
#applying k-means algorithm
#perform k means clustering with k=14
set.seed(0804)
km.out <- kmeans(data, number_of_rows, nstart=20)

#assignments of the clusters 
km.out$cluster

#performance of the actual mpdel
km.out$tot.withinss
cat("the similarity of k-means: ",adjustedRandIndex(label,km.out$cluster))

#task-5 in here, trying different value of k
time_now <- Sys.time()
best_value_score <- 0
k_best_value <- 0
for (indexing in 1:number_of_rows)
{
  km.out <- kmeans(data, indexing, nstart=20)
  #assigning the cluster to the samples
  km.out$cluster
  #performance of the model
  km.out$tot.withinss
  if (best_value_score<adjustedRandIndex(label,km.out$cluster))
  {
    best_value_score <- adjustedRandIndex(label,km.out$cluster)
    k_best_value <- indexing
  }
}


cat("k best value to choose for k-means",k_best_value," with similarity score as",best_value_score)
cat("the time that took is::",Sys.time()-time_now,"to get the clusters")

#plot the data points with different colours for the clustering
plot(data, col=(km.out$cluster+1), main="clustering results", xlab="", ylab="", pch=20, cex=2)
