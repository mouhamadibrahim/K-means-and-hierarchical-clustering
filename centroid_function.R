centroid_function <- function(data_set, indexing, method){
  source('C:\\Users\\kali\\Desktop\\DA3 assignment\\centroid_distance.R')
  distance_matrix <- as.matrix(dist(data_set))
  distance_matrix[distance_matrix==0] <- NA
  clusters <- list()
  for (i in 1:nrow(data_set)) {clusters[[i]] <- i} 
  status_of_data <- c(rep(1,nrow(data_set)))
  matrix_distances <- matrix(NA,2*nrow(data_set)-indexing,2*nrow(data_set)-indexing)
  matrix_distances[1:nrow(distance_matrix),1:ncol(distance_matrix)] <- distance_matrix
  formated_matrix <- matrix_distances
  for (i in 1:(nrow(data_set)-indexing)) {
    minimum_clusters <- which(formated_matrix==min(formated_matrix, na.rm=TRUE), arr.ind=T)
    clusters[[nrow(data_set)+i]] <- c(clusters[[minimum_clusters[nrow(minimum_clusters),1]]],clusters[[minimum_clusters[nrow(minimum_clusters),2]]])  
    status_of_data[minimum_clusters[nrow(minimum_clusters),]] <- 0
    status_of_data[nrow(data_set)+i] <- 1
    matrix_distances <- centroid_distance(data_set, matrix_distances, clusters, method)  
    formated_matrix[minimum_clusters[nrow(minimum_clusters),],] <- NA
    formated_matrix[,minimum_clusters[nrow(minimum_clusters),]] <- NA
    formated_matrix[(nrow(data_set)+i),1:(nrow(data_set)+i)] <- matrix_distances[(nrow(data_set)+i),1:(nrow(data_set)+i)]
    formated_matrix[1:(nrow(data_set)+i),(nrow(data_set)+i)] <- matrix_distances[1:(nrow(data_set)+i),(nrow(data_set)+i)]
  }
  find_clusters <- clusters[status_of_data==1]
  finClu1 <- c()
  for (i in 1:length(find_clusters)) finClu1[find_clusters[[i]]] <- i
  cat('the number of cluster:\n',finClu1,'\n')
  return(list(finClu1, find_clusters))
}