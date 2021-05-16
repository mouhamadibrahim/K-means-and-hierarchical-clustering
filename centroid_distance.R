centroid_distance <- function(X, distance_of_matrix_clusters, clusters, method_used){
  total_number_of_clusters <- length(clusters)
  latest_clusters_value <- clusters[[length(clusters)]]
  if(method_used == 'centroid'){
    for (index in c(1:(total_number_of_clusters-1))[-latest_clusters_value]) {
      distance_of_matrix_clusters[total_number_of_clusters,index] <- abs(mean(X[clusters[[index]],])-mean(X[latest_clusters_value,]))
      distance_of_matrix_clusters[index,total_number_of_clusters] <- abs(mean(X[clusters[[index]],])-mean(X[latest_clusters_value,]))
    }
    return(distance_of_matrix_clusters)
  }
  
}