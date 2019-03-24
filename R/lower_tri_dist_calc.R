# calculates distances from cgMLST data and returns
# the lower triangular of the matrix, with (diag = FALSE)
# or without (diag = TRUE) the diagonal
lower_tri_dist_calc <- function(data, diag = TRUE, data_frame = FALSE) {
  dist <- as.matrix(daisy(data, metric = "gower"))
  if (diag == TRUE) {
    dist[upper.tri(dist, diag = TRUE)] <- NA
  } else {
    dist[upper.tri(dist, diag = FALSE)] <- NA
  }
  dist_vec <- as.vector(as.matrix(dist))
  dist_compl <- dist_vec[!is.na(dist_vec)]
  
  if (data_frame == TRUE) {
    dist_compl <- as.data.frame(as.matrix(dist))
  }
  return(dist_compl)
}