# Calculate distances from cgMLST data and
# return distance matrix
calc_dist <- function(path,
                      metric = "gower") {
  # read cgMLST data
  data <- read.table(path,
                     sep = "\t",
                     header = TRUE,
                     colClasses = "factor") %>%
    na_if("0") %>%
    column_to_rownames("FILE")

  # calculate distances and generate dendrogram
  dist <- as.matrix(daisy(data, metric = metric))

  return(dist)
}
