# calculate distances and generate basic
# dendrogram from chewBBACA cgMLST data
calc_dist <- function(path,
                      metric = "gower",
                      method = "average") {
  # read cgMLST data
  data <- read.table(path,
                     sep = "\t",
                     header = TRUE,
                     colClasses = "factor") %>%
    na_if("0") %>%
    column_to_rownames("FILE")

  # calculate distances and generate dendrogram
  tree <- as.phylo(hclust(daisy(data,
                                metric = metric),
                          method = method))
  return(tree)
}
