#' Calculate distances from cgMLST alleles
#'
#' Imports the cgMLST.tsv file from chewBBACA and calculates distances between samples. Outputs a distance matrix.
#'
#' @param path Full path to the cgMLST.tsv file, directly from chewBBACA
#' @param metric Character string specifying the metric to be used, default "gower" (see \code{\link[cluster]{daisy}})
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom cluster daisy
#' @importFrom utils read.table
#' @importFrom tibble column_to_rownames
#'
calc_dist <- function(path,
                      metric = "gower") {
  # read cgMLST data
  data <- read.table(path,
                     sep = "\t",
                     header = TRUE,
                     colClasses = "factor") %>%
    na_if("0") %>%
    column_to_rownames("FILE")

  # calculate distances
  dist <- as.matrix(daisy(data, metric = metric))

  return(dist)
}
