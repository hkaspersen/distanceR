#' Import and calculate distances from cgMLST data
#'
#' Imports the "cgMLST.tsv" generated with chewBBACA in the correct format for distance calculation.
#'
#' @param path Full path to the cgMLST.tsv file, directly from chewBBACA.
#' @param metric Character string specifying the metric to be used, default "gower" (see \code{\link[cluster]{daisy}})
#' @param method The agglomeration method to be used, default "average" (see \code{\link[stats]{hclust}})
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom cluster daisy
#' @importFrom ape as.phylo
#' @importFrom tibble column_to_rownames
#' @importFrom utils read.table
#' @importFrom stats hclust
#'
calc_tree <- function(path,
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
