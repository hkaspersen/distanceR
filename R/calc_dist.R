#' Calculate distances from MLST/cgMLST alleles
#'
#' Function that calculates the distances between samples based on an allele matrix.
#'
#' @param allele_matrix The matrix holding the allele data. Row names should hold the sample IDs
#' @param method Character string specifying the clustering method to be used, default "average" (see \code{\link[stats]{hclust}})
#' @param metric Character string specifying the metric to be used, default "gower" (see \code{\link[cluster]{daisy}})
#' @param phylo Logical, if TRUE a phylo object will be calculated from the distance matrix
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom cluster daisy
#' @importFrom ape as.phylo
#'
calc_dist <- function(allele_matrix,
                      method = "average",
                      metric = "gower",
                      phylo = TRUE) {

  # calculate distances from matrix
  dist <- hclust(daisy(allele_matrix,
                       metric = metric),
                 method = method)

  # generate phylo object if phylo == TRUE
  if (phylo == TRUE) {
    tree <- as.phylo(dist)
    tree$tip.label <- rownames(allele_data)
    return(tree)
  } else {
    return(dist)
  }
}
