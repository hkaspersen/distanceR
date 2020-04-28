#' Plot a tree from a phylo object
#'
#' Function that plots a simple tree from a phylo object, with a treescale and tip labels
#'
#' @param phylo The phylo object to plot
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@protonmail.com}
#'
#' @export
#' @import ggtree
#' @importFrom ggplot2 scale_x_continuous
#'
plot_tree <- function(phylo) {
  ggtree(tree) +
    geom_treescale() +
    geom_tiplab(size = 3,
                align = TRUE) +
    scale_x_continuous(expand = c(0.3,0))
}
