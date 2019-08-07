#' Fix tip labels in trees
#'
#' Correct tip labels based on matching values to a separate data frame
#'
#' @param tree A tree object
#' @param df A data frame with two columns: ref - matching values to the tip points in the tree. id - the new tip values
#' @param sub_string The string to remove from the tip labels before matching values
#' @param tree_type The tree object type, either treedata or phylo
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
#' @importFrom dplyr left_join
#'
fix_tip_labels <- function(tree, df, sub_string, tree_type = "treedata") {

  if (tree_type == "phylo") {
    tree$tip.label <- sub(".ref", "", tree$tip.label)
    tree$tip.label <- sub(sub_string, "", tree$tip.label)

    refnames <- as.data.frame(tree$tip.label)
    names(refnames) <- "ref"

    refnames <- left_join(refnames, df, by = "ref")
    tree$tip.label <- refnames$id
  }

  if (tree_type == "treedata") {
    tree@phylo$tip.label <- sub(".ref", "", tree@phylo$tip.label)
    tree@phylo$tip.label <- sub(sub_string, "", tree@phylo$tip.label)

    refnames <- as.data.frame(tree@phylo$tip.label)
    names(refnames) <- "ref"

    refnames <- left_join(refnames, df, by = "ref")
    tree@phylo$tip.label <- refnames$id
  }

  return(tree)

}
