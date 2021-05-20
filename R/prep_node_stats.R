#' SNP distance statistics for given node
#'
#' Function that identifies the tips below a given node, calculates SNP distance statistics for all isolates below the given node, and outputs a data frame that can be used to plot with ggtree
#'
#' @param tree Phylo, The tree
#' @param dists Data frame, The SNP distance matrix (from snp-dists)
#' @param nodes Numeric, vector of nodes of interest
#' @param sub_string The string that will be removed from the SNP distance matrix isolate names to match the tree tip labels
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @importFrom tidytree as_tibble
#' @importFrom funtools func_paste
#'
prep_node_stats <- function(tree, dists, nodes, sub_string) {
  tibble_tree <- as_tibble(tree)

  tiplist <- lapply(nodes, function(x) prep_node(tibble_tree, x)) %>%
    bind_rows()

  distify(dists) %>%
    mutate(isol1 = sub(sub_string, "", isol1),
           isol2 = sub(sub_string, "", isol2)) %>%
    left_join(tiplist, by = c("isol1" = "label")) %>%
    rename("node1" = node) %>%
    left_join(tiplist, by = c("isol2" = "label")) %>%
    rename("node2" = node) %>%
    filter(node1 == node2) %>%
    group_by(node1) %>%
    mutate(mean_val = round(mean(value),1),
           median_val = median(value),
           range = paste0(min(value), " - ", max(value))) %>%
    select(node1, mean_val, median_val, range) %>%
    summarise_all(list(func_paste)) %>%
    ungroup() %>%
    filter(!is.na(node1)) %>%
    mutate(label = paste0("Mean: ",
                          mean_val,
                          "\nMedian: ",
                          median_val,
                          "\nRange: ",
                          range)) %>%
    rename("node" = node1)
}
