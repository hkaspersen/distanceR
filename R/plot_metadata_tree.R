#' Plot tree with metadata and distances
#'
#' Function that plots a tree with associated metadata, in addition to summarizing SNP distances for given nodes
#'
#' @param tree The tree-file
#' @param metadata The data frame containing the metadata. First column must be the identifier
#' @param tiplab_var The column name of the data mapped to the tip labels
#' @param tippoint_var The column name of the data mapped to the tip points
#' @param heatmap_data The data frame used for mapping the heatmap to the tree. Row names must be identifiers
#' @param tree_palette Named vector with colors that match the uniqe values in the tip labels variable
#' @param heatmap_palette Named vector with colors that match the unique values in the tippoint variable
#' @param snpdists Data frame holding the SNP-distances, generated with snpdist
#' @param nodes A numeric vector that refers to the nodes in the tree to summarise SNP distances on
#' @param align_lab Should the tip labels be aligned?
#' @param rmv_str Suffix to remove from sample id's in the snpdists
#' @param label_offset Distance between the tip and the label
#' @param label_size Font size of label
#' @param point_size Size of the tip point
#' @param clade_offset Distance between the clade label and tree
#' @param heatmap_offset Distance between tree and heatmap
#' @param heatmap_width Width of the heatmap
#' @param colnames Should the heatmap have column names?
#' @param colnames_pos The position of the column names, "top" or "bottom"
#' @param colnames_angle The angle of the column names
#' @param xlim_max The max value for the x axis
#' @param treescale_x The x position of the treescale
#' @param treescale_y The y position of the treescale
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggtree
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 xlim
#'
plot_metadata_tree <- function(tree,
                               metadata,
                               tiplab_var,
                               tippoint_var,
                               heatmap_data,
                               tree_palette,
                               heatmap_palette,
                               snpdists,
                               nodes,
                               align_lab = TRUE,
                               rmv_str = "_filtered.fasta",
                               label_offset = NULL,
                               label_size = 4,
                               point_size = 4,
                               clade_offset = NULL,
                               heatmap_offset = NULL,
                               heatmap_width = NULL,
                               colnames = FALSE,
                               colnames_pos = NULL,
                               colnames_angle = 0,
                               xlim_max = NULL,
                               treescale_x = NULL,
                               treescale_y = NULL) {
  # Prep node data
  node_data <- prep_node_stats(tree,
                               snpdists,
                               nodes,
                               rmv_str)
  # Plot tree
  p <- ggtree(tree) %<+% metadata +
    geom_nodepoint(aes(subset = as.numeric(label) >= 95),
                   color = "grey20",
                   size = 1.5) +
    geom_tiplab(aes(label = !!sym(tiplab_var)),
                offset = label_offset,
                size = label_size,
                align = align_lab) +
    geom_tippoint(aes(color = !!sym(tippoint_var)),
                  size = point_size) +
    geom_cladelab(
      data = node_data,
      mapping = aes(node = node,
                    label = label),
      fontsize = 2.5,
      align = TRUE,
      offset = clade_offset
    ) +
    geom_treescale(x = treescale_x,
                   y = treescale_y) +
    scale_color_manual(values = tree_palette)

  # Add heatmap
  gheatmap(
    p,
    heatmap_data,
    width = heatmap_width,
    offset = heatmap_offset,
    colnames = colnames,
    colnames_angle = colnames_angle,
    colnames_position = colnames_pos
  ) +
    scale_fill_manual(values = heatmap_palette) +
    xlim(0, xlim_max)
}
