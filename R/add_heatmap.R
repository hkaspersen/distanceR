#' Add heatmap to tree objects
#'
#' Annotates tree objects with a heatmap, f. ex. representing presence/absence of genes.
#'
#' @param tree A tree object to annotate
#' @param heatmap_data A file containing the data to add heatmap of. The first column need to hold the same ID's used to create the tree object. The remaining columns will be used in the heatmap. Each unique value in the whole file, except the ID column, will be given a unique color
#' @param perc_open How many percent the tree is opened. This is used to fit the heatmap labels in the gap produced
#' @param perc_rotate How many percent the tree is rotated. This is to get the heatmap labels on the top pf the tree
#' @param heatmap_offset The distance between the tree and the heatmap
#' @param heatmap_width The width of the heatmap
#' @param colnames_offset The distance between the heatmap column names and the heatmap
#' @param colnames_position The location of the heatmap column names
#' @param font_size Font size of the heatmap column names
#' @param palette_type Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3" (see \code{\link[RColorBrewer]{brewer.pal}})
#' @param own_palette If a named vector of colors are specified here, they are used instead of the automatically generated colors from colorBrewer
#'
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @importFrom ggtree rotate_tree
#' @importFrom ggtree open_tree
#' @importFrom ggtree gheatmap
#' @importFrom utils read.table
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom RColorBrewer brewer.pal
#'
add_heatmap <- function(tree,
                        heatmap_data,
                        perc_open = 10,
                        perc_rotate = 95,
                        heatmap_offset = 0.05,
                        heatmap_width = 0.5,
                        colnames_offset = 0,
                        colnames_position = "top",
                        font_size = 5,
                        palette_type = "Paired",
                        own_palette = NULL) {
  # Import heatmap data
  heatmap_df <- read.table(
    heatmap_data,
    sep = "\t",
    header = TRUE,
    row.names = 1,
    colClasses = "factor"
  )

  # Set colors
  if (!is.null(own_palette)) {
    # User-defined color palette
    palette <- own_palette
  } else {
    # Create color palette
    vars <- unique(unlist(heatmap_df))
    palette <- brewer.pal(length(vars), palette_type)
    names(palette) <- vars
  }

  # Open tree
  open_tree <- rotate_tree(open_tree(tree,
                                     perc_open),
                           perc_rotate)

  # Add heatmap
  annot_tree <- gheatmap(
    open_tree,
    heatmap_df,
    offset = heatmap_offset,
    width = heatmap_width,
    colnames_offset_y = colnames_offset,
    colnames_position = colnames_position,
    font.size = font_size
  ) +
    scale_fill_manual(values = palette)

  return(annot_tree)
}
