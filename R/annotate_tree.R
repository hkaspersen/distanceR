#' Annotate tree with metadata
#'
#' Add informative node colors and labels to existing tree. The function creates unique color palettes based on unique column values in supplied metadata file.
#'
#' @param tree A tree object to annotate
#' @param metadata_path The path to a tab-separated metadata file with information to annotate with. The first column in the file must be exact-matching IDs to the IDs used to create the tree
#' @param file Logical, should an R object be used as metadata file instead of a system file?
#' @param layout One of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight' (see \code{\link[ggtree]{ggtree}})
#' @param tree_color The color of the tree
#' @param line_width The width of the lines in the tree
#' @param color_variable A string representing the column name in the metadata file to plot as nodes on the tree, where each unique value in the column will be represented with a color
#' @param tippoint_size The size of the colored nodes in the tree
#' @param label_variable A string representing the column name in the metadata file to plot as labels on the tree
#' @param label_size The font size of the labels
#' @param label_offset The distance between the labels and the tree
#' @param palette_type Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3" (see \code{\link[RColorBrewer]{brewer.pal}})
#' @param own_palette If a named vector of colors are specified here, they are used instead of the automatically generated colors from colorBrewer
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggtree
#' @import dplyr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils read.table
#' @importFrom ggplot2 scale_color_manual
#'
annotate_tree <- function(tree,
                          metadata_path,
                          file = FALSE,
                          layout = "circular",
                          tree_color = "black",
                          line_width = 0.1,
                          color_variable,
                          tippoint_size = 1,
                          label_variable,
                          label_size = 4,
                          label_offset = 0.01,
                          palette_type = "Paired",
                          own_palette = NULL) {

  if (file == TRUE) {
    # Set data frame object
    metadata_df <- metadata_path
  } else {
    # Import metadata
    metadata_df <- read.table(
      metadata_path,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE)
  }

  # Set colors
  if (!is.null(own_palette)) {
    # User-defined color palette
    palette <- own_palette
  } else {
    # automatically create based on variables
    vars <- unique(metadata_df[[color_variable]])
    palette <- brewer.pal(length(vars), palette_type)
    names(palette) <- vars
  }

  if (layout == "circular") {
    annotated_tree <- ggtree(tree,
                             layout = layout,
                             color = tree_color,
                             size = line_width) %<+% metadata_df +
      geom_tippoint(aes(color = !! sym(color_variable)),
                    size = tippoint_size) +
      geom_tiplab2(aes(label = !! sym(label_variable)),
                   offset = label_offset,
                   size = label_size) +
      scale_color_manual(values = palette)
  }

  if (layout != "circular") {
    annotated_tree <- ggtree(tree,
                             layout = layout,
                             color = tree_color,
                             size = line_width) %<+% metadata_df +
      geom_tippoint(aes(color = !! sym(color_variable)),
                    size = tippoint_size) +
      geom_tiplab(aes(label = !! sym(label_variable)),
                  offset = label_offset,
                  size = label_size) +
      scale_color_manual(values = palette)
  }

  return(annotated_tree)

}
