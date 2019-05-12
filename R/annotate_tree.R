#' Annotate tree with metadata
#'
#' Add informative node colors and labels to existing tree. The function creates unique color palettes based on unique column values in supplied metadata file.
#'
#' @param tree A tree object to annotate
#' @param metadata The path to a tab-separated metadata file with information to annotate with. The first column in the file must be exact-matching IDs to the IDs used to create the tree. May also be a data frame.
#' @param layout One of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight' (see \code{\link[ggtree]{ggtree}})
#' @param tree_color The color of the tree
#' @param line_width The width of the lines in the tree
#' @param color_variable A string representing the column name in the metadata file to plot as nodes on the tree, where each unique value in the column will be represented with a color
#' @param tippoint_size The size of the colored nodes in the tree
#' @param label_variable A string representing the column name in the metadata file to plot as labels on the tree
#' @param label_size The font size of the labels
#' @param label_offset The distance between the labels and the tree
#' @param palette_type Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3" (see \code{\link[RColorBrewer]{brewer.pal}})
#' @param legend_position Specify legend position in plot
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
                          metadata,
                          layout = "circular",
                          tree_color = "black",
                          line_width = 0.1,
                          color_variable = NULL,
                          tippoint_size = 1,
                          label_variable = NULL,
                          label_size = 4,
                          label_offset = 0.01,
                          palette_type = "Paired",
                          legend_position = "right",
                          own_palette = NULL) {

  if (is.data.frame(metadata) == TRUE) {
    # Set data frame object
    metadata_df <- metadata
  } else {
    # Import metadata
    metadata_df <- read.table(
      metadata,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE)
  }

  # Set colors
  if (!is.null(own_palette)) {
    if (!is.null(color_variable)) {
      # User-defined color palette
      palette <- own_palette
    }
  } else {
    if (!is.null(color_variable)) {
      # automatically create based on variables
      vars <- unique(metadata_df[[color_variable]])
      palette <- brewer.pal(length(vars), palette_type)
      names(palette) <- vars
    }
  }

  if (layout %in% c("circular","fan")) {
        p <- ggtree(tree,
                    layout = layout,
                    color = tree_color,
                    size = line_width) %<+% metadata_df +
          {if (!is.null(label_variable))
            geom_tiplab2(aes(label = !! sym(label_variable)),
                         offset = label_offset,
                         size = label_size)} +
          {if (!is.null(color_variable))
            geom_tippoint(aes(color = !! sym(color_variable)),
                        size = tippoint_size)} +
          {if (!is.null(color_variable))
            scale_color_manual(values = palette)} +
          theme(legend.position = legend_position)
      } else {
        p <- ggtree(tree,
                    layout = layout,
                    color = tree_color,
                    size = line_width) %<+% metadata_df +
          {if (!is.null(label_variable))
            geom_tiplab(aes(label = !! sym(label_variable)),
                        offset = label_offset,
                        size = label_size)} +
          {if (!is.null(color_variable))
            geom_tippoint(aes(color = !! sym(color_variable)),
                          size = tippoint_size)} +
          {if (!is.null(color_variable))
            scale_color_manual(values = palette)} +
          theme(legend.position = legend_position)
      }

  return(p)

}
