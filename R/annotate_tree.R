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
#' @param shape_variable A string representing the column in the metadata file to represent as shapes on the tips
#' @param shape_values The shape types
#' @param clade_label_node The node where the clade label is placed
#' @param clade_label The label that will be placed on the clade
#' @param cladelabel_offset The distance between the tree and the clade label
#' @param align_cladelabel Should the clade label be aligned?
#' @param align Should the labels align?
#' @param ladderize Should the tips be ladderized?
#' @param midroot Should the tree be midrooted?
#' @param node_labels Should node labels be included? This is the node number, not the bootstrap values!
#' @param bootstrap_lab Should the bootstrap values be included? Note: This is the values in the node.label data from the tree
#' @param nodelab_geom Should the bootstrap labels be plain text or labels?
#' @param bootlab_size The size of the bootstrap values
#' @param bootstrap_var The bootstrap values used to create colored nodes in the tree, only works for Treedata types
#' @param nodepoint_size The size of the colored bootstrap nodes
#' @param treescale Should a treescale be rendered?
#' @param treescale_x x position of treescale
#' @param treescale_y y position of treescale
#' @param treescale_linesize Size of the treescale line
#' @param palette_type Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3" (see \code{\link[RColorBrewer]{brewer.pal}})
#' @param legend_position Specify legend position in plot
#' @param color_palette If a named vector of colors are specified here, they are used instead of the automatically generated colors from colorBrewer
#' @param shape_palette Use your own named vector as shape palette
#' @param node_palette Use your own colors for bootstrap values
#' @param tree_type What type of tree object to use, either "treedata" or "phylo"
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggtree
#' @import dplyr
#' @importFrom phangorn midpoint
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils read.table
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 guides
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
                          shape_variable = NULL,
                          shape_values = c(21, 22, 23, 24, 25),
                          clade_label_node = NULL,
                          clade_label = NULL,
                          cladelabel_offset = NULL,
                          align_cladelabel = FALSE,
                          align = FALSE,
                          ladderize = TRUE,
                          midroot = FALSE,
                          node_labels = FALSE,
                          bootstrap_lab = TRUE,
                          nodelab_geom = "label",
                          bootlab_size = 3,
                          bootstrap_var = NULL,
                          nodepoint_size = 3,
                          treescale = TRUE,
                          treescale_x = NULL,
                          treescale_y = NULL,
                          treescale_linesize = 0.5,
                          palette_type = "Paired",
                          legend_position = "right",
                          color_palette = NULL,
                          shape_palette = NULL,
                          node_palette = NULL,
                          tree_type = "phylo") {

  # disable scientific notation
  options(scipen = 999)

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

  if (!is.null(color_palette)) {
    if (!is.null(color_variable)) {
      # User-defined color palette
      palette <- color_palette
    }
  } else {
    if (!is.null(color_variable)) {
      # automatically create based on variables
      vars <- unique(metadata_df[[color_variable]])
      palette <- brewer.pal(length(vars), palette_type)
      names(palette) <- vars
    }
  }

  # Midroot tree

  if (midroot == TRUE) {
    if (tree_type == "phylo") {
      tree <- midpoint(tree, node.labels = "support")
      }

    if (tree_type == "treedata") {
      midtree <- midpoint(tree@phylo, node.labels = "support")
      tree@phylo <- midtree
      }
  }

  # Render tree
  if (layout %in% c("circular", "fan")) {
    p <- ggtree(tree,
                layout = layout,
                color = tree_color,
                size = line_width,
                ladderize = ladderize) %<+% metadata_df +
      {if (isTRUE(node_labels))
        geom_text(aes(label = node))} +
      {if (isTRUE(bootstrap_lab))
        geom_nodelab(size = bootlab_size,
                     geom = nodelab_geom)} +
      {if (!is.null(bootstrap_var))
        geom_nodepoint(aes(color = !!sym(bootstrap_var)),
                       size = nodepoint_size)} +
      {if (!is.null(clade_label))
        geom_cladelabel2(
          clade_label_node,
          clade_label,
          offset = cladelabel_offset,
          align = align_cladelabel
        )} +
      {if (!is.null(label_variable))
        geom_tiplab2(
          aes(label = !!sym(label_variable)),
          offset = label_offset,
          size = label_size,
          align = align
        )} +
      {if (!is.null(color_variable) & !is.null(shape_variable))
        geom_tippoint(aes(
          fill = !!sym(color_variable),
          shape = !!sym(shape_variable)),
          size = tippoint_size)} +
      {if (!is.null(color_variable) & is.null(shape_variable))
        geom_tippoint(aes(fill = !!sym(color_variable)),
                      size = tippoint_size)} +
      {if (is.null(color_variable) & !is.null(shape_variable))
        geom_tippoint(aes(shape = !!sym(shape_variable)),
                      size = tippoint_size)} +
      {if (isTRUE(treescale))
        geom_treescale(x = treescale_x,
                       y = treescale_y,
                       linesize = treescale_linesize)} +
      {if (!is.null(color_variable))
        scale_fill_manual(values = palette)} +
      {if (!is.null(shape_variable))
        scale_shape_manual(values = shape_values)} +
      {if (!is.null(shape_palette))
        scale_shape_manual(values = shape_palette)} +
      {if (!is.null(node_palette))
        scale_color_manual(values = node_palette)} +
      theme(legend.position = legend_position) +
      guides(fill = guide_legend(override.aes = list(shape = 21)),
             shape = guide_legend(override.aes = list(fill = "black")))


  } else {


    p <- ggtree(tree,
              layout = layout,
              color = tree_color,
              size = line_width,
              ladderize = ladderize) %<+% metadata_df +
    {if (isTRUE(node_labels))
        geom_text(aes(label = node))} +
    {if (isTRUE(bootstrap_lab))
        geom_nodelab(size = bootlab_size,
                     geom = nodelab_geom)} +
    {if (!is.null(bootstrap_var))
        geom_nodepoint(aes(color = !!sym(bootstrap_var)),
                       size = nodepoint_size)} +
    {if (!is.null(clade_label))
        geom_cladelabel(
          clade_label_node,
          clade_label,
          offset = cladelabel_offset,
          align = align_cladelabel
        )} +
    {if (!is.null(label_variable))
      geom_tiplab(
        aes(label = !!sym(label_variable)),
            offset = label_offset,
            size = label_size,
            align = align)} +
    {if (!is.null(color_variable) & !is.null(shape_variable))
        geom_tippoint(aes(
          fill = !!sym(color_variable),
          shape = !!sym(shape_variable)),
        size = tippoint_size)} +
    {if (!is.null(color_variable) & is.null(shape_variable))
        geom_tippoint(aes(fill = !!sym(color_variable)),
                      size = tippoint_size)} +
    {if (is.null(color_variable) & !is.null(shape_variable))
        geom_tippoint(aes(shape = !!sym(shape_variable)),
                      size = tippoint_size)} +
    {if (isTRUE(treescale))
        geom_treescale(x = treescale_x,
                       y = treescale_y,
                       linesize = treescale_linesize)} +
    {if (!is.null(color_variable))
        scale_fill_manual(values = palette)} +
    {if (!is.null(shape_variable))
        scale_shape_manual(values = shape_values)} +
    {if (!is.null(shape_palette))
        scale_shape_manual(values = shape_palette)} +
    {if (!is.null(node_palette))
        scale_color_manual(values = node_palette)} +
    theme(legend.position = legend_position) +
    guides(fill = guide_legend(override.aes = list(shape = 21)),
           shape = guide_legend(override.aes = list(fill = "black")))
  }

  return(p)

}
