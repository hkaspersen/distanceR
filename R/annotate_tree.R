# Annotate tree with metadata
annotate_tree <- function(tree,
                          metadata_path,
                          layout = "circular",
                          tree_color = "black",
                          line_width = 0.1,
                          color_variable,
                          tippoint_size = 1,
                          label_variable,
                          label_size = 4,
                          label_offset = 0.01) {

  # Import metadata
  metadata_df <- read.table(
    metadata_path,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE)

  # Set colors for variable
  vars <- unique(metadata_df[[color_variable]])
  palette <- brewer.pal(length(vars), "Paired")
  names(palette) <- vars

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