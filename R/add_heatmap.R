# Add heatmap to tree
add_heatmap <- function(tree,
                        heatmap_data,
                        perc_open = 10,
                        perc_rotate = 95,
                        heatmap_offset = 0.05,
                        heatmap_width = 0.5,
                        colnames_offset = 2,
                        colnames_position = "top",
                        font_size = 5) {
  # Import heatmap data
  heatmap_df <- read.table(
    heatmap_data,
    sep = "\t",
    header = TRUE,
    row.names = 1,
    stringsAsFactors = FALSE
  )

  # Create color palette
  vars <- unique(unlist(heatmap_df))
  palette <- brewer.pal(length(vars), "Paired")
  names(palette) <- vars

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
