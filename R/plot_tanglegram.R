#' Create tanglegram from two dendrograms
#'
#' Compare two dendrograms by creating a tanglegram between the two trees. Note that the same node labels must be present in both trees
#'
#' @param tree1 First tree object
#' @param tree2 Second tree object
#' @param plot_title The title of the plot
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import ggtree
#' @import dplyr
#'
plot_tanglegram <- function(tree1, tree2, plot_title = NULL) {
  p1 <- ggtree(tree1)
  p2 <- ggtree(tree2)

  d1 <- p1$data
  d2 <- p2$data

  d2$x <- max(d2$x) - d2$x + max(d1$x) + 1

  pp <- p1 + geom_tree(data=d2)

  dd <- bind_rows(d1, d2) %>%
    filter(!is.na(label)) %>%
    group_by(label) %>%
    mutate(is_horiz = n_distinct(node) == 1) %>%
    ungroup()

  final_plot <- pp + geom_line(aes(x,
                                   y,
                                   group = label,
                                   color = is_horiz),
                               data = dd) +
    scale_color_manual(values = c('TRUE' = "#80b1d3",
                                  'FALSE' = "#fb8072")) +
    theme(legend.position = c(.9,.9)) +
    labs(color = 'Horizontal Nodes') +
    ggtitle(plot_title)

  return(final_plot)
}
