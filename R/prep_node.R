#' Prepare list of tips below node
#'
#' Function that identifies the tips below a given node, and outputs a data frame with the sample id and node id
#'
#' @param df The tree, in tibble format
#' @param node Numeric, the node of interest
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @importFrom tidytree offspring
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
prep_node <- function(df, node) {
  offspring(df, node, tiponly = TRUE) %>%
    select(label) %>%
    mutate(node = node)
}
