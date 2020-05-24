#' Prepare snp-dist table
#'
#' Function that prepares the snp-dists table for distance calculations
#'
#' @param df The data frame holding the results from snp-dists
#' @param colname The name of the first column in the data frame (usually snp-dists 0.6.3 or some other version)
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @importFrom dplyr rename
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#'
distify <- function(df, colname = "snp-dists 0.6.3") {
  df %>%
    rename("isol1" = !! sym(colname)) %>%
    gather(isol2, value, -isol1) %>%
    filter(isol1 != isol2)
}
