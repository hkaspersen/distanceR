#' Calculate Simpson diversity
#'
#' Function that calculates the Simpson diversity of data in given column
#'
#' @param df Data frame holding the data in tidy format. For ST diversity calculations, have one column with the ST of each sample in the rows
#' @param colname The name of the column holding the data you want calculated. Must be given as a string.
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @importFrom dplyr count
#'
simpson_diversity <- function(df, colname) {
  df <- count(df, !!sym(colname))

  n_unique <- length(unique(df[[colname]]))
  min_n <- min(df$n)
  max_n <- max(df$n)
  sd <- round(sd(df$n), 2)
  median_n <- median(df$n)

  n_1 <- df$n - 1
  n_n_1 <- df$n * n_1
  total <- sum(df$n)
  all_div <- total*(total - 1)
  sum_div <- sum(n_n_1)

  d <- 1 - (sum_div/all_div)

  results <- data.frame(id = colname,
                        n_unique = n_unique,
                        min = min_n,
                        max = max_n,
                        median = median_n,
                        sd = sd,
                        simpson = round(d, 3))

  return(results)
}
