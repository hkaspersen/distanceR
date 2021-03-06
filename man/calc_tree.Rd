% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_tree.R
\name{calc_tree}
\alias{calc_tree}
\title{Import and calculate distances from cgMLST data}
\usage{
calc_tree(path, metric = "gower", method = "average")
}
\arguments{
\item{path}{Full path to the cgMLST.tsv file, directly from chewBBACA.}

\item{metric}{Character string specifying the metric to be used, default "gower" (see \code{\link[cluster]{daisy}})}

\item{method}{The agglomeration method to be used, default "average" (see \code{\link[stats]{hclust}})}
}
\description{
Imports the "cgMLST.tsv" generated with chewBBACA in the correct format for distance calculation.
}
\author{
Håkon Kaspersen, \email{hakon.kaspersen@vetinst.no}
}
