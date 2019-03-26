\name{calc_tree}
\alias{calc_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import and calculate distances from cgMLST data
}
\description{
Imports the "cgMLST.tsv" generated with chewBBACA in the correct format for distance calculation.
The function then calculates distances between samples in the data by using the "daisy" and "hclust" functions from Cluster, with the default metric "gower" in daisy and "average" method in hclust. The clustered distances are then converted to a dendrogram with the as.phylo function from ape.
}
\usage{
calc_tree(path, metric = "gower", method = "average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Full path to the cgMLST.tsv file, directly from chewBBACA.
}
  \item{metric}{
Character string specifying the metric to be used, default "gower".
From the daisy package:

"The currently available options are "euclidean", "manhattan" and "gower".
Euclidean distances are root sum-of-squares of differences, and manhattan distances are the sum of absolute differences.
“Gower's distance” is chosen by metric "gower" or automatically if some columns of x are not numeric. Also known as Gower's coefficient (1971), expressed as a dissimilarity, this implies that a particular standardisation will be applied to each variable, and the “distance” between two units is the sum of all the variable-specific distances."
}
  \item{method}{
From the hclust package:
The agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Håkon Kaspersen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
