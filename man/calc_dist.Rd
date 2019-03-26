\name{calc_dist}
\alias{calc_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance calculation from cgMLST alleles
}
\description{
Imports the cgMLST.tsv file from chewBBACA and calculates distances between samples. Outputs a distance matrix.
}
\usage{
calc_dist(path, metric = "gower")
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
