\name{annotate_tree}
\alias{annotate_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annotate tree with nodes and labels
}
\description{
Add informative node colors and labels to existing tree. The function creates unique color palettes based on unique column values in supplied metadata file.
}
\usage{
annotate_tree(tree,
              metadata_path,
              layout = "circular",
              tree_color = "black",
              line_width = 0.1,
              color_variable,
              tippoint_size = 1,
              label_variable,
              label_size = 4,
              label_offset = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
A tree object to annotate.
}
  \item{metadata_path}{
The path to a tab-separated metadata file with information to annotate with. The first column in the file must be exact-matching IDs to the IDs used to create the tree.
}
  \item{layout}{
From ggtree:
One of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight'.
}
  \item{tree_color}{
The color of the tree, default "black".
}
  \item{line_width}{
The width of the lines in the tree, default 0.1.
}
  \item{color_variable}{
A string representing the column name in the metadata file to plot as nodes on the tree, where each unique value in the column will be represented with a color (max 11 unique values).
}
  \item{tippoint_size}{
The size of the colored nodes in the tree.
}
  \item{label_variable}{
A string representing the column name in the metadata file to plot as labels on the tree.
}
  \item{label_size}{
The font size of the labels.
}
  \item{label_offset}{
The distance between the labels and the tree.
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
