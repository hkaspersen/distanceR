\name{add_heatmap}
\alias{add_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add heatmaps to trees.
}
\description{
This function adds heatmaps to trees from specified file. The file is imported and added to the tree, and each unique value in the data is given a unique color in the heatmap. Utilizes the gheatmap function from ggtree.
}
\usage{
add_heatmap(tree,
            heatmap_data,
            perc_open = 10,
            perc_rotate = 95,
            heatmap_offset = 0.05,
            heatmap_width = 0.5,
            colnames_offset = 2,
            colnames_position = "top",
            font_size = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
A tree object to annotate.
}
  \item{heatmap_data}{
A file containing the data to add heatmap of. The first column need to hold the same ID's used to create the tree object. The remaining columns will be used in the heatmap. Each unique value in the whole file, except the ID column, will be given a unique color (max 11 unique values).
}
  \item{perc_open}{
How many percent the tree is opened. This is used to fit the heatmap labels in the gap produced.
}
  \item{perc_rotate}{
How many percent the tree is rotated. This is to get the heatmap labels on the top pf the tree.
}
  \item{heatmap_offset}{
The distance between the tree and the heatmap.
}
  \item{heatmap_width}{
The width of the heatmap.
}
  \item{colnames_offset}{
The distance between the heatmap column names and the heatmap.
}
  \item{colnames_position}{
The location of the heatmap column names, default is "top" for a right-hand side fix (when centered).
}
  \item{font_size}{
Font size of the heatmap column names.
}
  \item{palette_type}{
Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3". See ColorBrewer website for details.
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
