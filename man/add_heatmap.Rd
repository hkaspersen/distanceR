% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_heatmap.R
\name{add_heatmap}
\alias{add_heatmap}
\title{Add heatmap to tree objects}
\usage{
add_heatmap(
  tree,
  heatmap_data,
  layout = "circular",
  perc_open = 10,
  perc_rotate = 95,
  heatmap_offset = 0.05,
  heatmap_width = 0.5,
  colnames_offset = 0,
  colnames_position = "top",
  colnames_angle = 0,
  font_size = 5,
  palette_type = "Paired",
  own_palette = NULL
)
}
\arguments{
\item{tree}{A tree object to annotate}

\item{heatmap_data}{A file containing the data to add heatmap of. The first column need to hold the same ID's used to create the tree object. The remaining columns will be used in the heatmap. Each unique value in the whole file, except the ID column, will be given a unique color}

\item{layout}{The layout of the tree, either "circular", "rectangular", or other, (see \code{\link[ggtree]{ggtree}})}

\item{perc_open}{How many percent the tree is opened. This is used to fit the heatmap labels in the gap produced}

\item{perc_rotate}{How many percent the tree is rotated. This is to get the heatmap labels on the top pf the tree}

\item{heatmap_offset}{The distance between the tree and the heatmap}

\item{heatmap_width}{The width of the heatmap}

\item{colnames_offset}{The distance between the heatmap column names and the heatmap}

\item{colnames_position}{The location of the heatmap column names}

\item{colnames_angle}{The angle of the column names}

\item{font_size}{Font size of the heatmap column names}

\item{palette_type}{Specify which colorBrewer palette to use. Choose from "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3" (see \code{\link[RColorBrewer]{brewer.pal}})}

\item{own_palette}{If a named vector of colors are specified here, they are used instead of the automatically generated colors from colorBrewer}
}
\description{
Annotates tree objects with a heatmap, f. ex. representing presence/absence of genes.
}
\author{
Håkon Kaspersen, \email{hakon.kaspersen@vetinst.no}
}
