% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_metadata_tree.R
\name{plot_metadata_tree}
\alias{plot_metadata_tree}
\title{Plot tree with metadata and distances}
\usage{
plot_metadata_tree(
  tree,
  metadata,
  tiplab_var,
  tippoint_var,
  heatmap_data,
  tree_palette,
  heatmap_palette,
  snpdists,
  nodes,
  align_lab = TRUE,
  rmv_str = "_filtered.fasta",
  label_offset = NULL,
  label_size = 4,
  point_size = 4,
  clade_offset = NULL,
  heatmap_offset = NULL,
  heatmap_width = NULL,
  colnames = FALSE,
  colnames_pos = NULL,
  colnames_angle = 0,
  xlim_max = NULL,
  treescale_x = NULL,
  treescale_y = NULL
)
}
\arguments{
\item{tree}{The tree-file}

\item{metadata}{The data frame containing the metadata. First column must be the identifier}

\item{tiplab_var}{The column name of the data mapped to the tip labels}

\item{tippoint_var}{The column name of the data mapped to the tip points}

\item{heatmap_data}{The data frame used for mapping the heatmap to the tree. Row names must be identifiers}

\item{tree_palette}{Named vector with colors that match the uniqe values in the tip labels variable}

\item{heatmap_palette}{Named vector with colors that match the unique values in the tippoint variable}

\item{snpdists}{Data frame holding the SNP-distances, generated with snpdist}

\item{nodes}{A numeric vector that refers to the nodes in the tree to summarise SNP distances on}

\item{align_lab}{Should the tip labels be aligned?}

\item{rmv_str}{Suffix to remove from sample id's in the snpdists}

\item{label_offset}{Distance between the tip and the label}

\item{label_size}{Font size of label}

\item{point_size}{Size of the tip point}

\item{clade_offset}{Distance between the clade label and tree}

\item{heatmap_offset}{Distance between tree and heatmap}

\item{heatmap_width}{Width of the heatmap}

\item{colnames}{Should the heatmap have column names?}

\item{colnames_pos}{The position of the column names, "top" or "bottom"}

\item{colnames_angle}{The angle of the column names}

\item{xlim_max}{The max value for the x axis}

\item{treescale_x}{The x position of the treescale}

\item{treescale_y}{The y position of the treescale}
}
\description{
Function that plots a tree with associated metadata, in addition to summarizing SNP distances for given nodes
}
\author{
Håkon Kaspersen, \email{hakon.kaspersen@vetinst.no}
}
