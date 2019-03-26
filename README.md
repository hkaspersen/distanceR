# distanceR
R package for calculating distances from cgMLST allele data and annotating dendrograms.

## Functions:

- calc_dist: Calculate distances from chewBBACA cgMLST.tsv file. Outputs a distance matrix.

- calc_tree: Calculate distances from chewBBACA cgMLST.tsv file. Outputs a tree object.

- annotate_tree: Annotates existing tree object with labels and colored nodes with data from a supplied metadata file. The file need to be a tab-separated text file, where the first column has the ID's that were used to create the tree object (exact matches, the order of the ID's doesn't matter).

- add_heatmap: Annotates existing tree object with a heatmap from a supplied text file. The file need to be tab separated, where the first column has the ID's that were used to create the tree object (exact matches, the order of the ID's doesn't matter). The rest of the columns in the file will be used in the heatmap.

## Author
Håkon Kaspersen, Norwegian Veterinary Institute.
