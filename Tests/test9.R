library(tidyverse)
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

x <- tissue_gene_expression$x

# Q1
d <- dist(tissue_gene_expression$x)

# Q2
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# Q3
image(as.matrix(d))
