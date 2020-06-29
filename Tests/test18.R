library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# Q1
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q2
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

# Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Q5

imp_df <- data.frame(summary(pc)$imp)
imp_df <- imp_df[2,] %>% 
  gather(key = pc, value = imp)
imp_df <- imp_df %>% 
  mutate(pc_index = as.integer(str_remove(imp_df$pc, "PC")))
imp_df$pc <- factor(imp_df$pc, 
                    levels = imp_df$pc[order(imp_df$pc_index)])
imp_df <- imp_df %>% mutate(cum_sum = cumsum(imp))

# Then he used ggplot(), with the PCs on the x and the cumulative sum on the y.

imp_df %>% filter(pc_index < 20) %>% arrange(pc_index, cum_sum) %>%
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() + scale_y_continuous(breaks = seq(0,1,0.1)) + theme_grey()
