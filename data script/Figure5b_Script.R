library(pheatmap)

# Create data frame
setwd("\\data_script\\sup_figs\\Figure 5b")
data <- read_excel("Figure 5b.xlsx")
# Convert data to matrix
data1 <- as.matrix(data)

# Perform Z-score normalization for each row (gene)
data_z <- t(scale(t(data1)))  # Transpose, normalize, then transpose back

# Create annotation data frame for sample names
annotation_col <- data.frame(Sample = colnames(data_z), row.names = colnames(data_z))

# Create heatmap color palette
heatmap_colors <- colorRampPalette(c("#a8cae8", "#d1eff4", "#f2fafc", "#fbb46f", "#F58383"))(100)

# Plot heatmap
pheatmap(data_z,
         annotation_col = annotation_col,
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_col = 10,
         fontsize_row = 10,
         annotation_names_col = FALSE,
         cluster_cols = FALSE,
         cluster_rows = TRUE,
         main = "Heatmap with Z-score Normalized Data",
         color = heatmap_colors)