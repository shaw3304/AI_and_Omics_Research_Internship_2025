#######################################################################
####
#Assignment 5
# Differential Gene Expression Analysis using Limma
#student- Pallabi shaw
#######################################################################

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("AnnotationDbi", "hgu133plus2.db", "limma", "pheatmap"))


library(AnnotationDbi)
library(hgu133plus2.db)
library(limma)
library(ggplot2)
library(pheatmap)
library(dplyr)
library(tibble)

# -------------------------------------------------------------
# Map probe IDs to gene symbols
# -------------------------------------------------------------
probe_ids <- rownames(processed_data)

gene_symbols <- mapIds(
  hgu133plus2.db,
  keys = probe_ids,
  keytype = "PROBEID",
  column = "SYMBOL",
  multiVals = "first"
)

# Create a data frame for mapping
gene_map_df <- data.frame(PROBEID = names(gene_symbols), SYMBOL = gene_symbols)

# Merge with expression data
processed_data_df <- processed_data %>%
  rownames_to_column("PROBEID") %>%
  left_join(gene_map_df, by = "PROBEID") %>%
  filter(!is.na(SYMBOL))

# -------------------------------------------------------------
# Handle multiple probes mapping to the same gene
# -------------------------------------------------------------
expr_only <- processed_data_df %>%
  select(-PROBEID, -SYMBOL)

# Collapse duplicate probes 
averaged_data <- limma::avereps(expr_only, ID = processed_data_df$SYMBOL)

# Convert to numeric matrix for limma
data <- as.matrix(averaged_data)

# -------------------------------------------------------------
#  Create design matrix (Normal vs Cancer)
# -------------------------------------------------------------
design <- model.matrix(~0 + groups)
colnames(design) <- levels(groups)

# Fit linear model
fit <- lmFit(data, design)

# Define contrast: Cancer vs Normal
contrast_matrix <- makeContrasts(cancer_vs_normal = cancer - normal, levels = design)
fit2 <- contrasts.fit(fit, contrast_matrix)
fit2 <- eBayes(fit2)

# -------------------------------------------------------------
# Extract Differentially Expressed Genes (DEGs)
# -------------------------------------------------------------
deg_results <- topTable(fit2,
                        coef = "cancer_vs_normal",
                        number = Inf,
                        adjust.method = "BH")

# Classify DEGs based on thresholds
deg_results$Regulation <- ifelse(deg_results$adj.P.Val < 0.05 & deg_results$logFC > 1, "Upregulated",
                                 ifelse(deg_results$adj.P.Val < 0.05 & deg_results$logFC < -1, "Downregulated", "No"))

# -------------------------------------------------------------
# : Save DEG results
# -------------------------------------------------------------
dir.create("Results", showWarnings = FALSE)

write.csv(deg_results, file = "Results/DEG_All.csv", row.names = TRUE)
write.csv(subset(deg_results, Regulation == "Upregulated"), file = "Results/DEG_Upregulated.csv")
write.csv(subset(deg_results, Regulation == "Downregulated"), file = "Results/DEG_Downregulated.csv")

# -------------------------------------------------------------
# Volcano Plot
# -------------------------------------------------------------
dir.create("Result_Plots", showWarnings = FALSE)

png("Result_Plots/VolcanoPlot_DEG.png", width = 2000, height = 1500, res = 300)
ggplot(deg_results, aes(x = logFC, y = -log10(adj.P.Val), color = Regulation)) +
  geom_point(alpha = 0.7, size = 1.8) +
  scale_color_manual(values = c("Upregulated" = "red", "Downregulated" = "blue", "No" = "grey")) +
  theme_minimal(base_size = 12) +
  labs(title = "Volcano Plot of Differentially Expressed Genes",
       x = "log2 Fold Change",
       y = "-log10(Adjusted P-value)",
       color = "Regulation")
dev.off()


# -------------------------------
# Heatmap of Top 25 DEGs
# -------------------------------

# Ensure top 25 genes exist in your expression matrix
top25_genes <- head(rownames(deg_results[order(deg_results$adj.P.Val), ]), 25)
top25_genes <- top25_genes[top25_genes %in% rownames(data)]  # remove missing ones

# Subset the expression data
heatmap_data <- data[top25_genes, , drop = FALSE]

# Ensure it's a numeric matrix
heatmap_data <- as.matrix(heatmap_data)

# Create column labels with group info
group_char <- as.character(groups)
colnames(heatmap_data) <- paste0(group_char, "_", seq_along(group_char))

# Create colors for groups (optional)
group_colors <- ifelse(group_char == "cancer", "red", "blue")
annotation_col <- data.frame(Group = factor(group_char))
rownames(annotation_col) <- colnames(heatmap_data)

# Set PNG device with moderate size/resolution
png("Result_Plots/Heatmap_Top25_DEGs.png", width = 1200, height = 900, res = 150)

# Plot heatmap
pheatmap(heatmap_data,
         scale = "row",                     # scale by row
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         annotation_col = annotation_col,    # group annotation
         main = "Top 25 Differentially Expressed Genes")

dev.off()



