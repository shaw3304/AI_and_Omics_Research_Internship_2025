
 #Assignment- 3B
#student -pallabi shaw
# 1. Quality Control (QC) , RMA Normalization, Pre-processing and Filtering


if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")

BiocManager::install(c("GEOquery","affy","arrayQualityMetrics"))

# Load Required Libraries
library(GEOquery)             
library(affy)                 
library(arrayQualityMetrics)  
library(dplyr)              


gse_data <- getGEO("GSE79973", GSEMatrix = TRUE)

# Extract expression data matrix (genes/probes × samples)
expression_data <- exprs(gse_data$GSE79973_series_matrix.txt.gz)


# Extract feature data
feature_data <-  fData(gse_data$GSE79973_series_matrix.txt.gz)

# Extract phenotype data
phenotype_data <-  pData(gse_data$GSE79973_series_matrix.txt.gz)

# Check missing values in sample annotation
sum(is.na(phenotype_data$source_name_ch1)) 


getGEOSuppFiles("GSE79973", baseDir = "Raw_Data", makeDirectory = TRUE)
untar("Raw_Data/GSE79973_RAW.tar", 
      exdir = "C:\\Users\\PALLABI\\OneDrive\\Desktop\\AI_Omics_Internship_2025\\Module_II\\Raw_Data")

raw_data <- ReadAffy(celfile.path = "Raw_Data")

raw_data  

# (QC) Before Pre-processing

arrayQualityMetrics(expressionset = raw_data,
                    outdir = "Results/QC_Raw_Data",
                    force = TRUE,
                    do.logtransform = TRUE)


# RMA Normalization
normalized_data <- rma(raw_data)

# QC after normalization 
arrayQualityMetrics(expressionset = normalized_data,
                    outdir = "Results/QC_Normalized_Data",
                    force = TRUE)

processed_data <- as.data.frame(exprs(normalized_data))

dim(processed_data)  

#Filtering  Low-Variance Transcripts 

# Calculating median intensity per probe across samples
row_median <- rowMedians(as.matrix(processed_data))

# Visualization
hist(row_median,
     breaks = 100,
     freq = FALSE,
     main = "Median Intensity Distribution")

# Set a threshold to remove low variance probes 
threshold <- 3.5 
abline(v = threshold, col = "black", lwd = 2) 

# Select probes above threshold
indx <- row_median > threshold 
filtered_data <- processed_data[indx, ] 

# Rename filtered expression data with sample metadata
colnames(filtered_data) <- rownames(phenotype_data)

# Overwrite processed data with filtered dataset
processed_data <- filtered_data 


# Phenotype Data Preparation 
class(phenotype_data$source_name_ch1) 

# Define experimental groups (normal vs cancer)
groups <- factor(phenotype_data$source_name_ch1,
                 levels = c("gastric mucosa", "gastric adenocarcinoma"),
                 label = c("normal", "cancer"))

class(groups)
levels(groups)

##########3
total_samples <- nrow(phenotype_data)

# Count of samples per group
table(groups)

# Assign to variables
normal_samples <- sum(groups == "normal")
disease_samples <- sum(groups == "cancer")

# Print summary
cat("Total samples =", total_samples, "\n")
cat("Normal samples =", normal_samples, "\n")
cat("Disease samples =", disease_samples, "\n")

# Number of probes/transcripts remaining after filtering
remaining_transcripts <- nrow(processed_data)
cat("Number of transcripts after filtering =", remaining_transcripts, "\n")

# Check group labels
levels(groups)



# -------------------------------------
# PCA Plot of Normalized Data
# -------------------------------------

library(ggplot2)

pca_res <- prcomp(t(data), scale. = TRUE)

pca_df <- data.frame(
  Sample = colnames(data),
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  Group = groups
)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  labs(title = "PCA of Normalized Microarray Data",
       x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100,1), "%)"),
       y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100,1), "%)"))

dir.create("Result_Plots", showWarnings = FALSE)
png("Result_Plots/PCA_Normalized_Data.png", width = 2000, height = 1500, res = 300)
print(pca_plot)
dev.off()


# -------------------------------------
# Boxplot of Normalized Data
# -------------------------------------

library(reshape2)  # For melting the data
library(ggplot2)


norm_df <- as.data.frame(data)
norm_df$Gene <- rownames(data)


norm_long <- melt(norm_df, id.vars = "Gene", variable.name = "Sample", value.name = "Expression")


norm_long$Group <- rep(groups, each = nrow(data))

boxplot_norm <- ggplot(norm_long, aes(x = Sample, y = Expression, fill = Group)) +
  geom_boxplot(outlier.size = 1, notch = FALSE) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("normal" = "lightblue", "cancer" = "salmon")) +
  labs(title = "Boxplot of Normalized Microarray Data",
       x = "Samples",
       y = "Normalized Expression") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

# Save plot as PNG
dir.create("Result_Plots", showWarnings = FALSE)
png("Result_Plots/Boxplot_Normalized_Data.png", width = 2000, height = 1500, res = 300)
print(boxplot_norm)
dev.off()
