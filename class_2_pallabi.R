# Assignment 2 - DGE classification 


# defining classify_gene()
classify_gene <- function(logFC, padj) {
  if (is.na(padj)) padj <- 1
  if (is.na(logFC)) return("Not_Significant")
  if (logFC > 1  & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

input_dir <- "C:/Users/PALLABI/OneDrive/Desktop/AI_Omics_Internship_2025/Module_I/raw_data"

output_dir <- "C:/Users/PALLABI/OneDrive/Desktop/AI_Omics_Internship_2025/Module_I/results"   # processed results will be stored here

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

list.files(input_dir)

files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")



# loop each file one by one
result_list <- list()

for (file_name in files_to_process) {
  cat("\nProcessing:", file_name, "\n")
  path_in <- file.path(input_dir, file_name)
  df <- read.csv(path_in, stringsAsFactors = FALSE)
  
  cn <- tolower(names(df))
  padj_idx  <- grep("^padj$|padj", cn)
  logfc_idx <- grep("^logfc$|log2fc|log2fold", cn)
  
  if (length(padj_idx) < 1) stop("padj-like column not found in ", file_name)
  if (length(logfc_idx) < 1) stop("logFC-like column not found in ", file_name)
  
  names(df)[padj_idx[1]]  <- "padj"
  names(df)[logfc_idx[1]] <- "logFC"
  
  # ensuring they are  numeric
  df$logFC <- as.numeric(df$logFC)
  df$padj  <- as.numeric(df$padj)
  
  # replacing missing padj with 1
  df$padj[is.na(df$padj)] <- 1
  
  # classify_gene row-wise
  df$status <- apply(df, 1, function(row) {
    classify_gene(as.numeric(row["logFC"]), as.numeric(row["padj"]))
  })
  
  # saving processed file
  out_path <- file.path(output_dir, paste0("Processed_", file_name))
  write.csv(df, out_path, row.names = FALSE)
  cat("Saved processed file to:", out_path, "\n")
  
  # print summary counts
  cat("\nSummary counts for", file_name, ":\n")
  print(table(df$status))
  
  result_list[[file_name]] <- df
}

#Summary counts for DEGs_Data_1.csv :
##Not_Significant 
#22283 

#Summary counts for DEGs_Data_2.csv :
#Downregulated Not_Significant     Upregulated 
#1383           52622             670



#  combined summary
combined_status <- unlist(lapply(result_list, function(x) x$status))
cat("\nCombined summary across all files:\n")
print(table(combined_status))

#Downregulated Not_Significant     Upregulated 
#1383           74905             670


total_significant <- sum(combined_status %in% c("Upregulated", "Downregulated"))
cat("\nTotal significant (Upregulated + Downregulated) across all files:", total_significant, "\n")
#Total significant (Upregulated + Downregulated) across all files: 2053 


save.image("PallabiShaw_Class_2_Assignment.RData")

