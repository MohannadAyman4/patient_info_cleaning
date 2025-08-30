#Operators: 
#1) Assignment operator: 
hight <- c(1.75 , 1.76 , 1.82 , 1.67)
c(68 , 78 , 85 , 75) -> weight 
smoking_status = c("YES" , "NO" , "NO", "YES")

#2) Arithmetic operator 
BMI <- weight/(hight^2)
BMI

#3)Comparison opertor 
BMI>25
BMI<18.5

#Logical operator 

(BMI>25)&(smoking_status =="YES")
(BMI>25)|(smoking_status =="YES")

#---------------------------------------------------------------------------
#Data Structure 

#1)Vectors: 

#Numeric vector 
num_vec <- c(1,2,3,4)
class(num_vec)

#Character vector 
Chrc_vector <- c("gene1" , "gene2" , "gene3")

#Logical vector 
logical_vector <- c(TRUE , FALSE)
mix_vector <- c("gene1" ,1,"gene2" , 2)
#----------------------------------------------------------------------------
#2) Lists
all_vectors <- list(num_vec, Chrc_vector , logical_vector)
vec_col <- cbind(num_vec , Chrc_vector)
#----------------------------------------------------------------------------
#3) Matrices 
my_matrix <- matrix(1:9 , nrow = 3 , ncol = 3)
#----------------------------------------------------------------------------
#4) Data frame 

data <- data.frame( patient_id = c("P1" ,"P2" , "P3") , age = c(65 ,78 ,NA), diagnosis = c("Cancer" , "diabetes" , "Cancer"))
str(data)
head(data)
tail(data)
dim(data)
#---------------------------------------------------------------------------
#Missing value 
is.na(data)
sum(is,na(data))
na.omit(data)
clean_data <- data
clean_data [ is.na(clean_data)] <- 0
#---------------------------------------------------------------------------
#Functions 
calculate_BMI <- function(weight , height) { bmi <- weight/(height^2)
return(bmi)
}
calculate_BMI(weight = 90 , height = 1.75)

#---------------------------------------------------------------------------
input_dir <- "Data"
output <- "result"
files_to_process <- c("DEGs_Data_1.csv" , "DEGs_Data_2.csv")
result_list <- list()
for (file in files_to_process) {
  cat("\nprocessing" , file, "\n")
  input_file_path <- file.path(input_dir , file)
  data <- read.csv(file.path(input_dir, file), header = TRUE)
  cat("File imported. Checking for missing values ............... \n")
  
  missing_count <- sum(is.na(data))
  cat("Number of missing values:", missing_count, "\n")
  data$Category <- mapply(classify_gene, data$log2FoldChange, data$padj)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(file), "_processed.csv"))
  write.csv(data, out_file, row.names = FALSE)
  summary_counts <- table(data$Category)
  print(paste("Summary for", file, ":"))
  print(summary_counts)
  summary_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(file), "_summary.csv"))
  write.csv(as.data.frame(summary_counts), summary_file, row.names = FALSE)
}




classify_gene <- function(log2FC, padj, lfc_threshold = 1, padj_threshold = 0.05) {
  if (is.na(log2FC) | is.na(padj)) {
    return("Not_Significant")
  } else if (padj < padj_threshold & log2FC >= lfc_threshold) {
    return("Upregulated")
  } else if (padj < padj_threshold & log2FC <= -lfc_threshold) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}
input_dir <- "C:/Users/BAB AL SAFA/OneDrive/Documents/lnternship/Data"
files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")
output_dir <- "C:/Users/BAB AL SAFA/OneDrive/Documents/Internship/result"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

if (!dir.exists(output_dir)) dir.create(output_dir)
for (file in files_to_process) {
  cat("\nProcessing:", file, "\n")
  input_file_path <- file.path(input_dir, file)
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values ............... \n")
  data <- na.omit(data)
  output_file <- file.path(output_dir, paste0("Processed_", file))
  write.csv(data, output_file, row.names = FALSE)
  summary_counts <- table(data$classification)
  cat("Summary for", file, ":\n")
  print(summary_counts)
  summary_df <- as.data.frame(summary_counts)
  summary_file <- file.path(output_dir, paste0("Summary_", file))
  write.csv(summary_df, summary_file, row.names = FALSE)
}



