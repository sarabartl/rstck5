#' SCRIPT -------------- 
#' 
#' This script ingests a series of BNC files,
#' converts them to a standardised format using the
#' bnc_to_standard.R function and saves the converted
#' versions to .csv files.
#' 

# assign path for bnc .xlsx files and get a list of paths for all files
sbnc_path <- "/Users/sarabartl/Desktop/qsf_survey_creation/SBNC_DATA"
sbnc_files <- list.files(path = sbnc_path, pattern = "*.xlsx", full.names = TRUE)

# assign path for folder to save standardised files to
sbnc_standard_path <- "/Users/sarabartl/Desktop/qsf_survey_creation/SBNC_STAND"

# define a function that processes each file
process_bnc_file <- function(file) {
  # read the .tsv file
  data <- readxl::read_xlsx(file, col_names = FALSE)
  # apply udc_to_standard() function
  data_transformed <- bnc_to_standard(data)
  # generate the output file names
  output_file <- gsub(".xlsx", "_STAN.csv", basename(file))
  # write to files
  write.csv(data_transformed, file.path(sbnc_standard_path, output_file), row.names = FALSE)
}

# apply process_file function to each tsv_file
lapply(sbnc_files, process_bnc_file)