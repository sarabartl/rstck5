#' SCRIPT -------------- 
#' 
#' This script ingests a series of UDC files,
#' converts them to a standardised format using the
#' udc_to_standard.R function and saves the converted
#' versions to .csv files.
#' 

# assign path for udc .tsv files and get a list of paths for all files
tsv_path <- "/Users/sarabartl/Desktop/qsf_survey_creation/UDC_DATA"
tsv_files <- list.files(path = tsv_path, pattern = "*.tsv", full.names = TRUE)

# assign path for folder to save standardised files to
udc_standard_path <- "/Users/sarabartl/Desktop/qsf_survey_creation/UDC_STAND"

# define a function that processes each file
process_udc_file <- function(file) {
  # read the .tsv file
  data <- read.delim(file, sep = "\t", header = TRUE)
  # apply udc_to_standard() function
  data_transformed <- udc_to_standard(data)
  # generate the output file names
  output_file <- gsub("CLEAN_(.*).tsv", "UDC_STAN_\\1.csv", basename(file))
  # write to files
  write.csv(data_transformed, file.path(udc_standard_path, output_file), row.names = FALSE)
}

# apply process_file function to each tsv_file
lapply(tsv_files, process_udc_file)