#' FUNCTION -------------- 
#' 
#' Turn a BNC transcript into a standardised format
#'
#' @description
#' `bnc_to_standard` converts a BNC transcript into a standardised format with 
#' 3 columns: line number, speaker, turn.
#' 
#' @parameters
#' bnc_transcript: a bnc transcript (dataframe)

#' @return
#' The bnc transcript in the standardised format (dataframe)

bnc_to_standard <- function(bnc_transcript){
  standard_bnc <- names_to_letters(bnc_transcript, 1) |> #apply names_to_letters function
    dplyr::select(3, 1, 6)|> #keep turn, name and message columns
    dplyr::rename(line=1, speaker=2, text=3) #rename columns 
  
  return(standard_bnc)
}

#' EXAMPLE --------------

bnc_test <- readxl::read_xlsx("/Users/sarabartl/Desktop/qsf_survey_creation/BNC_CODE/S6TR.xlsx", col_names = FALSE)
bnc_to_standard(bnc_test)
