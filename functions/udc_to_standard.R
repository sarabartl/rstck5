#' FUNCTION -------------- 
#' 
#' Turn a UDC transcript into a standardised format
#'
#' @description
#' `udc_to_standard` converts a UDC transcript into a standardised format with 
#' 3 columns: line number, speaker, turn.
#' 
#' @parameters
#' udc_transcript: a tsv file  (dataframe)
#' anon: logical paramter; if set to TRUE, the usernames will be anonimised, 
#' if set to FALSE, the original usernames will be kept
#'
#' @return
#' The udc transcript in the standardised format (dataframe)

udc_to_standard <- function(udc_transcript, anon = TRUE){
  
  if (anon == TRUE) {
  standard_udc <- names_to_letters(udc_transcript, 2) |> #apply names_to_letters function
    dplyr::select(TURN, NAME, MESSAGE)|> #keep turn, name and message columns
    dplyr::rename(line=TURN, speaker=NAME, text=MESSAGE) |> #rename columns 
    dplyr::mutate(line_group = sub("\\..*", "", line)) |> #deal with turn groups 
    dplyr::mutate(line_group = factor(line_group, levels=unique(line_group))) |> 
    dplyr::group_by(line_group, speaker) |>
    dplyr::summarise(
      text= paste(text, collapse = " "), #combine turns of turn groups into one df line
      .groups = "drop") |>
    dplyr::rename(line = line_group) #rename turn column
  }
  
  if (anon == FALSE) {
    standard_udc <- names_to_letters(udc_transcript, 2) |> #apply names_to_letters function
      dplyr::select(TURN, USER, MESSAGE)|> #keep turn, name and message columns
      dplyr::rename(line=TURN, speaker=USER, text=MESSAGE) |> #rename columns 
      dplyr::mutate(line_group = sub("\\..*", "", line)) |> #deal with turn groups 
      dplyr::mutate(line_group = factor(line_group, levels=unique(line_group))) |> 
      dplyr::group_by(line_group, speaker) |>
      dplyr::summarise(
        text= paste(text, collapse = " "), #combine turns of turn groups into one df line
        .groups = "drop") |>
      dplyr::rename(line = line_group) #rename turn column
  }
  
  return(standard_udc)
}

#' EXAMPLE --------------

udc_5976 <- read.delim("/Users/sarabartl/Desktop/RESTOCK5/UDC_Data/UDC_FINAL_R3/CLEAN_5976.tsv", sep="\t")
head(udc_to_standard(udc_5976))
head(udc_to_standard(udc_5976, anon = FALSE))



