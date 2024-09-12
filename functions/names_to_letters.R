#' FUNCTION -------------- 
#' 
#' Replace unique usernames with letters A - Z
#'
#' @description
#' `names_to_letters` replaces (user)names with capital letters A-Z
#' 
#' @parameters
#' conversation transcript (dataframe)
#' name_col: the index of the name column (int)
#'
#' @return
#' the conversation with anonymised user ids (dataframe)

# change usernames to A, B, etc.
names_to_letters <- function(transcript, name_col) {
  
  #get unique usernames 
  unique_names <- transcript[[name_col]] |>
    unique()
  
  #create letters needed to replace names
  letter_names <- setNames(LETTERS[1:length(unique_names)], unique_names)
  
  letters_transcript <- transcript |>
    dplyr::mutate(
      NAME = letter_names[transcript[[name_col]]], .after = name_col) |> #add letter name column
    dplyr::select((-name_col)) #delete username column
  
  return(letters_transcript)
}



