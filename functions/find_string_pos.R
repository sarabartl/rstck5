#' FUNCTION -------------- 
#'
#' @description
#' `find_string_pos` returns the index of a given string in a (nested) list
#' 
#' @parameters
#' lst: in our case the qsf file (list)
#' target: string for which we want to find the index (string)
#'
#' @return
#' the index or indices of the target string (list)

find_string_pos <- function(lst, target_string, current_path = "") {
  positions <- list()  # List to store all found positions
  
  for (i in seq_along(lst)) {
    # Build the current path as a string with double square brackets
    new_path <- paste0(current_path, "[[", i, "]]")
    
    # If the element is a list, recursively search inside it
    if (is.list(lst[[i]])) {
      positions <- c(positions, find_string_pos(lst[[i]], target_string, new_path))
    } 
    # If the element is a string, check if it matches the target
    else if (is.character(lst[[i]]) && lst[[i]] == target_string) {
      positions <- c(positions, new_path)  # Store the current path as a string
    }
  }
  
  return(positions)  # Return all positions
}