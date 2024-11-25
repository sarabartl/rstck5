generate_indices <- function(qsf_template, block_length, id_string) {
  
  # Get indices of id_string 
  string_pos <- find_string_pos(qsf_template, id_string)
  
  # Generate indices for each turn (for each point of insertion)
  indices_1 <- generate_single_index_list(string_pos[1], block_length)
  indices_2 <- generate_single_index_list(string_pos[2], block_length)
  
  # List of indices to return
  indices <- c(indices_1, indices_2)
  
  return(indices)
  
}