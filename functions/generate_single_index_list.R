# Function to increment the second-to-last index
generate_single_index_list <- function(pos_id_string, block_length) {
  # Use regular expression to find and replace the second-to-last index with incremented values
  index_list <- sapply(1:block_length, function(i) {
    # Substitute the second-to-last [[1]] with [[i]]
    gsub("\\[\\[1\\]\\]\\[\\[1\\]\\]$", paste0("[[", i, "]][[1]]"), pos_id_string)
  })
  return(index_list)
}


