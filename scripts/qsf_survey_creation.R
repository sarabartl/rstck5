# READ IN THE QSF TEMPLATE AND THE STANDARDISED BNC CONVERSATION TRANSCRIPT

wd <- getwd()
bnc_template <- qsf::read_qsf(paste0(wd, "/surveys/BNC_Template_prefilled.qsf"))
bnc_conv <- read.csv(paste0(wd, "/data/SBNC_STAND/S6TR_STAN.csv"))

# SET PARAMETERS 
# how many turns per block
block_size <- 50
# how many of the previous blocks turns in the next block
overlap <- 0
# strings in the template that indicate the start of a new block
id_strings <- c("Block1Turn1", "Block2Turn1", "Block3Turn1", "Block4Turn1", "Block5Turn1", "Block6Turn1") 


# FORMAT TRANSCRIPT IN HTML
conversation <- bnc_conv
formatted_turns <- format_turns(conversation)

#CREATE A SURVEY OBJECT 
# this is the template now but will become the modified survey
survey <- bnc_template

######################################################################################

### THIS STAYS THE SAME FOR EVERY SURVEY
# Determine how many blocks are needed
n_blocks <- ceiling(nrow(conversation)/block_size)

if (n_blocks < ceiling((((n_blocks-1) * overlap)+nrow(conversation))/block_size)) {
  n_blocks <- n_blocks + 1
}

# string ids of blocks to delete
del_block_id_strings <- id_strings[(n_blocks+1):length(id_strings)]

# string ids of blocks to keep
block_id_strings <- id_strings[1:n_blocks]

# go over the block id strings
for (i in 1:length(block_id_strings)) {
  # for the first block 
  if (i == 1) {
    conversation_chunk <- data.frame(formatted_turns[1:50,])
    indices <- generate_indices(survey, block_length = block_size, block_id_strings[i])
    for (i in 1:length(indices)) {
      if (i <= length(indices)/2) {
        eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[i,1]")))
      }
      if (i > length(indices)/2) {
        turn_line <- i - (length(indices)/2)
        eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[turn_line,1]")))
      }
    }
  }
  
  # for the other blocks
  else {
    
    if (overlap > 0) {
      
      block_n <- i
      
      # figure out which turns to display in a given block as a function of the 
      # block size and the overlap window
      row_x <- ((block_n - 1)*block_size) - (overlap * (block_n - 1))
      row_y <- (block_n * block_size) - (overlap * (block_n - 1))
      
      # subset conversation to turns needed
      conversation_chunk <- data.frame(formatted_turns[row_x:row_y,1])
      
      conversation_chunk[1:overlap, 1] <- gsub(
        'background-color:#ffb3b3;|background-color:#b3d0ff;', 
        'background-color:#bdc3c7;', 
        conversation_chunk[1:overlap, 1]
      )
      
      # get indices for turns
      indices <- generate_indices(survey, block_length = block_size, block_id_strings[i])
      
      # assign formatted turns to indices
      for (i in 1:length(indices)) {
        if (i <= length(indices)/2) {
          eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[i,1]")))
        }
        if (i > length(indices)/2) {
          turn_line <- i - (length(indices)/2)
          eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[turn_line,1]")))
        }
      }
    }
    
    # for the other blocks
    else if (overlap == 0) {
      
      block_n <- i
      
      # figure out which turns to display in a given block as a function of the 
      # block size and the overlap window
      row_x <- ((block_n - 1)*block_size) + 1
      row_y <- (block_n * block_size) - (overlap * (block_n - 1))
      
      # subset conversation to turns needed
      conversation_chunk <- data.frame(formatted_turns[row_x:row_y,1])
      
      
      # get indices for turns
      indices <- generate_indices(survey, block_length = block_size, block_id_strings[i])
      
      # assign formatted turns to indices
      for (i in 1:length(indices)) {
        if (i <= length(indices)/2) {
          eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[i,1]")))
        }
        if (i > length(indices)/2) {
          turn_line <- i - (length(indices)/2)
          eval(parse(text = paste0("survey", indices[[i]], " <- conversation_chunk[turn_line,1]")))
        }
      }
    }
  }
}

# DELETE BLOCKS THAT ARE NOT NEEDED

#########################################################################################################


# WRITE THE SURVEY TO A QSF FILE
qsf::write_qsf(survey, paste0(wd, "/S6TR_TEST.qsf"))






