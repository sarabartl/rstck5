#' FUNCTION -------------- 
#'
#' @description
#' `format_turns` formats the turns of a conversation transcript in html so it can be displayed 
#'  in qualtrics. This includes the font size, colour of the text and background colour.
#' 
#' @parameters
#' transcript (dataframe)
#' text_colours (list)
#' background_colours (list)
#' 
#' NOTE: we can add more colours to the default here when we move on to conversations
#' with more speakers and also make the handling of speakers more dynamic
#'
#' @return
#' the formatted transcript (dataframe)
#' 

format_turns <- function(transcript, text_colours = list("#001d4d","#4d0000") , 
                         background_colours = list("#b3d0ff", "#ffb3b3")) {
  
  # change angled brackets to html displayable format
  transcript$text <- gsub("<", "&lt;", transcript$text)
  transcript$text <- gsub(">", "&gt;", transcript$text)
  
  # different parts of the html formatting language
  a <- '<span style="font-size:16px;"><span style="color:'
  b <- ';"><strong><span style="background-color:'
  c <- ';">'
  d <- '&nbsp; '
  e <- '</span></strong>&nbsp; &nbsp;'
  f <- '</span></span>'
  
  # empty dataframe
  
  html_transcript <- data.frame()
  
  # build the formatted turns line by line
  for (i in 1:nrow(transcript)) {
    if (transcript[i,2] == "A") {
      html_transcript[i,1] <- paste0(a, text_colours[1], b, background_colours[1], c, transcript[i, "line"], 
                               d, transcript[i, "speaker"], e, transcript[i, "text"], f, sep = " ")
    }
    if (transcript[i,2] == "B") {
      html_transcript[i,1] <- paste0(a, text_colours[2], b, background_colours[2], c, transcript[i, "line"], 
                               d, transcript[i, "speaker"], e, transcript[i, "text"], f, sep = " ")
    }
  }
  
  
  return(html_transcript)
  
}