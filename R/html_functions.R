# function for extracting text from full text html (as a file or online)

# issues:
  # some p tags contain links only - these might be removable by class
  # references appear to have gone missing -
    # this is probably due to the poor way of dealing with nested data
    # a better option might be dealing with that structure more directly


#' Parse full text articles given as HTML
#' 
#' This function takes a link or file, and parses the resulting HTML into a
#' list where section headings are converted to list entries.
#' 
#' 
#' @param x A HTML file
#' @return A list of length equal to the number of sections in the source
#' document (identified by <h2> tags). Each list entry is a character vector;
#' each entry in the vector is a paragraph from the article text (identified by
#' <p> tag).
#' @examples
#' 
#' article <- read_fulltext_html("https://doi.org/10.1371/journal.pone.0140973")
#' 
#' @export read_fulltext_html
read_fulltext_html <- function(
  x # path to html (local file or online)
){

  # import using rvest
  html_doc <- read_html(x)

  # locate h2 nodes and text that follows it
  divs <- html_doc %>% html_nodes("div")
  text_list <- lapply(divs, function(a){
    headings <- a %>% html_nodes("h2")
    if(length(headings) > 0){
      text_out <- a %>% html_nodes("p") %>% html_text()
      return(c(html_text(headings), text_out))
    }else{
      NULL
    }
  })
  non_null <- which(!unlist(lapply(text_list, is.null)))
  text_list <- text_list[non_null]

  # there is duplication here, so keep only the smallest text
  attr_df <- data.frame(
    entry = seq_along(text_list),
    section = unlist(lapply(text_list, function(a){a[[1]]})),
    length = unlist(lapply(text_list, length)),
    stringsAsFactors = FALSE
  )
  attr_list <- split(attr_df, attr_df$section)
  keep_list <- sort(
    unlist(lapply(
      attr_list,
      function(a){a$entry[which.min(a$length)]}
    ))
  )
  text_final <- text_list[keep_list]

  # set list names
  names(text_final) <- unlist(lapply(text_final, function(a){a[[1]]}))
  text_final <- lapply(text_final, function(a){a[-1]})

  # subset
  text_final <- text_final[unlist(lapply(text_final, length)) > 0] # entries that have content
  text_final <- lapply(text_final, function(a){return(a[a != ""])}) # remove empty strings

  return(text_final)
}
