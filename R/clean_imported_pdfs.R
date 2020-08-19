# new functions for text mining on PDFs

# internal function to take text where each line contains data from two columns
# and recombine into a single vector where lines are in the correct order
# This problem can happen with pdftools::pdf_text()
# This function is not perfect yet but is a start!
column_extractor <- function(x){ # where x is a string that contains two columns/line
  # x <- sub("^\\s{1,}", "", x)
  x <- strsplit(x, "\\s{2,}")
  c(
    unlist(lapply(x, function(a){a[[1]]
      # if(length(a) > 2){
      #   a[a!= ""][1]
      # }else{
      #   a[[1]]
      # }
    })),
    unlist(lapply(x, function(a){
      if(length(a) == 1){
        ""
      }else{
        paste(a[-1], collapse = " ")
      }
      # if(length(a) == 2){
      #   a[[2]]
      # }else if(length(a) > 2){
      #   a <- a[a!= ""]
      #   return(paste(a[-1], collapse = " "))
      # }
    }))
  )
}


# take text as returned by pdftools::pdf_text,
# and return a string of length > 1
# each entry containing a paragraph from the text
# This has the following features:
  # double-columns are split properly, so text is in correct order
  # words are not split across lines, so word counts should be accurate


#' Parse text imported from PDFs
#' 
#' Text imported from \code{pdftools::pdf_text()} can sometimes include two
#' different columns per line, and words split across those lines. This
#' function attempts to clean data in this format. Fairly experimental at this
#' stage.
#' 
#' 
#' @param x A HTML file
#' @param single_string logical: should results be returned as a single string?
#' Defaults to TRUE
#' @param remove_references logical: should references be removed? Defaults to
#' TRUE
#' @return A string containing cleaned text.
#' @examples
#' 
#' \dontrun{article <- clean_double_columns(pdftools::pdf_text("file.pdf"))}
#' 
#' @export clean_double_columns
clean_double_columns <- function(
  x,
  single_string = TRUE, # logical: should results be returned as a single string?
  remove_references = TRUE # logical: should references be removed?
){
  # check size info
  if(sum(nchar(x)) < 10){
    return(paste(x, collapse = ""))
  }else{

    # split into two columns and recombine into a single vector
    line_list <- strsplit(x, "\\n")
    column_list <- lapply(line_list, column_extractor)
    text_full <- do.call(c, column_list)

    # work out where words are split over a line-ending and recombine
    end_dash <- grepl("[[:alpha:]]-$", text_full)
    if(any(end_dash)){
      replacements <- unlist(lapply(which(end_dash), function(a){
        paste0(
          sub("-$", "", text_full[a]),
          sub("^\\s{1,}", "", text_full[(a + 1)])
        )
      }))
      text_full[end_dash] <- replacements
      text_full <- text_full[-(which(end_dash) + 1)]
    }

    # convert to one entry per paragraph
    paragraph_string <- unlist(lapply(
      split(text_full, cumsum(text_full == "")),
      function(a){paste(a, collapse = " ")}
    ))
    paragraph_string <- gsub("\\s{2,}", " ", paragraph_string)
    paragraph_string <- paragraph_string[paragraph_string != ""]

    # remove content after acknowledgement (inc. references)
    if(remove_references){
      ref_lookup <- grepl("references", tolower(paragraph_string))
      if(any(ref_lookup)){
        ref_start <- which(ref_lookup)[length(which(ref_lookup))] - 1
        paragraph_string <- paragraph_string[c(1:ref_start)]
      }
    }

    if(single_string){
      return(
        paste(paragraph_string, collapse = "\n")
      )
    }else{
      return(paragraph_string)
    }
  }
}
