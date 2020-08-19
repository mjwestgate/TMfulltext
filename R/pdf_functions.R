# import multiple pdfs


#' Import multiple pdfs
#'
#' Takes a vector of file names and imports them. Basically a wrapper to
#' pdftools::pdf_text
#'
#' @param x string (or vector of strings) giving file locations
#' @param clean_columns logical - should columns get cleaned? Defaults to TRUE
#' @param cache_dir optional location to store temporary .rds files.
#'  If not present then text is returned instead
#' @return a list of same length as x
#' @export read_pdfs
read_pdfs <- function(
  x, # string (or vector of strings) giving file locations
  clean_columns = TRUE, # logical - should columns get cleaned?
  cache_dir # optional location to store temporary .rds files.
    # If not present then text is returned instead
){
  invisible(lapply(x, read_pdf,
    clean_columns = clean_columns,
    cache_dir = cache_dir))
}


read_pdf <- function(x, clean_columns = TRUE, cache_dir){
  # error catching
  if(missing(cache_dir)){cache_dir <- NULL}
  if(is.na(x) | nchar(x) < 1 | !grepl(".pdf$", a)){
    return("")
  }else{
    if(file.info(x)$size < 2){
      return("")
    }else{
      # process
      result <- pdftools::pdf_text(x)
      if(clean_columns){result <- clean_double_columns(result)}

      # return object as requested
      if(is.null(cache_dir)){
        return(result)
      }else{
        # set up file naming
        file_split <- strsplit(x, "/")[[1]]
        file_tr <- file_split[length(file_split)]
        out_file <- sub(".pdf$", ".rds", paste(cache_dir, file_tr, sep = "/"))
        saveRDS(result, out_file)
      # writeLines(result, paste0(out_file, ".txt"))
      }
    }
  }
}