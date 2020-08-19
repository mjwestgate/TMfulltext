# functions to manage large numbers of files

# get a vector of all file paths
# this is an expansion of list.files in two ways:
  # searches subdirectories (but only to 1 level deep)
  # optionally returns only those files with a specified suffix
# defaults to searching the working directory


#' Get names of files within a set of folders
#' 
#' Basically a slightly more advanced version of \code{list.files} for nested
#' data structures. This is a pretty basic implementation, but should work to 2
#' levels of nestedness.
#' 
#' 
#' @param path A path within which to locate files. Defaults to the working
#' directory
#' @param suffix Optional suffix to allow returning of selected file types
#' (e.g. "pdf")
#' @return A character vector listing all files in the top and any subfolders
#' @export get_file_names
get_file_names <- function(
  path,
  suffix
){
  # work out where to search
  if(missing(path)){
    top_level <- list.files()
  }else{
    top_level <- paste0(path, "/", list.files(path))
  }

  # look for directories to search and add those files
  dir_check <- dir.exists(top_level)
  if(any(dir_check)){
    all_files <- lapply(top_level[dir_check], function(a){
      lapply(
        paste0(a, "/", list.files(a)),
        function(b){
          paste0(b, "/",list.files(b))
        }
      )
    })
    result <- unlist(all_files)
    result <- sub("/$", "", result)
    result <- gsub("//", "/", result)
  }

  # add files in the top directory
  file_check <- file.exists(top_level) & !dir_check
  if(any(file_check)){
    result <- c(top_level[file_check], result)
  }

  # return all entries, or those with a specified suffix
  if(missing(suffix)){
    return(result)
  }else{
    return(result[
      grepl(paste0(suffix, "$"), result)
    ])
  }

}

# import .txt files in a standard way


#' Read text files
#' 
#' Import .txt files in a standard way
#' 
#' 
#' @param x A .txt file to process
#' @param skip_blank logical: should blank lines be skipped? Defaults to FALSE
#' @return A character vector
#' @export read_text
read_text <- function(x, skip_blank = FALSE){
  scan(x,
    sep = "\n",
    what = "character",
    quote = "",
    quiet = TRUE,
    blank.lines.skip = skip_blank,
    strip.white = TRUE
  )
}
