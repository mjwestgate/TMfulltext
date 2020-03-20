# functions to manage large numbers of files

# get a vector of all file paths
# this is an expansion of list.files in two ways:
  # searches subdirectories (but only to 1 level deep)
  # optionally returns only those files with a specified suffix
# defaults to searching the working directory
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