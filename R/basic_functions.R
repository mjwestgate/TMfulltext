# get_lower_files <- function(x){paste0(x, "/", list.files(x))}

# get a vector of all file paths
get_file_names <- function(path){
  top_level <- paste0(path, "/", list.files(path))
  all_files <- lapply(top_level, function(a){
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
  return(result)
}

# import .txt files in a standard way
read_text <- function(x){
  scan(x,
    sep = "\n",
    what = "character",
    quote = "",
    quiet = TRUE,
    blank.lines.skip = FALSE,
    strip.white = TRUE
  )
}