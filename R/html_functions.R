# functions for extracting text from full text html files

# get text
extract_text_sections <- function(x){
  section_list <- lapply(rvest::html_children(x), rvest::html_text)
  section_list <- lapply(section_list, function(a){
    sub("\n$", "", trimws(a))
  })
  section_list <- section_list[
    which(unlist(lapply(section_list, nchar)) > 0)
  ]
  break_check <- grepl("\n", section_list)
  if(any(break_check)){
    section_split <- base::strsplit(unlist(section_list), "\n")
    section_split <- lapply(section_split, function(a){
      a <- trimws(a)
      a <- a[which(a != "")]
      return(a)
    })
    if(length(section_split) > 1){
      section_names <- unlist(lapply(section_split, function(a){a[[1]]}))
      # names(section_split) <- section_names
      # section_list <- section_split
      section_list <- c(
        section_split[[1]],
        lapply(c(2:length(section_split)), function(a, data){
          as.list(data[[a]][2:length(a)])
        }, data = section_split)
      )
      names(section_list) <- section_names
    }else{
      section_list <- as.list(section_split[[1]])
    }
  }
  if(length(section_list) == 1){
    split_section <- trimws(base::strsplit(section_list[[1]], "\n")[[1]])
    split_section <- split_section[which(split_section != "")]
    return(as.list(split_section))
  }else{
    return(section_list)
  }
}