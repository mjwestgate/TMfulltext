#' Run Tesseract
#'
#' Process and image using tesseract via a call to 'system'. This requires that
#' tesseract is already installed on your system.
#'
#'
#' @param file An image
#' @param file_out Where should the .txt file be saved?
#' @param args Additional arguments passed to tesseract
#' @return A .txt file.
#' @export run_tesseract
run_tesseract <- function(
  file,
  file_out,
  args = "--psm 1"
){
  file_out <- sub(".png", "", file)
  script <- paste(
    "tesseract",
    file,
    file_out,
    args,
    sep = " "
  )
  system(script)
}


# function to take a scanned document and
  # convert each page to png at high res
  # scan using tesseract via system version (not R package) to set multi-column parameters
  # re-import text files to entries of a list
  # save list as a .rds in a 'processed_data' folder

# note that ROpenSci package tesseract is not used because of apparent incapacity to set
# the 'psm' parameter, which is necessary for recognising multi-column text
# issue of how to recognise two columns is raised here:
# https://stackoverflow.com/questions/31651071/how-to-ocr-multiple-column-in-a-document-using-tesseract


#' Process PDFs using OCR
#'
#' This function takes a PDF and does several things: 1. convert each page to
#' png at 300 dpi; 2. scan using tesseract; 3. reimport resulting .txt files;
#' 4. combines and re-exports as a single txt file.
#'
#'
#' @param filename A PDF file to process
#' @param temp_path location of a temporary file in which to store intermediate
#' files
#' @param save_path location where .txt file should be saved
#' @param keep_levels how many levels of file structure should be retained.
#' Currently ignored
#' @return a single .txt file in the specified file location
#' @note The ROpenSci package 'tesseract' is not used because of apparent
#' incapacity to set the 'psm' parameter, which is necessary for recognising
#' multi-column text. Instead this version called the system version of
#' tesseract directly via the 'system' function.
#' @export process_ocr
process_ocr <- function(
  filename,
  temp_path = "./data_processed/temp",
  save_path = "./data_processed",
  keep_levels = 2
){

  # work out name of output file
  file_split <- strsplit(filename, "/")[[1]]
  file_keep <- rev(rev(file_split)[seq_len(keep_levels)])
  file_out <- paste(
    save_path,
    paste(file_keep, collapse = "/"),
    sep = "/"
  )
  file_out <- sub(".pdf", ".txt", file_out)

  # work out where to put images
  output_filenames <- paste0(
    temp_path,
    "/page",
    formatC(
      seq_len(pdftools::pdf_info(filename)$pages),
      width  = 2,
      format = "d",
      flag = 0
    )
  )

  # run pdf conversion
  pdftools::pdf_convert(
    filename,
    format = "png",
    dpi = 300,
    verbose = FALSE,
    filenames = paste0(output_filenames, ".png") # ditto
  )

  # run tesseract on all files
  invisible(lapply(
    output_filenames,
    function(a){run_tesseract(file = paste0(a, ".png"), file_out = a)}
  ))

  # run_tesseract(
  #   file = paste0(output_filenames, ".png"),
  #   file_out = output_filenames
  # )

  # import and save as rds
  text_list <- lapply(output_filenames, function(a){
    read_text(paste0(a, ".txt"))
  })

  # attempt to create nested folders for keep_levels != 2
  # save_folders <- rep("", keep_levels)
  # current_path <- save_path
  # for(i in seq_len(keep_levels)){
  #   current_path <-  paste0(current_path, "/", file_keep[i])
  #   save_folders[[i]] <- current_path
  # }
  # not important for now

  # just create single folder levels
  folder_path <- paste0(save_path, "/", file_keep[1])
  if(!dir.exists(folder_path)){
    dir.create(folder_path)
  }

  # export vector as an rds for later processing
  writeLines(unlist(text_list), con = file_out)
  # saveRDS(unlist(text_list), file_out)

  # remove temporary files
  remove_files <- paste0(temp_path, "/", list.files(temp_path))
  invisible(lapply(remove_files, function(a){file.remove(a)}))

}
