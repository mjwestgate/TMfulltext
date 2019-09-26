\name{process_ocr}
\alias{process_ocr}
\title{Process PDFs using OCR}
\description{This function takes a PDF and does several things: 1. convert each page to png at 300 dpi; 2. scan using tesseract; 3. reimport resulting .txt files; 4. combines and re-exports as a single txt file.}
\usage{
process_ocr(filename, temp_path = "./data_processed/temp",
  save_path = "./data_processed", keep_levels = 2)
}
\arguments{
  \item{filename}{A PDF file to process}
  \item{temp_path}{location of a temporary file in which to store intermediate files}
  \item{save_path}{location where .txt file should be saved}
  \item{keep_levels}{how many levels of file structure should be retained. Currently ignored}
}
\note{The ROpenSci package 'tesseract' is not used because of apparent incapacity to set the 'psm' parameter, which is necessary for recognising multi-column text. Instead this version called the system version of tesseract directly via the 'system' function.}
\value{
a single .txt file in the specified file location
}