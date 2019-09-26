\name{run_tesseract}
\alias{run_tesseract}
\title{Run Tesseract}
\description{Process and image using tesseract via a call to 'system'. Clearly, this requires that tesseract is already installed on your system.}
\usage{
run_tesseract(file, file_out, args = "--psm 1")
}
\arguments{
  \item{file}{An image}
  \item{file_out}{Where should the .txt file be saved?}
  \item{args}{Additional arguments passed to tesseract}
}
\value{
A .txt file.
}