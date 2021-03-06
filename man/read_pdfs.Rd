% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_functions.R
\name{read_pdfs}
\alias{read_pdfs}
\title{Import multiple pdfs}
\usage{
read_pdfs(x, clean_columns = TRUE, cache_dir)
}
\arguments{
\item{x}{string (or vector of strings) giving file locations}

\item{clean_columns}{logical - should columns get cleaned? Defaults to TRUE}

\item{cache_dir}{optional location to store temporary .rds files.
If not present then text is returned instead}
}
\value{
a list of same length as x
}
\description{
Takes a vector of file names and imports them. Basically a wrapper to
pdftools::pdf_text
}
