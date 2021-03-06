% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_management.R
\name{get_file_names}
\alias{get_file_names}
\title{Get names of files within a set of folders}
\usage{
get_file_names(path, suffix)
}
\arguments{
\item{path}{A path within which to locate files. Defaults to the working
directory}

\item{suffix}{Optional suffix to allow returning of selected file types
(e.g. "pdf")}
}
\value{
A character vector listing all files in the top and any subfolders
}
\description{
Basically a slightly more advanced version of \code{list.files} for nested
data structures. This is a pretty basic implementation, but should work to 2
levels of nestedness.
}
