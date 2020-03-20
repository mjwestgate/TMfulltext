\name{read_fulltext_html}
\alias{read_fulltext_html}
\title{Parse full text articles given as HTML}
\description{This function takes a link or file, and parses the resulting HTML into a list where section headings are converted to list entries.}
\usage{
read_fulltext_html(x)
}
\arguments{
  \item{x}{A HTML file}
}
\value{
A list of length equal to the number of sections in the source document (identified by <h2> tags). Each list entry is a character vector; each entry in the vector is a paragraph from the article text (identified by <p> tag).
}
\examples{
article <- read_fulltext_html("https://doi.org/10.1371/journal.pone.0140973")
}