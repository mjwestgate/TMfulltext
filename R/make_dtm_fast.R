# This implementation has a few features that are beneficial for large corpora
  # 1. uses {ngram} for calculating word sums
  # 2. calculates word frequencies for whole corpus first, then only returns common terms
    # this s much easier than creating a full dtm then subsetting
  # allows parallelization of dtm construction via {parallel}

#' Construct a document-term matrix (DTM) quickly
#'
#' Takes bibliographic data and converts it to a DTM for passing to topic
#' models. It is currently experimental, but uses a few tricks to increase speed:
#'
#' 1. uses {ngram} for calculating word sums
#' 2. calculates word frequencies for whole corpus first, then only returns common terms
#' 3. supports parallelisation via {parallel}
#'
#'
#' @param x a vector or \code{data.frame} containing text
#' @param stop_words optional vector of strings, listing terms to be removed
#' from the DTM prior to analysis. Defaults to \code{revwords()}.
#' @param min_freq minimum proportion of entries that a term must be found in
#' to be retained in the analysis. Defaults to 0.01.
#' @param max_freq maximum proportion of entries that a term must be found in
#' to be retained in the analysis. Defaults to 0.85.
#' @param bigram_check logical: should ngrams be searched for?
#' @param bigram_quantile what quantile of ngrams should be retained. Defaults
#' to 0.8; i.e. the 80th percentile of bigram frequencies after removing all
#' bigrams with frequencies <=2.
#' @param retain_empty_rows logical: should the function return an object with
#' the same length as the input string (TRUE), or remove rows that contain no
#' text after filtering rare & common words etc? (FALSE, the default). The
#' latter is the default because this is required by \code{run_topic_model}.
#' @return An object of class \code{simple_triplet_matrix}, listing the terms
#' (columns) present in each row or string.
#' @seealso \code{\link{run_topic_model}}, \code{\link{screen_topics}}
#'
#' @export make_dtm_fast

make_dtm_fast <- function(
  x, # a vector of strings
  stop_words,
  merge_stems = TRUE,
  parallel = FALSE,
  n_cores,
  verbose = FALSE
){

  # clean up text using tm functions
  if(verbose){cat("Cleaning text data\n")}
  x <- clean_string(x)

  if(verbose){cat("Getting corpus-wide term frequencies\n")}
  ngram_single <- get_corpus_word_frequencies(x)

  if(verbose){cat("Removing unnecessary terms\n")}
  ngram_sub <- subset_keywords(ngram_single, stop_words = stop_words)

  if(verbose){cat("Calculating dtm\n")}
  dtm <- make_dtm_ngram(x,
    keep_words = ngram_sub$ngrams,
    parallel = parallel,
    n_cores = n_cores)

  if(merge_stems){
    if(verbose){cat("Merging stems\n")}
    return(merge_stems(dtm))
  }else{
    return(dtm)
  }
}

# function to construct dtm using ngram rather than tm
make_dtm_ngram <- function(
  x,
  keep_words,
  parallel = TRUE,
  n_cores
){
  # error catching
  # inherits(x, "character")
  if(missing(keep_words)){stop("keep_words is missing, with no default")}

  # evaluate word frequencies using lapply
  if(parallel){
    if(missing(n_cores)){
      n_cores <- parallel::detectCores() - 1
    }

    # or parallelize using parallel::parLapply
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, "repeat_ngram")
    invisible(parallel::clusterEvalQ(cl, library(ngram)))
    ngram_list <- parallel::parLapply(
      cl,
      X = seq_along(x),
      fun = function(a, string, lookup){
        repeat_ngram(a, x = string, lookup = lookup)
      },
      string = x,
      lookup = keep_words
    )
    parallel::stopCluster(cl)

  }else{
    ngram_list <- lapply(
      seq_along(x),
      function(a, string, lookup){repeat_ngram(a, x = string, lookup = lookup)},
      string = x,
      lookup = keep_words
    )
  }

  # convert to df then triplet matrix
  ngram_df <- do.call(rbind, ngram_list)
  ngram_df <- ngram_df[!is.na(ngram_df$ngrams), ]
  word_factor <- as.factor(ngram_df$ngrams)
  dtm <- slam::simple_triplet_matrix(
    i = as.integer(ngram_df$entry), # articles
    j = as.integer(word_factor), # words
    v = as.integer(ngram_df$freq),  # counts
    # nrow = as.integer(length(unique(ngram_df$entry))),
    # nrow = as.integer(max(ngram_df$entry)), # fails when there are removed, empty rows
    # ncol = as.integer(length(levels(word_factor))),
    dimnames = list(
      "Docs" = as.character(seq_len(max(ngram_df$entry))),
      "Terms" = as.character(levels(word_factor))
    )
  )
  return(dtm)
}


# function to call ngram on a single entry
# note this calls from a vector by index so we can glue this together with lapply
repeat_ngram <- function(
  a, # number
  x, # vector of strings
  lookup # terms to subset with
){
  empty_df <- data.frame(ngrams = NA, freq = NA, entry = a)
  if(is.na(x[a])){
    return(empty_df)
  }else{
    if(nchar(x[a]) < 10){
      return(empty_df)
    }else{
      ngram_tr <- ngram::get.phrasetable(ngram::ngram(x[a], 1))[, 1:2]
      ngram_tr$ngrams <- sub("\\s$", "", ngram_tr$ngrams)
      keyword_lookup <- ngram_tr$ngrams %in% lookup
      if(any(keyword_lookup)){
        ngram_tr <- ngram_tr[keyword_lookup, ]
        ngram_tr$entry <- a
        return(ngram_tr)
      }else{
        return(empty_df)
      }
    }
  }
}


# NOTE: This is taken direct from synthesisr on 2020-02-27
# there might be a way to parallelize this code as well
merge_stems <- function(dfm) {
  if (!class(dfm) %in% c("simple_triplet_matrix")) {
    stop("merge_stems only accepts objects of class simple_triplet_matrix")
  }

  stem_terms <- SnowballC::wordStem(dfm$dimnames$Terms)
  lookup <- data.frame(
    initial_n = seq_along(dfm$dimnames$Terms),
    initial = (dfm$dimnames$Terms),
    stemmed = SnowballC::wordStem(dfm$dimnames$Terms),
    stringsAsFactors = FALSE
  )
  dtm_df <- data.frame(i = dfm$i, j = dfm$j, v = dfm$v)

  if(base::anyDuplicated(lookup$stemmed) > 0) {
    lookup$n <- nchar(lookup$initial)
    text_split <-
      split(lookup[, c("initial_n", "n")], lookup$stemmed)
    text_match <- data.frame(
      initial_n = unlist(lapply(text_split, function(a){a$initial_n})),
      final_n = unlist(lapply(text_split, function(a){
        if(nrow(a) > 1) {
          rep(a$initial_n[order(a$n, decreasing = FALSE)[1]], nrow(a))
        }
        else{
          a$initial_n
        }
      })))
    lookup$final_n <- text_match$final_n[order(text_match$initial_n)]
    dtm_df$j_new <- lookup$final_n[dtm_df$j]
    dtm_list <- split(dtm_df[c("j_new", "v")], dtm_df$i)
    name_lookup <- as.numeric(names(dtm_list))
    dtm_df2 <- do.call(rbind, lapply(
      seq_along(dtm_list),
      function(a, data) {
        result <- unlist(lapply(split(data[[a]]$v, data[[a]]$j_new), sum))
        return(data.frame(
          i = as.integer(a),
          j = as.integer(names(result)),
          v = as.integer(result),
          stringsAsFactors = FALSE))
        }, data = dtm_list))
    unique_j <- sort(unique(lookup$final_n))
    lookup2 <- data.frame(
      index = seq_len(max(lookup$final_n)),
      end = NA,
      stringsAsFactors = FALSE)
    lookup2$end[unique_j] <- seq_along(unique_j)
    dfm$i <- as.integer(name_lookup[dtm_df2$i])
    dfm$j <- as.integer(lookup2$end[dtm_df2$j])
    dfm$dimnames$Terms <- lookup$initial[sort(unique(lookup$final_n))]
    dfm$v <- as.integer(dtm_df2$v)
    dfm$ncol <- length(unique(dfm$j))
  }
  return(dfm)
}


# sub-functions for create_dtm_fast
clean_string <- function(x){
  x <- tolower(x)
  x <- gsub(" - ", " ", x) # remove separated dashes
  x <- tm::removePunctuation(x,
    preserve_intra_word_contractions = FALSE,
    preserve_intra_word_dashes = TRUE
  )
  # replace inter-word dashes (i.e. dashes that follow a word) with space
  x <- gsub("-(?=[[:alnum:]]+)", " ", x, perl = TRUE)
  x <- tm::removeNumbers(x)
  return(x)
}


get_corpus_word_frequencies <- function(x){
  # create a single string version of x to allow calculation of word frequencies
  # across all texts
  x_string <- paste(x, collapse = "\n")
  x_string <- gsub("\\n", " ", x_string)
  x_string <- gsub("\\s{2,}", " ", x_string)

  # get a list of all terms that appear in any entry of x
  ngram_single <- ngram::get.phrasetable(ngram::ngram(x_string, 1))[, 1:2]
  # note: this could probably be sped up by parralelising

  # return cleaned list of terms
  ngram_single$ngrams <- gsub("\\s$", "", ngram_single$ngrams) # remove trailing spaces
  ngram_single$ngrams <- gsub("[^a-zA-Z]", "", ngram_single$ngrams) # keep only standard letters
  ngram_single <- ngram_single[ngram_single$ngrams != "", ]
  ngram_unique <- as.data.frame(do.call(rbind, lapply(
    split(ngram_single, ngram_single$ngrams),
    function(a){
      if(nrow(a) > 1){
        b <- a[1, ]
        b$freq <- sum(a$freq)
        return(b)
      }else{
        return(a)
      }
    }
  )), stringsAsFactors = FALSE)

  return(ngram_unique)
}

# exclude unwanted terms
# note: doing this iteratively isn't particularly tidy;
# but minimizes compute time by only running checks as they are needed
# also computes the simplest extractions first
subset_keywords <- function(
  x, # ngram table as returned by get_corpus_word_frequencies
  min_freq = 10,
  min_char = 4,
  stop_words
){
  if(missing(stop_words)){
    stop_words <- revtools::revwords()
  }
  # by frequency (i.e. remove rare terms)
  exclude_freq <- x$freq < min_freq
  excluded_words <- x$ngrams[exclude_freq]
  ngram_sub <- x[!exclude_freq, ]
  # short words (i.e. few characters)
  exclude_nchar <- nchar(ngram_sub$ngrams) < min_char
  excluded_words <- c(excluded_words, ngram_sub$ngrams[exclude_nchar])
  ngram_sub <- ngram_sub[!exclude_nchar, ]
  # stopwords
  exclude_stop <- ngram_sub$ngrams %in% stop_words
  excluded_words <- unique(c(excluded_words, ngram_sub$ngrams[exclude_stop]))
  ngram_sub <- ngram_sub[!exclude_stop, ]
  return(ngram_sub)
}