
# sortMixedTypes() {{{1 -------------------------------------------------------
#' @title       sort a vector of mixed types
#' @description this will sort an alphanumeric vector where everythign is
#'              stored as a string
#' @details     this function was adapted from gtools::mixedsort()
#' @param       x the vector
#' @param       decreasing should values be returned in decreasing order
#' @return      a sorted alphanumeric vector
sortMixedTypes <- function(x, decreasing = TRUE) { # {{{2 ---------------------
  if (!is.vector(x)) {
    message("Unable to operate over something that's not a vector")
    return(NULL)
  }
  if (length(x) < 1) return(NULL)
  else if (length(x) == 1) return(1)
  if (!is.character(x)) return(order(x, decreasing=decreasing))
  delim <- '\\$\\@\\$'
  regex <- paste0("((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])",
                  "(?:[0123456789]*)(?:(?:[.])",
                  "(?:[0123456789]{0,}))?)",
                  "(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))")
  numeric <- function(x) as.numeric(x)
  nonnumeric <- function(x) ifelse(is.na(numeric(x)), toupper(x), NA)
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == '')
  delimited <- gsub(regex, paste(delim, '\\1', delim, sep = ""), x,
                    perl = TRUE)
  vec <- strsplit(delimited, delim)
  vec <- lapply(vec, function(x) x[x > ""])
  # create numeric version of the data
  suppressWarnings(vec.num <- lapply(vec, numeric))
  # create non-numeric version of data
  suppressWarnings(vec.chr <- lapply(vec, nonnumeric))
  # transpose so that 1st vector contains 1st element from each original str
  max.elements <- max(sapply(vec, length))
  vec.num.t <- lapply(1:max.elements, function(i) sapply(vec.num,
                                                         function(x) x[i]))
  vec.chr.t <- lapply(1:max.elements, function(i) sapply(vec.chr,
                                                         function(x) x[i]))
  # now order them
  rank.num <- sapply(vec.num.t, rank)
  rank.chr <- sapply(vec.chr.t, function(x) as.numeric(factor(x)))
  # merge them together
  rank.num[!is.na(rank.chr)] <- 0 # mask off string values
  rank.chr <- t(t(rank.chr) + apply(matrx(rank.num), 2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.chr), rank.num, rank.chr)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) order.frame[which.nas,] <- NA
  if (length(which.blanks) > 0) order.frame[which.blanks,] <- NA
  order.frame <- as.list(order.frame)
  order.frame$decreasing = decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
} # 2}}} 1}}} -----------------------------------------------------------------
