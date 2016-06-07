
# to dos in function -------------------------------------------------

# function should take a data frame and a vector

# within the data frame, two colums are selected, one that is the indicator and one that is the return value

## TESTTHAT
# - order is correct when units return.

# match could also be character, this must work as well, need to be checked
# add also an quiet = TRUE element, that suppresses warnings.
# add then onexit argument, that resets the options
# see here the slides

## add checks
# Ändeurasd


#' vlookup
#'
#' This function is an implementation of the vlookp function from Excel. It is used to return values from a column in a data frame, which correspond to certain values in another column in the same data frame. These other values are taken as input for the function.
#'
#' @param data A data frame in which the values should be looked up
#' @param look.for The values to look for in the column \code{look.in} in the data frame.
#' @param look.in The column to which the values from \code{look.for} are matched.
#' @param return.from The column from which the corresponding values are returned.
#' @param return.multiple If one value from \code{look.for} would return multiple values from \code{return.from}, should these multiple values be returned? Default is to FALSE.If set to FALSE and multiple matches occur, an error is returned.
#' @param return.na Should missing values be returned? Defaul is to TRUE.
#'
#' @return A vector with the corresponding values from the column \code{return.from}. Length must not be the same as \code{look.for}, but usually is if \code{return.multiple = FALSE} and \code{return.na = TRUE} and all values from \code{look.for} are in \code{look.in}.
#' @export
#'
#' @examples
#' data <- data.frame(a = c(1, 2, 3, 5, 5, 3, NA, 4, 4),
#' b = c("A", "B", "C", "D", "D", "E", "F", "D", NA),
#' stringsAsFactors = FALSE)
#' vlookup(data, look.for = c(1, 5), look.in = "a", return.from = "b", return.multiple=TRUE, return.na = TRUE)


vlookup <- function(data, look.for, look.in,
                    return.from, return.multiple = FALSE,
                    return.na = TRUE){

  stopifnot(is.logical(return.multiple))

  ## replace these two in function, but only with version control ;)
  indicator <- look.in
  return.col <- return.from

  ## CHECKS
  # check that indicator and return are only one variable each.

  # make checks on data frame or matrix

  # check that indicator and return are parts of data

  # check that there are only unique values, give error when values are doubled.

  ## function body:
  if (length(look.for) != length(unique(look.for))) {
    message("Some duplicates from `look.for` were removed")
    look.for <- unique(look.for)
  }
  pos.of.matches <- match(look.for, data[, indicator])

  # check if duplicates in look.for would return different results:
  if (return.multiple) {
    returns.all <- purrr::map(look.for, ~ data[. == data[ ,indicator],
                                               return.col])
    return <- unlist(returns.all)
    if (length(return) != length (look.for)) {
      message("duplicate return values are returned, i. e. length of look.for is not length of return. To avoid this, set `return.multiple = FALSE`")
    }

  } else {
    # first condition, check that there is a double look.for
    matches.freq <- purrr::map(look.for, ~ table(. == data[indicator]))
    matches.often <- purrr::map(matches.freq, ~ .["TRUE"] > 1)

    if (any(unlist(matches.often))) {
      double.match <- look.for[matches.often  == TRUE]
      would.return <- purrr::map(double.match, ~
                                   data[data[, indicator] == ., return.col])
      # these would be returned:
      number.returns <- purrr::map(would.return, table)

      # any dublicate returns?
      return.too.much <- double.match[purrr::map(number.returns, length) > 1]

      # this would be the ones that returned too much
      if (length(return.too.much) != 0) {
        stop("the following values would return more than one unique value from `return:\n", list(return.too.much))
      }
    }
    return <- data[pos.of.matches, return.col]
  }

  if (!return.na) {
    return <- return[!is.na(return)]
  }
    return
}



