
#' Modus of a variable
#'
#' Get the modus of a variable, i. e. the value that is most frequent in an variable (can be numeric and character)
#'
#' If two (not all) values are equally often in the variable, the first value of these will be returned. A message is issued.
#'
#' @param variable The variable containing
#' @param useNA Should NAs be used? Will be passed on to \code{\link{table}}. Can be either \code{"no"}, \code{"ifany"}, or \code{"always"}.
#' @param equal.dist If all entries are equally often, this value will be returned as output. Default is to 99.
#'
#' @return The modus will be returned.
#' @export
#'
#' @examples
#' letter_samp <- sample(letters, 50, TRUE)
#' modus(letter_samp)
#' ##
#' set.seed(9)
#' m <- matrix(sample(1:5, 30, replace=TRUE), 10, 3, byrow=TRUE)
#' m[c(23, 12)] <- NA
#' m[c(4)] <- 1 # no variance
#' data <- as.data.frame(m)
#' apply(data, 1, modus)
#' ##
#' # message, since 2 and 3 are equally often.
#' modus(c(2, 2, 2, 3, 3, 3, 4, 4))

modus <- function(variable, useNA = "no", equal.dist = 99){
  variable <- unlist(variable)
  numeric_in <- is.numeric(variable)
  table <- table(variable, useNA = useNA)

  # bei gleichverteilung eine 5
  if (length(table) == 1) {
    return.value <- names(which.max(table))
    } else {
      if (var(table, na.rm = TRUE) == 0) {
        return.value <- equal.dist
        } else {
          return.value <- names(which.max(table))
        }
    }

  if (sum(table == table[return.value]) > 1) {
    message("The value returned is not the only value with this frequency. Please check data.")
  }

  if (numeric_in) {
    return.value <- as.numeric(return.value)
  }
  return.value
}


