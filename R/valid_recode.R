

# function todos -----------------------------------------------------

# include checks
  # no vectors in the varibale

# add tests

# function -----------------------------------------------------------

#' Recode values to 0 and 1 according to predefined vector of valid elements
#'
#' The function recodes a vector of elelemnts into 0 and 1, based on a vector of valid values. It assumes that any correct answer in a questionnaire will be coded as 1 and all other answers (per default, even \code{NA}) will be returned as 0.
#'
#' @param variable The variable that should be recoded to 0 and 1, must be a vector.
#' @param valid_values The vector of values that are valid, i. e. will be returned as 1.
#' @param keep.na Logical, should missings be kept as \code{NA}? Default is to \code{FALSE}, returning 0 for a missing value.
#'
#' @return A vector containing 0 and 1 (and \code{NA} if specified) of type \code{integer}.
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(var1 = c("7", NA, "five", "nine", "eight", "six",
#'                    "x", "X", "100", "50", " five", "fife",
#'                    "5", " Five"),
#'                    var2 = c(1, 2, 3, 4, 1, NA, 9, 1, 14, 15, 0, 1, 3, 6),
#'                    id = c(1:14),
#'                    stringsAsFactors = FALSE)
#' valid_recode(variable = data$var1,
#'                            valid.values = c("five", "Five", "5"))
#' valid_recode(variable = data$var2, valid.values =  c(1, 2), keep.na = TRUE)

valid_recode <- function(variable, valid.values, keep.na = FALSE){
  # remove whitespace
  var_trimmed <- stringr::str_trim(variable)

  if (!typeof(variable) == typeof(valid.values)) {
    stop ("both objects must be the same type, e.g. both character")
  }
  hit_correct <- is.element(var_trimmed, valid.values)

  # replace NA if requested
  if (keep.na) {
    hit_correct[is.na(var_trimmed)] <- NA
  }

  hit_int <- as.integer(hit_correct)
  hit_int
}


