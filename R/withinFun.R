#' withinFun, applying functions on clustered data structure.
#'
#' The function applies a function on a nested data structure. The function applied can be any type of function with one argument. The function uses the function \code{\link[purrr]{map}} and the package \code{\link[stringr]{stringr}}. For how to enter the function, see details.
#'
#' Entering a function. The argument \code{fun} needs to be a character string. It needs to have the following struncture \code{fun = "function(variable), arguments to function"}.
#' \itemize{
#'  \item {\code{variable}} the function to apply, such as \code{mean}, \code{length}, \code{sd}.
#'  \item {\code{function}} the variable to apply the function on. This variable must not be enterd as a string, just as plain text.
#' \item {\code{further arguments to function}} further arguments that are passed to the function, such as \code{na.rm = TRUE} for \code{mean}.
#' }
#'
#' The results can be either of the following, depending on \code{return.data.frame}
#'
#' \itemize{
#'  \item {\code{return.data.frame = TRUE}} This returns the complete data frame with the new variable included. The results of the function and the input data are merged by the \code{id.var}.The new variable has the name \code{variable_function}.
#'  \item {\code{return.data.frame = FALSE}} This returns a list with each entry being an \code{id.var}. This result can be easily used for further analyses.
#'  }
#' @param data An object, a \code{data frame} or a \code{list}.
#' @param id.var The variable in which the scores are clustered, usually an ID variable. Must be a character string.
#' @param fun The function to apply on the scores within the ID variable. Must be entered as a character string. See details for further information.
#' @param return.data.frame Logical, should a data frame be returned, matching the results as a variable on the same level as the ID variable? Default is to \code{FALSE}.
#' @param var.spec Not implemented yet. If a variable cannot be detected, it can be specified here. Default is to \code{NULL}.
#' @param fun.spec Not implemented yet. If a function cannot be detected, it can be specified here. Default is to \code{NULL}.
#'
#' @return Returns a data frame or a list with the results of the function, \code{fun}. Can be either a list or a data frame, depending on the value of \code{return.data.frame}.
#' @export
#'
#' @examples
#' ## make sample data frame
#' variable_1 <- rnorm(100, 1)
#' variable_2 <- rnorm(100, 2)
#' id <- sample(letters[1:5], 100, replace = TRUE)
#' data <- data.frame(id, variable_1, variable_2)
#'
#' ## run function, get length of each ID variable on "variable1", returning a data frame
#' withinFun(data, id.var="id", "length(variable1)", return.data.frame = TRUE)
#'
#' # get the mean of each ID variable on "variable1", returning a list with the entries only
#' withinFun(data, id.var="id", "mean(variable1, na.rm =TRUE)")


withinFun <- function(data, id.var, fun, return.data.frame = FALSE,
                      var.spec = NULL, fun.spec = NULL){
  # basic checks
  stopifnot(is.character(fun))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(id.var))
  stopifnot(is.character(var.spec) | is.null(var.spec))

  stopifnot(is.character(fun.spec) | is.null(fun.spec))

  # detect ids
  unique.id <- as.character(unique(data[, id.var]))

  fun.detect <- stringr::str_split_fixed(fun, pattern = "[:punct:]", n = 2)[1]

  if(!eval(parse(text = stringr::str_c("is.function(", fun.detect, ")")))){
    stop("The function detected is not a known function")
  } else {
    message(stringr::str_c("The detected function was '", fun.detect, "'"))
  }

  var.detect <- stringr::str_split_fixed(fun, pattern = "[:punct:]", n = 3)[2]
  var.spec <- var.detect ## change here the input so that the function continues to run
  pos <- stringr::str_locate_all(fun, var.detect)[[1]]
  stringr::str_sub(fun, pos) <- stringr::str_c("data[data[id.var] == ., var.detect]")
  fun2run <- stringr::str_c("purrr::map(unique.id, ~ ", fun, ")") # one bracket too many, dont know why

  # run function:
  id.results <- eval(parse(text = fun2run))
  names(id.results) <- unique.id

  if(!return.data.frame) {
    return(id.results)
  }
  if (return.data.frame) {
    r.df <- data.frame(id = unique.id, results = unlist(id.results))
    colnames(r.df) <- c(id.var, stringr::str_c(var.detect, fun.detect, sep="_"))
    return(merge(data, r.df, by  = id.var))
  }
}

