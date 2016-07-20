



# title --------------------------------------------------------------


# function for computing whatever function within in a level-2 variable in any data set


# function name

# withinFun




## example

variable1 <- rnorm(100, 1)
variable2 <- rnorm(100, 2)
id <- sample(letters[1:5], 100, replace = TRUE)
data <- data.frame(id, variable1, variable2)

var.spec = NULL
fun.spec = NULL
return.data.frame = FALSE
id.var <- "id"
fun <- "mean(variable1, na.rm = TRUE)"

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

  ## automatically detecting function and variable:
  if (is.null(fun.spec)) {
    # extract variable from fun
    fun.detect <- stringr::str_split_fixed(fun, pattern = "[:punct:]", n = 2)[1]

    if(!eval(parse(text = stringr::str_c("is.function(", fun.detect, ")")))){
      stop("The function detected is not a known function")
    } else {
      message(stringr::str_c("The detected function was '", fun.detect, "'"))
    }

    fun.spec <- fun.detect ## change here the input so that the function continues to run, overwrite zero
  } else {
    fun.detect <- fun.spec
  }

  if(is.null(var.spec)){
    var.detect <- stringr::str_split_fixed(fun, pattern = "[:punct:]", n = 3)[2]
    var.spec <- var.detect ## change here the input so that the function continues to run

    ## message that the variable detected is a certain varable
    ## check that the variable is part of the data frame

  } else {
    var.detect <- var.spec
  }

  pos <- stringr::str_locate_all(fun, var.detect)[[1]]
  stringr::str_sub(fun, pos) <- stringr::str_c("data[data[id.var] == ., var.detect]")
  fun2run <- stringr::str_c("purrr::map(unique.id, ~ ", fun, "))") # one bracket too many, dont know why

  # run function:
  library(purrr)
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

withinFun(data, id.var="id", "length(variable1", return.data.frame = TRUE)







