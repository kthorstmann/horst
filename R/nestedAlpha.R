





# general information ------------------------------------------------

# this function estmates the reliability of scales used in MLM models, following the article from Nezlek, 2016.
# the function runs a simple model with 3 levels (no slopes, intercepts only) to decompose variance.


## further options to include:
# reliability when item is dropped
# reliability when all scales are estmated simultaneously (multivariate case)


#' sample data frame, 10 persons, 10 days with 4 items per day
#' person <- sort(rep(letters[1:10], 40))
#' day <- rep(sort(rep(seq(1:10), 4)), 10)
#' variable <- rnorm(400, 50, 10)
#'data <- data.frame(person, day, variable, stringsAsFactors = FALSE)
#'
#'nestedAlpha(item.level.1 = "variable",
#'            level.2      = "day",
#'            level.3      = "person",
#'            data = data)





# function -----------------------------------------------------------

data = data
item.level.1 = "variable"
level.2      = "day"
level.3      = "person"



nestedAlpha <- function(item.level.1, level.2, level.3, data){

  # make model
  model.string <- stringr::str_c("lme4::lmer(", item.level.1,
                                 " ~ 1 + (1 | ", level.3,
                                 ") + (1 | ", level.2,
                                 "), data = data)")
  # run model
  model <- eval(parse(text = model.string))

  # extract variance:
    # item level reliability (p. 4)
      # level-1 variance:
      var_item_lvl1 <- attr(VarCorr(model), "sc")^2
      # level-2 variance:
      var_occasion_lvl2 <- attr(VarCorr(model)[[2]], "stddev")["(Intercept)"]^2

  # determine length of scale:
  p <- max(table(data$day, data$person))


  # compute item level reliability
  item_alpha <- var_occasion_lvl2/(var_occasion_lvl2 + (var_item_lvl1/p))
  attr(item_alpha, "names") <- "alpha"

  return(item_alpha)
}
