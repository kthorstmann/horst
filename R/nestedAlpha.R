
## further options to include:
  # reliability when item is dropped
  # reliability when all scales are estmated simultaneously (multivariate case)

## further todos
  # include some checks
  # add tests for the function

## add checks
  # 1 all variables are %in% names(data)
  # 2 data == dataframe
  # 3 variance in level1 > level2 > level3, else, warning
  # 4 number of items is mostly equal

## return optionally
  # model that was run (code)
  # optional: results of model (fitted model)


# function -----------------------------------------------------------

#' nestedAlpha
#'
#' This function returns a nested alpha for multilevel data as proposed by Nezlek (2016).
#'
#' Nezlek, 2016, describes one way in that the internal consistency as a measure for reliabilty can be obtained for longitudinal data. The data structure for these types of examinations usually is items (for example all items of one scale) nested in days (for example 10 days of a diary study), which are again nested in persons (all subjects).
#'
#' To estimate the reliability, one needs to consider the nested structure.
#'
#'
#'
#'
#' Please consider the following points before running the analysis:
#' \itemize{
#' \item {Coding of items} As is also the case with Alpha, all items must be coded in the direction of the construct.
#' \item {All items are weighted equally}
#' \item {All items are assumed to be parallel (i. e. equal error-variances, equal loadings, and unidimensional)}
#' \item {The variable on level-1 should not be standardized or transformed, but the raw scores}
#' }
#'
#' The function relies on the \code{\link[lme4]{lmer}} function in order to run the multilevel model. The multilevel model is run with standard settings, however, these can be changed using the \code{...}-argument.
#'
#' @param item.level.1 The variable that includes the items on level 1.
#' @param level.2 The variable that includes the nests on level 2, for example days or measurement occasions in which the items are nested.
#' @param level.3 The cluster in which the days are clustered, usually persons, groups, classes.
#' @param data The data frame that contains all of the above variables.
#' @param ... Other parameters that can be passed on to \code{\link[lme4]{lmer}}.
#'
#' @return
#' An alpha for nested data.
#'
#' @author K. T. Horstmann \email{horstmak@@hu-berlin.de}
#'
#'@references
#' Nezlek, J. B. (in press). A practical guide to understanding reliability in studies of within-person variability. \emph{Journal of Research in Personality}. \url{http://dx.doi.org/10.1016/j.jrp.2016.06.020.}
#'
#' @export
#'
#' @examples
#' sample data frame, 10 persons, 10 days with 4 items per day
#' person <- sort(rep(letters[1:10], 40))
#' day <- rep(sort(rep(seq(1:10), 4)), 10)
#' variable <- rnorm(400, 50, 10)
#' data <- data.frame(person, day, variable, stringsAsFactors = FALSE)
#'
#' nestedAlpha(item.level.1 = "variable",
#'             level.2      = "day",
#'             level.3      = "person",
#'             data         = data)

nestedAlpha <- function(item.level.1, level.2, level.3, data, ...){

  # make model
  model.string <- stringr::str_c("lme4::lmer(", item.level.1,
                                 " ~ 1 + (1 | ", level.3,
                                 "/", level.2,
                                 "), data = data, ...)")

  # run model
  model <- eval(parse(text = model.string))

  # extract variance:
    # item level reliability (p. 4)
      # level-1 variance:
      var_item_lvl1 <- attr(lme4::VarCorr(model), "sc")^2
      # level-2 variance:
      var_occasion_lvl2 <- attr(lme4::VarCorr(model)[[1]], "stddev")["(Intercept)"]^2

  # determine length of scale:
  p <- max(table(data[,level.2], data[,level.3]))

  # compute item level reliability
  item_alpha <- var_occasion_lvl2/(var_occasion_lvl2 + (var_item_lvl1/p))
  attr(item_alpha, "names") <- "alpha"

  return(item_alpha)

}
