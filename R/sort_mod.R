





#' Sorted Modification Indices from lavaan objects
#'
#' @param fit A fitted lavaan object.
#' @param cut A cut.off for modification indices to display. The default value is 10.
#'
#' @return A data frame with sorted modification indices for any fitted lavaan object
#' @export
#'
#' @examples
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' fit <- lavaan::cfa(HS.model, data=lavaan::HolzingerSwineford1939)
#' sort_mod(fit, cut = 10)

sort_mod  <- function(fit, cut = 10){
  MI  <-  lavaan::modindices(fit)
  MI  <-  MI[order(-MI$mi),]
  subset(MI, mi>cut)
}

