





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
#' fit <- lavaan::cfa(HS.model, data=lavaan::HolzingerSwineford1939, estimator = "MLR")
#' sort_mod(fit, cut = 10)

sort_mod  <- function(fit, cut = 10){
  MI  <-  lavaan::modindices(fit)
  MI  <-  MI[order(-MI$mi),]
  subset(MI, mi>cut)
}





#' Get fitmeasures from lavaan objects as text.
#'
#' This function extracts the most common used fit measures in a text format, such that they are easy to extract and copy/edit in a word document. \strong{The default extracts are the robust fitmeasures}.
#'
#' Further implementations:
#'
#'  - specify fit.measures to extract
#'  - extract in order to plot under a plot
#'  - extract as latex
#'  - extract as markdown
#'
#' @param fit A fitted lavaan object.
#' @param round The number of digits to round to.
#' @param extract.in The format to extract in, only form is currently word.
#'
#' @return The function returns a text with the Chisq-value, the corresponding \emph{df} and \emph{p}-value, the CFI, RMSEA and SRMR. Best used for objects fitted with \code{estimator = "MLR"}.
#' @export
#'
#' @examples
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' fit <- lavaan::cfa(HS.model, data=lavaan::HolzingerSwineford1939, estimator = "MLR")
#' lav_fit(fit, 2)
lav_fit <- function(fit, round = 2, extract.in = "word"){
  fits <- lavaan::fitMeasures(fit)
  fits <- round(fits, round)
  chi_sq <- fits["chisq.scaled"]
  df <- fits["df.scaled"]
  p <- fits["pvalue.scaled"]
  cfi <- fits["cfi.scaled"]
  srmr <- fits["srmr_bentler"]
  rmsea <- fits["rmsea.scaled"]
  rmsea.lo <- fits["rmsea.ci.lower.scaled"]
  rmsea.up <- fits["rmsea.ci.upper.scaled"]


  if (p <= 0.001) {
    p_operator <- "<"
    p_value <- .001
  } else
    if(p <= 0.01){
      p_operator <- "<"
      p_value <- .01
    } else
      if(p <= 0.05){
        p_operator <- "<"
        p_value <- .05
      } else {
        p_operator <- "="
        p_value <- p
      }

  chiq.txt <- paste0("X = ", chi_sq, " (", df, "), ", "p ", p_operator, " ",
                     p_value)
  cfi.txt <- paste0("CFI = ", cfi)
  rmsea.txt <- paste0("RMSEA = ", rmsea, ", 95% CI [", rmsea.lo, "; ", rmsea.up, "]")
  srmr.txt <- paste0("SRMR = ", srmr)

  out.text <- paste(chiq.txt, cfi.txt, rmsea.txt, srmr.txt, sep = "; ")
  out.text
}


