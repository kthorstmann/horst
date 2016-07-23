


#' tStats as text
#'
#' Get the \emph{t}-statistics from the object of \code{t.test}.
#'
#'
#' @param t.fit The object containing the result of \code{t.test}
#' @param round Number of decimals to which the test-statistic is rounded.
#' @param out.text The format of the output text. Currently available are
#' \itemize{
#' \item {\code{"plain"}} for plain text output (needs only manual editing for word, for example)
#' \item {\code{"plot"}} for an output that can be used as input for example in \code{text}.
#' \item {\code{"latex"}} will follow for easy use in markdown.
#' }
#' @param eff.size Effect size that can be inserted manually (see \code{\link[compute.es]{tes}} for the computation of an effect size for a \emph{t}-value). Default is to \code{NULL}.
#' @param eff.tpye Name of the effect size. Must be character. Default is to \code{NULL}.
#'
#' @return Returns a character string with the \emph{t}-statistics of a t-test.
#' @export
#' @author Kai T. Horstmann
#'
#'
#' @examples
#' fit <- t.test(1:10, y = c(7:20))
#' boxplot(1:10, c(7:20))
#' mtext(side = 1, line = 2, tStats(fit, out = "plot"))
#' mtext(side = 3, line = 2, tStats(fit, out = "plot", eff.size = -2.29, eff.type = "d"))


tStats <- function(t.fit, round = 2, out.text = "plain",
                   eff.size = NULL, eff.type = NULL){
  if (!is.numeric(round)) {stop("'round' must be numeric")}
  if(!is(t.fit) == "htest") {
    stop ("Input must be fitted t-test from function 't.test'")
  }
  if(!is.null(eff.size) ) {stopifnot(is.numeric(eff.size))}
  if(!is.null(eff.type) ) {stopifnot(is.character(eff.type))}
  # condition: both must be non null or null
  cond <- c(is.null(eff.size), is.null(eff.type))
  if(sum(cond) == 1) {stop ("If you indicate one of 'eff.type' or 'eff.size', you must specifiy the other")}

  tv <- round(t.fit$statistic, round)
  dfv <- round(t.fit$parameter, round)
  pv <- round(t.fit$p.value, round)

  v <- c(0, 0.001, 0.01, 0.05)
  int <- findInterval(pv, v, rightmost.closed = TRUE, left.open=TRUE)
  if (int == 1) {p.report <-  "< 0.001"} else {
    if (int == 2) {p.report <-  "< 0.01"} else {
      if (int == 3) {p.report <-  "< 0.05"} else {
        if (int == 4) {p.report <-  paste("=", pv)} else {
        }
      }
    }
  }

  if (out.text == "plot") {
    report.text <- bquote(paste(italic("t"), " = ", .(tv),
                                "(", .(dfv), "), ",
                                italic("p"), " = ", .(p.report)))
    if(!is.null(eff.type)) {
      report.text <- bquote(paste(italic("t"), " = ", .(tv),
                                  "(", .(dfv), "), ",
                                  italic("p"), " = ", .(p.report),", ",
                                  italic(.(eff.type)), " = ", .(eff.size)))
    }

  } else if (out.text == "plain")
  {
    report.text <- paste0("t = ", tv,
                          "(", dfv, "), ",
                          "p ", p.report)
    if(!is.null(eff.type)) {
      report.text <- paste0(report.text, " ",eff.type, " = ", eff.size)
    }

  } else if (out.text == "latex")
  {
    report.text <-   message("not defined yet")
  }
  report.text
}
