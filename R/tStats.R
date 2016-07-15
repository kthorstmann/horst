


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
#'
#' @return Returns a character string with the \emph{t}-statistics of a t-test.
#' @export
#' @author Kai T. Horstmann
#'
#'
#' @examples
#' fit <- t.test(1:10, y = c(7:20))
#' boxplot(1:10, c(7:20))
#' mtext(side = 1, line = 2, tStats(fit, out = "plain"))
#'
#'
#'
tStats <- function(t.fit, round = 2, out.text = "plain"){
  if (!is.numeric(round)) {stop("'round' must be numeric")}
  if(!is(t.fit) == "htest") {
    stop ("Input must be fitted t-test from function 't.test'")
  }
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
  } else if (out.text == "plain")
    {
    report.text <- paste0("t = ", tv,
                          "(", dfv, "), ",
                          "p ", p.report)
  } else if (out.text == "latex")
    {
    report.text <-   message("not defined yet")
    }
  ## add possibility to add effect size and name of effect size maybe
  report.text
}
