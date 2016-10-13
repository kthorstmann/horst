#' Histogramm of correlations
#'
#' Plots a histogram of correlations to make Simpson's paradox visible.
#'
#' A histogram of correlations, [[1]] group means for the two level-1
#' variables and [[2]] their correlation on level-2. Currently,
#' the pearson correlation is used for computing the correlation.
#'
#'
#' @param data The data set to be used. Should consist of at least two numeric variables and one cluster variable.
#' @param var1 The first varaible to correlate.
#' @param var2 The second variable to correlate.
#' @param cluster The level 2 cluster variable, in which both the \code{var1} and \code{var2} are nested.
#'
#' @export
#'
#' @seealso \code{\link[stats]{cor}}

corHist <- function(data, var1, var2, cluster) {
  requireNamespace("purrr", quietly = TRUE)

  variable.names                        <-  c(var1, var2, cluster)
  names(data)[names(data) == var1]      <- "lvl1.var1"
  names(data)[names(data) == var2]      <- "lvl1.var2"
  names(data)[names(data) == cluster]   <- "cluster"
  head(data)

  # 1. compute means (type of correlation can be changed!) and within correlation
  subject.ids <- unique(data[,"cluster"])
  mean1 <- unlist(purrr::map(subject.ids, ~ mean(data[data["cluster"] == .,"lvl1.var1"], na.rm = TRUE)))
  mean2 <- unlist(purrr::map(subject.ids, ~ mean(data[data["cluster"] == .,"lvl1.var2"], na.rm = TRUE)))

  cor.lvl1 <- purrr::map(subject.ids, ~ cor(data[data["cluster"] == .,"lvl1.var1"],
                                            data[data["cluster"] == .,"lvl1.var2"],
                                            use = "complete.obs"))
if ( round(var(mean1, na.rm =TRUE), 1) == 0 | round(var(mean2, na.rm =TRUE), 1) == 0 ) {
  message("One of the input variables as a variance of nearly zero on the within means. Check if this variable is/was within person centered.")
}

  # 2. level 2 correlation of means (i. e. of variables on level 2)
  cor.lvll2   <- cor.test(mean1, mean2, use = "complete.obs")
  lvl2.cor.r  <- round(cor.lvll2$estimate, 3)
  lvl2.cor.p  <- round(cor.lvll2$p.value, 3)
  lvl2.cor.df <- round(cor.lvll2$parameter, 3)
  # 3. level 1 correlation across clusters
  cor.lvll1   <- cor.test(data[,"lvl1.var1"], data[,"lvl1.var2"], use = "complete.obs")
  lvl1.cor.r  <- round(cor.lvll1$estimate, 3)
  lvl1.cor.p  <- round(cor.lvll1$p.value, 3)
  lvl1.cor.df <- round(cor.lvll1$parameter, 3)
  # 4. plot
  # plot.new()
  hist(unlist(cor.lvl1), breaks = 20, xlim = c(-1, 1),
        main="", xlab="", ylab="")

  head.text <- bquote(paste("Correlation of ", bold(.(var1)), " and ", bold(.(var2)), sep = " "))
  mtext(side = 3, head.text, line = 2, cex = .8)
  abline(v = lvl2.cor.r, col = "red")  # add line lvl2 cor
  abline(v = lvl1.cor.r, col = "green")  # add line lvl1 cor (correlation across clusters)
  meanofcor = mean(unlist(cor.lvl1), na.rm=T)
  abline(v = meanofcor, col = "blue") # add line lvl1 cor mean
  testval.2 = bquote(paste("Level-2 correlartion (means, red): ", italic("r "), "= ", .(lvl2.cor.r),
                           ", ", italic("p"), " = ",  .(lvl2.cor.p), ", with ", italic("df"), " = ",
                           .(lvl2.cor.df),  sep = ""))  # add test stat summary
  mtext(side = 3, testval.2, line = 0, cex = .6, at = 0)
  testval.1 = bquote(paste("Level-1 correlation (no cluster structure, green): ", italic("r "), "= ",
                           .(lvl1.cor.r), ", ", italic("p"), " = ",  .(lvl1.cor.p), ", with ",
                           italic("df"), " = ", .(lvl1.cor.df),  sep = ""))
  mtext(side = 3, testval.1, line = 1, cex = .6, at = 0)

  out.data <- data.frame(subject.ids, mean1, mean2)
  name1    <- paste("mean", var1, sep = "")
  name2    <- paste("mean", var2, sep = "")
  colnames(out.data) <- c(cluster, name1, name2)

  results <- list(level2.data = out.data, level.2.cor = cor.lvll2)
  return(results)
}

