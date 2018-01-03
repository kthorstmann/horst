


#' Make Entries in Data Frames Bold for Markdown Table
#'
#' @param data.frame A data frame. Numeric entries will be made bold.
#' @param cut The cutoff, numeric. (Absolute) values that are larger than cur will be transformed.
#'
#' @return The same data frame, but with entries formatted to be bold
#' @export
#'
#' @examples
#' library(psych)
#' # from psych::fa example:
#' Thurstone.33 <- as.matrix(Thurstone.33)
#' mle3 <- fa(Thurstone.33,3 ,rotate="none",fm="mle")
#' fs <- psych::fa.sort(mle3)
#' loadings <- round(as.data.frame(fs$loadings[1:nrow(fs$loadings), 1:ncol(fs$loadings)]), 2)
#' make_bold(loadings)
make_bold <- function(data.frame, cut = 0.30){
  stopifnot(is.data.frame(data.frame))
  numerics <- sapply(data.frame, is.numeric)
  x <- c(0.3, 0.2)
  change_to_bold <- function(x, cut){
    out <- sapply(x, function(x){ifelse(abs(x) >= cut, paste0("**", x, "**"), x)})
    out
  }
  data.frame[numerics] <- apply(data.frame[numerics], 2, change_to_bold, cut = cut)
  return(data.frame)
}
