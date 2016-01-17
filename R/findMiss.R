
# function_findMiss  --------------------------------------------------


#'Find missing or invalid data
#'
#'A function to find missings values or values that should not be defined
#'in a data frame.
#'
#'If \code{invalids} is set to \code{FALSE}, the values entered in
#'\code{values} are the valid ones, all other values are reported. If set
#'to \code{TRUE}, values specified in \code{values} are invalid and
#'reported.
#'
#'@param data A data frame
#'@param values A vector of values to look or not to look for (see
#'  \code{invalids}). Defaults to \code{NA}.
#'@param invalids logical. Are the \code{values} entered valid or invalid
#'  values? See Details.
#'@export
#'@return Returns a list with an entry for each variable that has missing
#'  values (or invalid values), the position of the missing or invalid
#'  values as well as the percentage of missing or invalid values per
#'  variable. Add some values here.


# function -----------------------------------------------------------


findMiss <- function(data, values = NA, invalids=TRUE){
  dat <- data
  if (invalids){
    missingPosition = sapply(dat, function(x) {which(is.na(x) | x %in% values, arr.ind=F)})
  }
  if (!invalids){
    missingPosition = sapply(dat, function(x) {which(is.na(x) | !x %in% values, arr.ind=F)})
  }

  resultL <- list()
  for (i in 1:length(missingPosition)) {
    resultL[[i]] <- dat[missingPosition[[i]],i]
  }
  l1 <- unname(missingPosition)
  l2 <- unname(resultL)

  for(i in 1:length(l1)){
    l1[[i]] <- data.frame(list(l1[[i]]), list(l2[[i]]))
  }
  names(l1) <- colnames(dat)
  cols <- c("missingRow", "invalidVal")
  l1 <- lapply(l1, setNames, cols)
  l3 <- l1[lapply(l1, nrow) > 0] #take only the elements with nrow > 0.

  return(l3)
}
