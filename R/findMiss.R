
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


# add dependency library(purrr)

# but function should give the position of the value and the actual value

# also add option set.NA, which sets all invalid values to NA, maybe another function?

findMissing <- function(data, values = c(NA), invalids=TRUE){

  # check the type of data
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is.vector(values))
  stopifnot(is.logical(invalids))

  values <- values[!is.na(values)]

  test.values <- function(x) {purrr::map(data, ~ is.na(.) | . == x )}
  positions <- purrr::map(values, test.values)

  pos_df <- map(positions, as.data.frame)
  pos_num <- map(pos_df, function(x) {map(x, as.numeric)})
  pos_df_num <- map(pos_num, as.data.frame)

  for(i in seq_along(pos_df_num)-1) {
    pos_df_num[[1]] <- pos_df_num[[1]] + pos_df_num[[i+1]]
  }

  result <- as.data.frame(pos_df_num[[1]])

  result[result!=0] <- 1
  result

}


# findMiss <- function(data, values = NA, invalids=TRUE){
#   dat <- data
#   if (invalids){
#     missingPosition = sapply(dat, function(x) {which(is.na(x) | x %in% values, arr.ind=F)})
#   }
#   if (!invalids){
#     missingPosition = sapply(dat, function(x) {which(is.na(x) | !x %in% values, arr.ind=F)})
#
#   resultL <- list()
#   for (i in 1:length(missingPosition)) {
#     resultL[[i]] <- dat[missingPosition[[i]],i]
#   }
#   l1 <- unname(missingPosition)
#   l2 <- unname(resultL)
#
#   for(i in 1:length(l1)){
#     l1[[i]] <- data.frame(list(l1[[i]]), list(l2[[i]]))
#   }
#   names(l1) <- colnames(dat)
#   cols <- c("missingRow", "invalidVal")
#   l1 <- lapply(l1, setNames, cols)
#   l3 <- l1[lapply(l1, nrow) > 0] #take only the elements with nrow > 0.
#
#   return(l3)
#   }
#   }
