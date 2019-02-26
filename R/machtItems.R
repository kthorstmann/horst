#'  Match short and long item names in EFA
#'
#' Get long items from an item-codebook and the results of an exploratory factor analysis (EFA) combined in one data frame.
#'
#' A common problem is the match of short item names, which are used in a data frame, to their rather long counterparts, i. e. the full item. This function allows matching full items (such as "I like to run an efa") with their short counterpart which was used in the data frame (such as "efa_like"). The matching is based on an item codebook.
#' @param efa.result A fitted EFA from \code{\link[psych]{efa}}.
#' @param codebook The codebook, in which both the \code{shortitem} and \code{longitem} are to be found.
#' @param name The variable in the codebook containing the short names of the item, which were also used in the data frame for the \code{\link[psych]{efa}}.
#' @param label The variable in the codebook containing the full item (which was for example given to the participant in the study).
#' @param digits The number of digits that should be displayed in the resulting data frame.
#' @param cut The smalles loadings that should be displayed. If \code{NULL}, all loadings are returned.
#' @data The original data frame that was used for the EFA. If included, means, percent missing, and frequencies are added.
#'
#'@return A data frame with the \code{shortitem} as rownames, in the first column the long items (from \code{longitem}), and in the next columns the factors/components that were extracted during the EFA, witht heir factor loadings.
#'
#' @export
#'
#' @seealso  \code{\link[psych]{efa}} to run an EFA, \code{\link[openxlsx]{write.xlsx}} to write the resulting data frame into a user friendly .xlsx format.

matchItems <- function(efa.result, codebook, shortitem = "name",
                       longitem = "label", digits=3, cut=NULL, data = NULL){
  codebook <- as.data.frame(codebook)
  fs        <-  psych::fa.sort(efa.result)
  loadings  <-  as.data.frame(fs$loadings[1:nrow(fs$loadings), 1:ncol(fs$loadings)])
  colnames(loadings) <- attr(fs$loadings, which="dimnames")[[2]]
  matches   <-  match(x = rownames(loadings), table = codebook[,shortitem])
  Item      <-  codebook[matches,longitem]
  resultEFA <-  data.frame(Item, loadings)
  is.num    <-  sapply(resultEFA, is.numeric)
  resultEFA[is.num] <-  lapply(resultEFA[is.num], round, digits) # round to digits
  if (!is.null(cut)) {
    resultEFA[is.num][abs(resultEFA[is.num]) <= cut] = NA
  }
  if (is.null(data)) {
    return(resultEFA)
  } else {
    itemnames_short <- rownames(loadings)
    # check if all loading names are in df:
    if(any(!itemnames_short %in% names(df))) {
      stop("The data frame does not contain all variabls from the EFA")
    }
    # extract items and reorder:
    df_efa <- df[itemnames_short]
    means <- round(apply(df_efa, 2, mean, na.rm = TRUE), 2)
    missing_perc <- apply(df_efa, 2, function(x) {round(sum(is.na(x))/length(x), 2)})
    # table distribution
    min <- min(as.matrix(df), na.rm = TRUE)
    max <- max(as.matrix(df), na.rm = TRUE)
    # count how many of min and max the variable has
    x <- df$horse_satis_89
    levels <- min:max
    count_levels <- function(x){
      freq <- sapply(levels, function(y){
        sum(y == x, na.rm = TRUE)
      })
      names(freq) <- levels
      return(freq)
    }
    freq_cols <- as.data.frame(t(apply(df, 2, count_levels)))
    # generate output
    res_EFA <- cbind(resultEFA, means, missing_perc)
    res_EFA$name <- rownames(res_EFA)
    freq_cols$name <- rownames(freq_cols)
    res_EFA_out <- merge(res_EFA, freq_cols, by = "name", sort = FALSE)
    return(res_EFA_out)
  }
}
