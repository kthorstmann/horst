

#'  A command to match variable names in a given data set with variable names from a codebook.
#'
#'  A data set contains variable names, which are usually short. The more meaningful variable names are stored in a codebook, e. g. in Excel. This function allows matching and replacing/renaming the variable names in the data set with the variable names from the codebook.
#'
#' Details to be shown
#' @param data The data frame.
#' @param codebook The codebook which must be in the current working directory as a data frame
#' @param code.old.vars The column in the codebook, in which the original (short) variable names are stored (default is to \code{varname}, as in Unipark)
#' @param code.new.vars The column in the codebook, in which the new (usually longer, meaningful) variable names are stored (default is to \code{new.varname}, as in Unipark)
#'
#'@return Returns the original data set with names replaced from the Codebook. All names in the data set that do not appear in the Codebook remain unchanged.
#'
#' @export
#'
#' @seealso Not much



# function -----------------------------------------------------------


renameCodebook <- function(data, codebook,
                           code.old.vars = "varname",
                           code.new.vars = "new.varname"){
  codeb <- codebook
  new.long.names <- codeb[,code.new.vars] # the new names which replace
  old.names.in.data <- names(data) # the old names to be replaced
  old.names.in.codebook <- codeb[,code.old.vars] # the old names to match against

  # check if all variables from the codebook are also in the
  if(!any(old.names.in.codebook %in% old.names.in.data)) {warning ("Some original varnames in the Codebook are not in the data set")}

  # all variables that have matches in the codebook:
  matches  <-  match(x = old.names.in.data, table = old.names.in.codebook)
  to.replace <- which(!is.na(matches))

  # get the new item names:
  new.item.names <- codeb[matches,code.new.vars]
  replace.with <- new.item.names[!is.na(new.item.names)]
  colnames(data)[to.replace] <- replace.with
  data
}
