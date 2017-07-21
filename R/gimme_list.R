
#' Turns Long Data Into a List of Data Frames
#'
#' The function turns long data into a list of data frames that can be used as input for the command \code{\link[gimme]{gimmeSEM}}.
#'
#' @param data The data in long format
#' @param state_vars The state_variables (level 1) that should be extracted from the data set and kept for the list. Must not contain the ID-variable
#' @param id_var The ID/group variable that identifies participants on level-2
#' @param min_Ti The minimum number of measurement occasions required for each participant Participants with less measurement occasions will be dropped
#'
#' @return Returns a list of data frames as required by \code{\link[gimme]{gimmeSEM}}. Each entry in the list is a data frame for one participant/group.
#' @export
#'
#' @examples
#' ## simulate data
#'data_long <- data.frame(ID = sort(rep(letters[1:10], 3)),
#'                        var1 = sort(rep(1:10, 3)),
#'                        var2 = sort(rep(11:20, 3)))
#'gimme_list(data_long, state_vars = c("var1", "var2"), id_var = "ID", min_Ti = 2)
gimme_list <- function(data, state_vars, id_var, min_Ti){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(state_vars))
  stopifnot(is.character(id_var))
  stopifnot(is.numeric(min_Ti))
  if (is.factor(data[,id_var])) {
    data[,id_var] <- as.character(data[,id_var])
    message("ID/group variable was transformed into factor")
  }
  sub_df <- data[c(id_var, state_vars)]
  counts_per_id <- aggregate(sub_df[, id_var],
                             list(data[,id_var]),
                             function(x) length(x)) ## could be replaced with unique

  valid_ids <- counts_per_id[counts_per_id["x"] >= min_Ti, "Group.1"]
  if (length (valid_ids) == 0) (stop("No id/group had ", min_Ti
                                     ," or more measurement occasions"))
  data_list <- purrr::map(valid_ids, ~ sub_df[sub_df[id_var] == ., state_vars])
  names(data_list) <- valid_ids

  message(length(valid_ids), " ids/groups had ",
          min_Ti, " or more measurement occasions and were selected. A list with ",
          length(valid_ids), " entries was created.")
  data_list
}
