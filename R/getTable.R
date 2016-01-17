



# function_getTable --------------------------------------------------

#' Get correlation tables for data frames.
#'
#' A function to generate correlation tables that can be easily transformed
#' to APA tables.
#'
#' This is a function that returns a table of correlations and, depending
#' on \code{apa}, a table that is losely formatted as an apa table. Pearson
#' correlations (using \code{cor} of all variables are computed. Their
#' signifcance levels are also reported, based on the function
#' \code{cor.test} from the  \code{Hmisc}-package.
#'
#' @param data The data frame for which the correlations should be
#'   computed. Non-numerics are excluded from further processing
#' @param apa logical, apa Should APA style be approximated, leading to a
#'   table with an empty upper half. Default is to \code{FALSE}.
#' @param xlsx logical, shoudl an xlsx-file be produced? Default is to
#'   \code{FALSE}.
#' @param xlsxName The name of the resulting xlsx-file, if \code{xlsx =
#'   TRUE}. Default is to \code{Result getTable}.
#' @param diag The value that should be displayed in the first diagonal of
#'   the resulting table, e. g. \code{mean}. Default is empty.
#' @param diag2 The value that should be displayed in the second diagonal
#'   of the resulting table, e. g. \code{var}. Default is empty.
#' @param round The number of digits that should be displayed. Is passed on
#'   to \code{round}. Default is \code{3}.
#' @param ... Other parameters passed on to \code{pearson.cor}.
#' @return Returns a data frame with correlations. See Details.
#' @export
#' @seealso \code{\link{cor}} for the functon that computes the
#'   correlation, \code{\link[Hmisc]{cor.test}} for the function that
#'   computes the \emph{p}-values, and \code{\link[xlsx]{write.xlsx}} for
#'   the function that writes the Exceltable.


# function -----------------------------------------------------------

getTable <- function(data, apa = F, xlsx = F, xlsxName = "Result_getTable",
                     diag = "", diag2 = "", round = 3,
                     detachPackages = FALSE, ...) {

  is.num <- sapply(data, is.numeric)
  nonNumeric <- names(data)[!is.num]
  data <- data[is.num]

  if (any(!is.num)) {
    warning("The following item/columns are non-numeric and were thus ignored:",
            call. = FALSE)
    warning(paste(nonNumeric, collapse = " "), call. = FALSE)
  }

  # define pearson cor function, heart of function
  pearson.cor <- function(data, type = "pearson", round = 3) {
    compl.cor <- rcorr(as.matrix(data), type = type)
    rmat <- compl.cor$r
    pmat <- compl.cor$P
    # select lower from r
    low.rmat <- lower.tri(rmat)
    rmat.low <- rmat[low.rmat]
    # replace lower from p with lower from r
    pmat[lower.tri(pmat)] <- rmat.low
    # prepare results
    rp.mat <- round(pmat, round)
    # print(explan)
    rp.mat

  }

  A = pearson.cor(data, round = round, ...)

  if (diag != "") {
    valuesDiag <- eval(parse(text = paste0("sapply(data,", diag, ", na.rm=T)")))
    diag(A) <- round(valuesDiag, round)
  }

  namesOfVariables <- colnames(A)

  # select all values above the diagonal, i. e. all pvalues
  selectLog <- row(A) < col(A)
  Ap = Ar = A
  Ap[!selectLog] <- NA
  Ar[selectLog] <- NA

  # make 0.000 to 0.0001 and accordingly
  Ap000 <- Ap < 1/(10^round)
  Ap[Ap000] <- 1/(10^round)

  if (diag == "mean" && apa == T && diag2 == "") {
    diag2 = "sd"
  }

  if (diag2 != "") {
    valuesDiag2 = eval(parse(text = paste0("sapply(data,", diag2,
                                           ", na.rm=T)")))
    diag(Ap) = round(valuesDiag2, round)
  }

  As <- matrix(NA, nrow = nrow(A), ncol = nrow(A))
  st.05 <- t(Ap) <= 0.05
  As[st.05] <- "*"
  st.01 <- t(Ap) <= 0.01
  As[st.01] <- "**"
  st.001 <- t(Ap) <= 0.001
  As[st.001] <- "***"

  # combine matrices
  rows.combined <- nrow(Ap)
  cols.combined <- ncol(Ap) + ncol(Ar) + ncol(As)
  matrix.combined <- matrix(NA, nrow = rows.combined, ncol = cols.combined)

  seqAr = seq(1, cols.combined, 3)
  seqAp = seq(2, cols.combined, 3)
  seqAs = seq(3, cols.combined, 3)

  matrix.combined[, seqAr] <- Ar
  matrix.combined[, seqAp] <- t(Ap)
  matrix.combined[, seqAs] <- As

  B = as.data.frame(matrix.combined)

  seqNum <- c(seqAr, seqAp)
  B[, seqNum] <- apply(B[, seqNum], 2, as.numeric)

  names(B)[seqAr] <- namesOfVariables
  names(B)[seqAp] <- paste0(namesOfVariables, ".p")
  names(B)[seqAs] <- paste0(namesOfVariables, ".stars")
  rownames(B) <- namesOfVariables

  if (apa) {
    toReturn <- B
    if (T %in% Ap000) {
      warning(paste0("p-values smaller than ", 1/(10^round), " occured; they were set to p = ",
                     1/(10^round)), call. = F)
    }
  } else {
    toReturn <- A
  }

  if (xlsx) {
    if (!any(c(substr(xlsxName, nchar(xlsxName) - 2, nchar(xlsxName)) ==
               "xls", substr(xlsxName, nchar(xlsxName) - 3, nchar(xlsxName)) ==
               "xlsx"))) {
      xlsxName = paste0(xlsxName, ".xlsx")
      message("The ending .xlsx was added to your excel filename")
    }
    write.xlsx(toReturn, xlsxName, showNA = F)
  } else {
    return(toReturn)
  }
}

