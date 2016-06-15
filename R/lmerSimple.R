

#' lmerSimple
#'
#' A wrapper function for \code{lmer} models. The input are only character vectors. the function can therefore be run over lists of character vectors.
#'
#' The model assumes always random intercepts.
#'
#' @param criterion The value that is predicted.
#' @param fixed The fixed part of the multilevel-model. Can be variables on any level.
#' @param random The random part of the multilevel-model. Can be variables on any level.
#' @param interaction The interaction variables. Please enter as interaction term in \code{lmer} format.
#' @param cluster The cluster variable in the data frame.
#' @param data The data set.
#'
#' @return Returns an \code{lmer} model.
#' @export
#'
#' @examples
#' fit <- lmerSimple("Reaction", "Days", "Days", "", "Subject", sleepstudy)
#' fit
lmerSimple <- function(criterion, fixed, random, interaction, cluster, data){
  criterion.part <- paste(criterion, "~")
  fixed.vars <- paste(fixed, collapse = " + ")
  fixed.part <- paste("1 +", fixed.vars)

  random.vars <- paste(random, collapse = " + ")
  cluster.part <- paste(cluster, collapse = "+")
  random.part <- paste("(1 +", random.vars, "| ", cluster.part, ")")

  ## test if this works with data or pass data to data
  model.code <- paste("lme4::lmer(", criterion.part, fixed.part, "+", random.part,
                      ", data = data)")
  fit <- eval(parse(text = model.code))
  # model.code
  fit
}

#' Get t-values of multilevel-models
#'
#' @param fit A fitted \code{lmer} model.
#'
#' @return Returns a data frame with t-values of the fixed effects.
#' @export
#' @examples
#' fit <- lmerSimple("Reaction", "Days", "Days", "", "Subject", sleepstudy)
#' fit
#' lmertvalue(fit)
lmertvalue <- function(fit){
  fix <- lme4::fixef(fit)/sqrt(diag(vcov(fit)))
  fix.n <- as.data.frame(names(fix))
  fe1 <- cbind(fix.n,fix)
  rownames(fe1)  <- NULL
  fe1
}

#' Make a data frame of t-values.
#'
#' This function takes a list of \code{lmer}-models, extracts the fixed effects and returns their \emph{t}-values as a data frame. The names of the effects are matched by the \code{\link{merge}} function.
#'
#' @param A list of t-values.
#'
#' @return A data frame with t-values.
#' @export
merge.list.t <- function(list){
  all.t.val <- list
  t.value.frame <- all.t.val[[1]]
  if (length(all.t.val) > 1) {
    for (i in 2:length(all.t.val)) {
      t.value.frame <- merge(t.value.frame, all.t.val[[i]], by = "names(fix)", all = TRUE)
    }
  }
  colnames(t.value.frame) <- c("Effects", paste0("model", 1:(ncol(t.value.frame)-1)))
  t.value.frame
}


#' Plot t-values from \code{lmer}-models
#'
#' @param t.value.frame A data frame that contains t-values of the fixed effects. Usually an object returned from \code{merge.list.t}
#' @param axis.labels Should x-axis labels be drawn. Defaul is \code{TRUE}.
#' @param ... Other parameters passed on to \code{plot}.
#'
#' @return Plots t-values and fixed effects.
#' @export
plot.tvalues <- function(t.value.frame, axis.labels = TRUE, ...){
  xlabels <- as.character(t.value.frame[,"Effects"])[-1]
  xrange <- c(1, length(xlabels))
  t.value.frame.red <- t.value.frame[-1,]
  ylims <- c(min(t.value.frame.red[2:ncol(t.value.frame.red)], na.rm=TRUE),
             max(t.value.frame.red[2:ncol(t.value.frame.red)], na.rm=TRUE))

  plot(1, type = "n", bty = "n",
       ylim = ylims,
       xlim = xrange,
       xaxt ="n",
       xlab = "",
       ylab = "",
       ...)
  abline(h = c(-1.96, 0, 1.96))
  if (axis.labels) {
    axis(1, at = xrange[1]:xrange[2], labels = xlabels, las = 2, cex.axis = .8)
  }


  # plot lines
  for (i in 2:ncol(t.value.frame.red)) {
    lines(t.value.frame.red[,i], type = "c", lty = 1+i)
    points(t.value.frame.red[,i], type = "p",
           pch = ifelse(abs(t.value.frame.red[,i]) > 1.96, 20, 1),
           col = ifelse(abs(t.value.frame.red[,i]) > 1.96, "#00BFC4", "#F8766D"))
  }
}

#' lmerPlottt
#'
#' @param list.of.models Numerous multilevel models from \code{lme4}, entered as a list. If only one model is entered, needs also to be entered as a list, e. g. \code{list(fit)}.
#' @param ... Other parameters passed on to \code{plot}.
#'
#' @return Plots all \emph{t}-values of fixed effects of the multilevel models. Significant t-values (5%, two tailed) are plotted as green dots, non-significant ones as red, empty dots. Each model gets an individual line. If models are nested, one can see how the fixed effects change when more model parameters are entered.
#' @export
#'
#' @examples
#' fit <- lmerSimple("Reaction", "Days", "Days", "", "Subject", sleepstudy)
#' fit
#' lmerPlottt(list(fit))
lmerPlottt <- function(list.of.models, ...){
  all.t.val <- purrr::map(list.of.models, lmertvalue)
  t.value.frame <- merge.list.t(all.t.val)
  plot.tvalues(t.value.frame, ...)
}



