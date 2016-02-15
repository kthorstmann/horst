#' Moderation Plot plots theree variables
#'
#' This function plots three variables, two on each axis, one in the color dimension.
#'
#' The variable \code{mod} is currently plotted in 5-percent quantiles for each color.
#' @param data A dataset containint at least three variables to plot.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param mod The moderator variable to be plotted in a rainbow-color.
#' @param ... Further parameters passed on to \code{\link{plot}}.
#'
#'@return A graphical representation of a moderation, displaying each point in three variables.
#'
#' @export
#'

plotMod <- function(data, x, y, mod, ...){
  # quantiles
  x <- var1
  y <- var2
  perc <- seq(.0, 1, by = 0.05)
  quants <- quantile(data[,mod], perc, na.rm=T)
  u.quants <- unique(quants)
  n <- length(u.quants)
  # make colors:
  colfunc <- colorRampPalette(c("royalblue", "springgreen","yellow", "red"))

  # add colors to data frame
  color.quants <- cut(data[,mod], breaks=c(-Inf, u.quants), labels=colfunc(n))
  color.quants <- as.character(color.quants)

  # ylim and xlim
  xlim = c(min(data[var1], na.rm=T), max(data[var1], na.rm=T))
  ylim = c(min(data[var2], na.rm=T), max(data[var2], na.rm=T))
  # plot the values:
  plot.new()
  par(fig=c(0,0.8,0,1), new=TRUE)
  plot(jitter(data[,var1]), jitter(data[,var2]), col=color.quants,
       xlim = xlim, ylim = ylim , ... )
  # u.cols <- unique(color.quants)
  par(fig=c(0.65,1,0,1),new=TRUE)
  plot(x = rep(1, n), y = 1:n, col=colfunc(n), pch=19,
       ylab="", xlab="", bty="n", axes=FALSE)
  text(x = rep(1.2, 2), y = c(1, n), labels= c("Low", "High"), cex= 0.7, pos=NULL, srt="0")
  # text(x = rep(1.1, length(u.cols)), y = 1:length(u.cols), labels=quants, cex= 0.7, pos=NULL, srt="0")
  mtext("Value on Moderator", cex=0.8)
}

