
#'  Detect simpsons paradox in multilevel data
#'
#' Allows visual inspection of multilevel data and the relations between a set of variables and one outcome variable, both on level 1 and on level 2.
#'
#' The function computes all correlations between a set of variables and one outcome variable on level-1 (within cluster) and level-2 (between cluster, i. e. between the cluster means). The correlations on different levels are then plotted against each other. The more a point is off the diagonal, the more the correlations differ on level 1 and level 2.
#'
#' @param data The data frame to be used.
#' @param outcome The dependend variable, i. e. a variable to be used as a criterion in multilevel analysis.
#' @param scales The set of variables which are all correlated with \code{outcome} (and not amongst each other).
#' @param cluster The level 2 cluster variable, in which both the \code{var1} and \code{var2} are nested.
#' @param ... Other arguments passed on to \code{\link{cor}}.
#' @return A plot showing level 1 and level 2 correlations plotted against each other.
#' @export
#' @seealso \code{\link{cor}} for the function that computes the correlation.

simpsonCor <-  function(data, outcome, scales, cluster, ...){
  group_means  <-  aggregate(data[,c(outcome, scales)], list(data[,c(cluster)]), mean, na.rm=T)
  meanCors_l2l <-  cor(group_means[scales], group_means[outcome], use = "pairwise.complete.obs")
  cors_l1l     <-  cor(data[scales], data[outcome], ...)
  l1l2Cors     <-  cbind(cors_l1l, meanCors_l2l)
  level2.name         <-  paste("l2l_", outcome, sep="")
  level1.name         <-  paste("l1l_", outcome, sep="")
  colnames(l1l2Cors)  <-  c(level1.name, level2.name)

  title <- paste("Alignment of within-", cluster, " and ", cluster, "-level correlations", sep="")
  ylab.txt <-  bquote(paste("Correlation of Scale and ", bold(.(outcome)), " on Level Two", sep=""))
  xlab.txt <-  bquote(paste("Correlation of Scale and ", bold(.(outcome)), " on Level One", sep=""))

  plot(l1l2Cors,
       ylab = ylab.txt,
       xlab = xlab.txt, cex.lab =0.8,
       xlim = c(-1.2, 1.2), ylim=c(-1.2, 1.2), cex.axis=0.8,
       main = title, cex.main=0.8,
       sub = paste("Scales: ", paste0(scales, sep="", collapse=", "), sep=""), cex.sub=0.6,
       bty="n")
  rect(-1,0,0,1, col = "tomato2", border=NA)
  # bottomright
  rect(0,-1,1,0, col = "tomato2", border=NA)
  #topright
  rect(0,0,1,1, col = "olivedrab3", border=NA)
  # bottomleft
  rect(-1,-1,0,0, col = "olivedrab3", border=NA)
  points(l1l2Cors, pch=16)
  text(l1l2Cors+0.05, labels=rownames(l1l2Cors), cex=0.7, offset=10)
  abline(v=0)
  abline(h=0)
}
