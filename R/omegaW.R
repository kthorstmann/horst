
#' OmegaW for fitted lavaan objects
#'
#' Compute factor reliability Omega_W for latent factors
#'
#' The function computes omega_w, based on standardized factor loadings.
#'
#' @param fit A fitted lavaan object.
#'
#'
#' @export
#'
#' @seealso \code{\link[lavaan]{cfa}}

omegaW <- function(fit){
  std_fit <-  standardizedSolution(fit, type="std.all")
  latent_vars <-  subset(std_fit, op == "=~")
  factor_names <- unique(latent_vars$lhs)
  num.factors <-  length(factor_names)
  omega_list <- vector("list", 0)
  for(i in 1:num.factors){
    sub_lhs <-  subset(latent_vars, lhs == factor_names[i])
    para_i  <-  sub_lhs$est.std
    parameters_sq <-  para_i^2
    parameters_sq_diff_to_one 	<-  1-parameters_sq
    ratio <- parameters_sq/parameters_sq_diff_to_one
    cum_ratio <- sum(ratio)
    omega_w_i <- round(cum_ratio/(cum_ratio+1), 2)
    omega_list[factor_names[i]] = omega_w_i
  }
  omega_w <- sapply(omega_list, "[", 1)
  as.data.frame(omega_w)
}
