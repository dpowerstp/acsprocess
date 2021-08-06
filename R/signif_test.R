#' Significance test for ACS estimates
#'
#' @param est1 First estimate to compare
#' @param est2 Second estimate to compare
#' @param moe1 MOE of first estimate
#' @param moe2 MOE of second estimate
#' @param just_val Whether to return just the level of confidence or a list with the critical value and level of confidence
#'
#' @return A single value of whether/at what level of confidence the estimates differ statistically, or a list with the critical value and first value
#' @export
#'
#' @examples
signif_test <- function(est1, est2, moe1, moe2, just_val = F){

  se1 <- moe1 / 1.645
  se2 <- moe2 / 1.645

  se1_sqr <- se1 ^ 2
  se2_sqr <- se2 ^ 2

  z_score <- abs((est1 - est2) / sqrt(se1_sqr + se2_sqr))

  crit_val <- dplyr::case_when(z_score > 2.576 ~ 0.99,
                        z_score > 1.960 ~ 0.95,
                        z_score > 1.645 ~ 0.9)

  if (just_val){
    return(crit_val)
  }

  vals <- list("z_score" = z_score,
               "crit_val" = crit_val)

  return(vals)

}
