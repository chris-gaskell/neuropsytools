#' Calculate score deviances and significance
#'
#' This function calculates the mean deviances of test scores from the mean,
#' evaluates their significance, and computes various related statistics.
#'
#' @param x A numeric vector of test scores.
#' @param R A numeric matrix representing the correlation matrix. If not provided, correlation-based calculations are skipped.
#' @param sem A numeric vector of standard error of measurement (SEM) values.
#' @param sd Standard deviation of the test scores. Default is 15.
#' @param dp Number of decimal places for rounding in the results. Default is 2.
#' @param names Optional character vector of names for the test scores. If not provided, default names will be used.
#' @param conf.level Confidence level for the confidence intervals. Default is 0.90.
#' @param threshold Threshold for abnormality detection in Z scores. Default is z = -1.645.
#' @param apply.bonferroni Logical, whether to apply the Bonferroni correction. Default is FALSE.
#'
#' @return A list containing various statistics and results.
#'
#' @importFrom stats pnorm sd qnorm
#' @export
#'
#' @examples
#' sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
#' scores <- c(118, 107, 77, 68)
#' R <- matrix(c(1.00, 0.61, 0.64, 0.45,
#'               0.61, 1.00, 0.62, 0.52,
#'               0.64, 0.62, 1.00, 0.51,
#'               0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
#' names <- c("Verbal Comprehension", "Perceptual Reasoning",
#'            "Working Memory", "Processing Speed")
#' discreps_from_mean(scores, R = R, sem = sem, conf.level = 0.95, names = names)
discreps_from_mean <- function(x, R = NULL, sem, sd = 15, dp = 2, names = NULL, conf.level = 0.90, threshold = -1.645, apply.bonferroni = FALSE) {
  alpha = 1 - conf.level
  k <- length(x)
  if (is.null(names)) {
    names <- as.character(seq(1, k))
  } else {
    names <- stringr::str_to_upper(abbreviate(names, minlength = 2, method = "left.kept"))
  }

  z.sd <- qnorm(1 - ((1 - conf.level) / 2))
  mean.x <- mean(x)

  sem.dev <- numeric(length(sem))
  for (i in 1:length(sem)) {
    sem.dev[i] <- sqrt(((k - 2) / k) * sem[i]^2 + (1 / k^2) * sum(sem^2))
  }

  dev <- x - mean.x
  dev.ci.lb <- dev - sem.dev * z.sd
  dev.ci.ub <- dev + sem.dev * z.sd

  crit.vals <- sem.dev * z.sd
  signif.below <- dev < -crit.vals
  signif.either <- abs(dev) > crit.vals

  if (!is.null(R)) {
    G.bar <- mean(R)  # Mean of all elements in the correlation matrix
    h.a.bar <- rowMeans(R)  # Mean of each row of the matrix
    sd.dev <- sd * sqrt(1 + G.bar - 2 * h.a.bar)
    crit.val.sd <- sd.dev * z.sd
    sig.diffs <- abs(dev) > crit.val.sd

    p.vals.1t <- 1 - pnorm(abs(dev) / sem.dev)
    p.vals.2t <- p.vals.1t * 2

    # Apply sequential Bonferroni correction if requested
    if (apply.bonferroni) {
      bonferroni_results_1t <- sequen_bonf(p.vals.1t, alpha = alpha)
      p.vals.1t <- bonferroni_results_1t$adj.p.vals
      sig.diffs.1t <- bonferroni_results_1t$sig

      bonferroni_results_2t <- sequen_bonf(p.vals.2t, alpha = alpha)
      p.vals.2t <- bonferroni_results_2t$adj.p.vals
      sig.diffs.2t <- bonferroni_results_2t$sig
    }

    abn.1t <- round(100 - pnorm(abs(dev) / sd.dev) * 100, 4)
    abn.2t <- round(100 - pnorm(abs(dev) / sd.dev) * 100, 4) * 2
    abn.k <- sum(abn.2t < 5)

    abn.rates <- neuropsytools::aborm_j_battery(R = R, threshold = threshold)
    abn <- abn.rates$props[abn.k]


# output ------------------------------------------------------------------

    output_df <- data.frame(
      Test = names,
      Deviation = format(round(dev, dp), nsmall = dp),
      CI = paste0(
        format(round(dev.ci.lb, 2), nsmall = dp),
        " to ",
        format(round(dev.ci.ub, 2), nsmall = dp)
      ),
      `p-val 2t` = format.pval(p.vals.2t, digits = dp, eps = .0001),
      `p-val 1t` = format.pval(p.vals.1t, digits = dp, eps = .0001),
      `Abnormal 2d` = format(round(abn.2t, 2), nsmall = dp),
      `Abnormal 1d` = format(round(abn.1t, 2), nsmall = dp),
      stringsAsFactors = FALSE
    )


    result <- list(
      k = k,
      z.sd = z.sd,
      names =  names,
      mean.x = round(mean.x, dp),
      dev =       round(dev, dp),
      dev.ci.lb = round(dev.ci.lb, dp),
      dev.ci.ub = round(dev.ci.ub, dp),
      sem.dev = round(sem.dev, dp),
      crit.vals = round(crit.vals, dp),
      signif.below = signif.below,
      signif.either = signif.either,
      conf.level = conf.level,
      prev.1t = round(abn.1t, dp),
      prev.2t = round(abn.2t, dp),
      abn.k = abn.k,
      abn = round(abn, dp),
      abn.rates = abn.rates,
      sig.diffs = sig.diffs,
      output_df = output_df,
      dp = dp
    )
  }

  class(result) <- 'mean_devs'
  return(result)
}


#' @export
print.mean_devs <- function(x, ...) {

    output_table <- knitr::kable(x$output_df, format = "simple", align = "c",
                                 col.names = c("Test",
                                               "Discrep",
                                               paste(x$conf.level*100, "% CI"),
                                               "p-val 2t", "p-val 1t",
                                               "Abnormal 2d", "Abnormal 1d")
    )


  # Print the header
  header <- paste(
    "ABNORMALITY of Index score deviations from the case's mean Index score:\n",
    "Case's mean Index score: ", format(x$mean.x, nsmall = x$dp), "\n\n", sep = ""
  )
  cat(header)

  output_table

  cat(output_table, sep = "\n")

  if (!is.null(x$abn.rates)) {
    abn_k <- glue::glue("NUMBER of case's deviation scores that meet criterion for abnormality = {x$abn.k}")
    abn <- glue::glue("\n\nPERCENTAGE of normal population expected to exhibit this number or more of abnormally low scores = {x$abn}%")
    cat("\n", abn_k, "\n", abn, "\n", sep = "")
  }
}


# # Example Usage
# sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
# R <- matrix(c(1.00, 0.61, 0.64, 0.45,
#               0.61, 1.00, 0.62, 0.52,
#               0.64, 0.62, 1.00, 0.51,
#               0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
# scores <- c(118, 107, 77, 68)
# #scores <- c(80, 100, 100, 100)
# names <- c("Verbal Comprehension", "Perceptual Reasoning",
#            "Working Memory", "Processing Speed")
#
# # Generate the results
# results <- discreps_from_mean(x = c(60, 100, 100, 100), R = R, sem = sem,
#                                        conf.level = 0.95, names = names,
#                                        apply.bonferroni = F, dp = 4
#                                        )
#
# # Print the results using the custom print method
# print(results)
# print.default(results)
