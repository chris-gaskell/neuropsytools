#'#' Generate Descriptive Labels for Standardized Test Scores
#'
#'This function assigns qualitative descriptors to standardized test scores
#'based on predefined systems. Descriptors are derived according to established
#'thresholds in various neuropsychological assessment systems (see details for
#'more information).
#'
#'@details The function uses predefined threshold values and descriptors from
#'  various neuropsychological assessment systems to convert test scores into
#'  qualitative categories. By default, it uses the system from the American
#'  Academy of Clinical Neuropsychology consensus paper (Guilmette et al.,
#'  2020).
#'
#'  Available descriptor systems include Guilmette et al. (2020), Wechsler
#'  WISC-V (2014), Wechsler WAIS-IV (2008), NEPSY II (2007), Q Simple (2017),
#'  Groth-Marnat (2009), and Heaton (1991). The preferred and default system for
#'  this function is the American Academy of Clinical Neuropsychology consensus
#'  (Guilmette et al., 2020). For details on other descriptor systems, see
#'  Schoenberg and Rum (2017).
#'
#'@inheritParams convert_standard
#'@param metric Character string specifying the metric of the input scores.
#'  ("z", "t", "index", "scaled", "sten", "stanine", "sat"). Default is "index".
#'@param system Character string specifying the descriptor system to use.
#'  Options include "aan", "wisc", "wais", "groth.marnat", "nepsy", "q.simple",
#'  and "heaton". Default is "aan".
#'@param keep_na NA values are kept as NA without throwing a warning message.
#'
#'@return A character vector containing the qualitative descriptors (e.g.,
#'  "Low", "Average", "High") corresponding to the input test scores based on
#'  the selected descriptor system.
#'
#' @examples
#' # Convert index scores to descriptors using the AAN descriptor system
#' score_descriptor(score = c(85, 105, 120), metric = "index", system = "aan")
#'
#' # Convert index scores to descriptors using the NEPSY descriptor system
#' score_descriptor(score = c(75, 95, 110), metric = "index", system = "nepsy")
#'
#' @references
#'   - Schoenberg, M.R., & Rum, R.S. (2017). Towards reporting standards for
#'     neuropsychological study results: A proposal to minimize communication
#'     errors with standardized qualitative descriptors for normalized test scores.
#'     *Clinical Neurology and Neurosurgery, 162*, 72-79.
#'   - Guilmette, T.J., Sweet, J.J., Hebben, N., Koltai, D., Mahone, E.M.,
#'     Spiegler, B.J., Stucky, K., & Westerveld, M. (2020).
#'     American Academy of Clinical Neuropsychology consensus conference
#'     statement on uniform labeling of performance test scores.
#'     *The Clinical Neuropsychologist, 34*(3), 437-453.
#'     doi:10.1080/13854046.2020.1722244.
#'
#'@seealso
#'   - [convert_z()]: for converting z scores to other standardised metrics.
#'   - [convert_standard()]: For assessing a dissociation between two test scores for a single case.
#'
#'@export
score_descriptor <- function(score, metric = "index", system = "aan", keep_na = TRUE) {
  thresholds <- list(
    "aan"  =         c(69, 79, 89, 109, 119, 129),
    "wisc" =         c(69, 79, 89, 109, 119, 129),
    "wais" =         c(69, 79, 89, 109, 119, 129),
    "groth.marnat" = c(69, 79, 89, 109, 119, 129),
    "nepsy" =        c(64, 74, 89, 109),
    "q.simple" =     c(76, 84, 89, 109, 119, 129),
    "heaton" =       c(54, 61, 69, 76, 84, 91, 106, 114, 129)
  )

  descriptor_labels <- list(
    "aan" = c("Exceptionally Low", "Below Average", "Low Average", "Average", "High Average", "Above Average", "Exceptionally High"),
    "wisc" = c("Extremely Low", "Very Low", "Low Average", "Average", "High Average", "Very High", "Extremely High"),
    "wais" = c("Extremely Low", "Very Low", "Low Average", "Average", "High Average", "Superior", "Very Superior"),
    "nepsy" = c("Well Below Expected Level", "Below Expected Level", "Borderline", "At Expected Level", "Above Expected Level"),
    "q.simple" = c("Abnormal", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"),
    "groth.marnat" = c("Lower Extreme", "Well Below Average", "Low Average", "Average", "High Average", "Well Above Average", "Upper Extreme"),
    "heaton" = c("Severely Impaired", "Moderately to Severely Impaired", "Moderately Impaired", "Mildly to Moderately Impaired", "Mildly Impaired",
                 "Below Average", "Average", "Above Average", "Superior", "Very Superior")
  )

  if (metric != "index") {
    score <- neuropsytools::convert_standard(score, metric, "index", 0)
  }

  if (!(system %in% names(thresholds))) {
    stop("Invalid descriptor system. Choose from the available systems: ", paste(names(thresholds), collapse = ", "), ".")
  }

  if (keep_na) {
    # If keep_na is TRUE, we won't issue a warning about NA values
    result <- cut(score, breaks = c(-Inf, thresholds[[system]], Inf), labels = descriptor_labels[[system]], include.lowest = TRUE, right = TRUE)
  } else {
    # Otherwise, we keep the warning
    if (any(is.na(score))) {
      warning("NA values detected in score. These will be returned as NA in the output.")
    }
    result <- cut(score, breaks = c(-Inf, thresholds[[system]], Inf), labels = descriptor_labels[[system]], include.lowest = TRUE, right = TRUE)
  }

  # Return the result
  return(as.character(result))
}
