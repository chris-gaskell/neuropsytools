% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deficit.R
\name{deficit}
\alias{deficit}
\title{Assessing For a Deficit in Test Score When Compared to a Control Sample.}
\usage{
deficit(
  score,
  ctrl.mean,
  ctrl.sd,
  ctrl.n,
  direction = "lower",
  tail = "one.tailed",
  conf.level = 0.95,
  dp = 2
)
}
\arguments{
\item{score}{Numeric value representing the score of the single case.}

\item{ctrl.mean}{Numeric value representing the mean of the control group.}

\item{ctrl.sd}{Numeric value representing the standard deviation of the
control group.}

\item{ctrl.n}{Integer value representing the sample size of the control
group.}

\item{direction}{Character. Specifies the direction of the hypothesis.
Options are "lower" (one-tailed), "higher" (one-tailed), or "two.tailed"
(default, two-tailed).}

\item{tail}{Character. Specifies whether the test is one-tailed or
two-tailed. Options are "one.tailed" and "two.tailed" (default)}

\item{conf.level}{Confidence level (default is 0.95 for 95\%).}

\item{dp}{Number of decimal places for rounding the results (default is 2).}
}
\value{
A list of statistical input, parameters, and results. Key outputs
include:
\itemize{
\item t-value: The t-value calculated for the test.
\item p-value: The p-value for the test, indicating statistical significance.
\item effect-size (z-cc): The z-score (effect-size) corrected for the control group.
\item abnormality: The percentage of the population expected to score a more extreme score.
}
}
\description{
Utilises classical (frequentist) statistical methods to compare a single
case’s score with scores from a control sample. It also provides an interval
estimate of the effect size for the difference between the case and the
control group.
}
\details{
Assess for a dissociation between a single test score and a control
sample using the modified paired samples t-test approach of Crawford et al.
(1998). Unlike earlier methods (e.g. Payne & Jones) this method treats data
from the normative same as sample statistics and not population parameters.
The result provided is a t score and associated p-value. This approach
helps to reconcile the problem associated with small normative samples.

In addition to determining whether a difference exists it is also important
to understand the magnitude of that difference. Therefore, it is often
recommended that effect sizes are provided alongside p-values to estimate
the size of the observed effect. To this effect, Crawford et al. (1998) has
provided a method for deriving an effect-size in single-case studies using
the case-controls design (z-cc), where a single patient's cognitive
performance is compared to a matched control group. The modified z-score
(z-cc) is provided as both point and interval estimates.

Finally, neuropsychologists often need to determine how abnormal a
patient's test score is. In the case of the modified t-test, the
abnormality can be easily estimated by multiplying the t-value by 100
(Crawford & Howell, 1998). This estimate quantifies the percentage of the
population expected to exhibit a more extreme score. Confidence limits on
the estimate of abnormality are also provided (Crawford & Garthwaite,
2002).
}
\examples{
# Two-tailed test example:
res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
          ctrl.n = 30, conf.level = 0.95, direction = "lower",
          tail = "two.tailed", dp = 2)
print(res)

# One-tailed test example (higher):
res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
          ctrl.n = 30, conf.level = 0.95, direction = "higher",
          tail = "one.tailed", dp = 2)
print(res)

# One-tailed test example (lower):
res <- deficit(score = 130, ctrl.mean = 100, ctrl.sd = 15,
          ctrl.n = 30, conf.level = 0.95, direction = "lower",
          tail = "one.tailed", dp = 2)
print(res)
}
\references{
\itemize{
\item Crawford, J.R., & Garthwaite, P.H. (2002). Investigation of the single case in neuropsychology: confidence limits on the abnormality of test scores and test score differences. \emph{Neuropsychologia, 40}(2002), 1196–1208.
\item Crawford, J.R., Howell, D.C., & Garthwaite, P.H. (1998). Payne and Jones Revisited: Estimating the Abnormality of Test Score Differences Using a Modified Paired Samples t Test. \emph{Journal of Clinical and Experimental Neuropsychology, 20}(6), 898-905.
\item Crawford, J.R., & Howell, D.C. (1998). Comparing an individual’s test score against norms derived from small samples. \emph{The Clinical Neuropsychologist, 12}(4), 482-486.
\item Crawford, J.R., Garthwaite, P.H., & Porter, S. (2010). Point and interval estimates of effect sizes for the case-controls design in neuropsychology: Rationale, methods, implementations, and proposed reporting standards. \emph{Cognitive Neuropsychology, 27}(3), 245-260.
\item Payne, R. W., & Jones, G. (1957). Statistics for the investigation of individual cases. Journal of Clinical Psychology, 13, 115-121.
}
}
\seealso{
\itemize{
\item \code{\link[=deficit_bayes]{deficit_bayes()}}: For a Bayesian approach to assessing for a dissociation between a single test score and a control sample for a single case.
\item \code{\link[=discrep]{discrep()}}: For assessing a dissociation between two test scores for a single case.
\item \code{\link[=abnorm_ci_t]{abnorm_ci_t()}}: For generating interval estimates for abnormality using the modified t test.
}
}
