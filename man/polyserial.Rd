\name{polyserial}
\alias{polyserial}

\title{Polyserial Correlation}

\description{
  Computes the polyserial correlation between a quantitative variable and an ordinal variables, based on the assumption
  that the joint distribution of the quantitative variable and a latent continuous variable underlying the ordinal 
  variable is bivariate normal. Either
  the maximum-likelihood estimator or a quicker ``two-step'' approximation are available. For the ML
  estimator the estimates of the thresholds and the covariance matrix of the estimates are also available.
}
\usage{
polyserial(x, y, ML = FALSE, control = list(), std.err = FALSE)
}

\arguments{
  \item{x}{a numerical variable.}
  \item{y}{an ordered categorical variable; can be numeric, a factor,
    or an ordered factor, but if a factor, its levels should be in proper order.}
  \item{ML}{if \code{TRUE}, compute the maximum-likelihood estimate; if \code{FALSE}, the default, compute a quicker
    ``two-step'' approximation.}
  \item{control}{optional arguments to be passed to the \code{optim} function, which computes the ML estimate;
    for \code{ML=TRUE} only.}
  \item{std.err}{if \code{TRUE}, return the estimated covariance matrix of the correlation and
    thresholds; used only if \code{ML=TRUE}; the default is \code{FALSE}.}
}

}
\value{
  If \code{ML} and \code{std.err} are both \code{TRUE}, 
  returns an object of class \code{"polycor"} with the following components:
  \item{type}{set to \code{"polyserial"}.}
  \item{rho}{the polyserial correlation.}
  \item{cuts}{estimated thresholds for the ordinal variable (\code{y}).}
  \item{var}{the estimated covariance matrix of the correlation and thresholds.}
  \item{n}{the number of observations on which the correlation is based.}
  Othewise, returns the polyserial correlation.
}

\references{
    Drasgow, F. (1986) 
    Polychoric and polyserial correlations. 
    Pp. 68--74 in S. Kotz and N. Johnson, eds.,
    \emph{The Encyclopedia of Statistics, Volume 7.} Wiley.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\code{\link{hetcor}}, \code{\link{polychor}}, \code{\link{print.polycor}},
  \code{\link[stats]{optim}}}

\examples{
data <- rmvnorm(1000, c(0, 0), matrix(c(1, .5, .5, 1), 2, 2))
x <- data[,1]
y <- data[,2]
cor(x, y)  # sample correlation
y <- cut(y, c(-Inf, -1, .5, 1.5, Inf))
polyserial(x, y)  # 2-step estimate
polyserial(x, y, ML=TRUE, std.err=TRUE) # ML estimate
}

\keyword{models}

