\name{hetcor}
\alias{hetcor}
\alias{hetcor.default}
\alias{hetcor.data.frame}
\alias{print.hetcor}

\title{Heterogeneous Correlation Matrix}
\description{
  Computes a heterogenous correlation matrix, consisting of Pearson product-moment
  correlations between numeric variables, polyserial correlations between numeric 
  and ordinal variables, and polychoric correlations between ordinal variables.
}

\usage{
hetcor(data, ..., ML = FALSE, std.err = TRUE)
\method{hetcor}{data.frame}(data, ML = FALSE, std.err = TRUE, 
  use = c("complete.obs", "pairwise.complete.obs"), ...)
\method{hetcor}{default}(data, ..., ML = FALSE, std.err = TRUE)
\method{print}{hetcor}(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{data}{a data frame consisting of factors and/or numeric variables, or the first of several variables.}
  \item{\dots}{variables and/or arguments to be passed down.}
  \item{ML}{if \code{TRUE}, compute maximum-likelihood estimates;
    if \code{FALSE}, compute quick two-step estimates.}
  \item{std.err}{if \code{TRUE}, compute standard errors.}
  \item{use}{if \code{"complete.obs"}, remove observations with any missing data; 
    if \code{"pairwise.complete.obs"}, compute each correlation using all observations with
    valid data for that pair of variables.}
  \item{x}{an object of class \code{"hetcor"} to be printed.}
  \item{digits}{number of significant digits.}
}

\value{
  Returns an object of class \code{"hetcor"} with the following components:
  \item{correlations}{the correlation matrix.}
  \item{type}{the type of each correlation: \code{"Pearson"}, \code{"Polychoric"}, or \code{"Polyserial"}.}
  \item{std.errors}{the standard errors of the correlations, if requested.}
  \item{n}{the number (or numbers) of observations on which the correlations are based.}
  \item{NA.method}{the method by which any missing data were handled: \code{"complete.obs"} 
    or \code{"pairwise.complete.obs"}.}
  \item{ML}{\code{TRUE} for ML estimates, \code{FALSE} for two-step estimates.}
}

\author{John Fox \email{jfox@mcmaster.ca}}

\note{Although the function reports standard errors for product-moment correlations, transformations (the most well known
  is Fisher's \emph{z}-transformation) are available that make the approach to asymptotic normality much more rapid.}

\references{
    Drasgow, F. (1986) 
    Polychoric and polyserial correlations. 
    Pp. 68-74 in S. Kotz and N. Johnson, eds.,
    \emph{The Encyclopedia of Statistics, Volume 7.} Wiley.
    
    Olsson, U. (1979)
    Maximum likelihood estimation of the polychoric correlation coefficient.
    \emph{Psychometrika} \bold{44}, 443-460.
    
    Rodriguez, R.N. (1982)
    Correlation.
    Pp. 193-204 in S. Kotz and N. Johnson, eds.,
    \emph{The Encyclopedia of Statistics, Volume 2.} Wiley.
    
    Ghosh, B.K. (1966)
    Asymptotic expansion for the moments of the distribution of correlation coefficient.
    \emph{Biometrika} \bold{53}, 258-262.
    
    Olkin, I., and Pratt, J.W. (1958)
    Unbiased estimation of certain correlation coefficients.
    \emph{Annals of Mathematical Statistics} \bold{29}, 201-211.
}

\seealso{\code{\link{polychor}}, \code{\link{polyserial}}}

\examples{
R <- matrix(0, 4, 4)
R[upper.tri(R)] <- runif(6)
diag(R) <- 1
R <- cov2cor(t(R) \%*\% R)
round(R, 4)  # population correlations
data <- rmvnorm(1000, rep(0, 4), R)
round(cor(data), 4)   # sample correlations
x1 <- data[,1]
x2 <- data[,2]
y1 <- cut(data[,3], c(-Inf, .75, Inf))
y2 <- cut(data[,4], c(-Inf, -1, .5, 1.5, Inf))
data <- data.frame(x1, x2, y1, y2)
hetcor(data)  # Pearson, polychoric, and polyserial correlations, 2-step est.
hetcor(x1, x2, y1, y2, ML=TRUE) # Pearson, polychoric, polyserial correlations, ML est.
}

\keyword{models}

