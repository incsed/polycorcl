"polychor" <-
function (x, y, ML=FALSE, control=list(), std.err=FALSE){
  f <- function(pars) {
    if (length(pars) == 1){
       rho <- pars
       row.cuts <- c(-Inf, rc, Inf)
       col.cuts <- c(-Inf, cc, Inf)
       }
     else {
       rho <- pars[1]
       row.cuts <- c(-Inf, pars[2:r], Inf)
       col.cuts <- c(-Inf, pars[(r+1):(r+c-1)], Inf)
       }
    P <- matrix(0, r, c)
    R <- matrix(c(1, rho, rho, 1), 2, 2)
    for (i in 1:r){
      for (j in 1:c){
        P[i,j] <- pmvnorm(lower=c(row.cuts[i], col.cuts[j]),
                          upper=c(row.cuts[i+1], col.cuts[j+1]),
                          corr=R)
        }
      }
     - sum(tab * log(P))
    }
  tab <- if (missing(y)) x else table(x, y)
  r <- nrow(tab)
  c <- ncol(tab)
  n <- sum(tab)
  rc <- qnorm(cumsum(rowSums(tab))/n)[-r]
  cc <- qnorm(cumsum(colSums(tab))/n)[-c]
  if (ML) {
    result <- optim(c(optimise(f, interval=c(0, 1))$minimum, rc, cc), f,
      control=control, hessian=std.err)
    if (std.err) {
      result <- list(type="polychoric",
                     rho=result$par[1],
                     row.cuts=result$par[2:r],
                     col.cuts=result$par[(r+1):(r+c-1)],
                     var=solve(result$hessian),
                     n=n)
      class(result) <- "polycor"
      return(result)
      }
    else return(as.vector(result$par[1]))
    }
  else optimise(f, interval=c(0, 1))$minimum
  }

