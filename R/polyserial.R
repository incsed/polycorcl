"polyserial" <-
function(x, y, ML=FALSE, control=list(), std.err=FALSE){
  if (!is.numeric(x)) stop("x must be numeric")
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  z <- scale(x)
  tab <- table(y)
  n <- sum(tab)
  s <- length(tab)
  indices <- 1:n
  cuts <- qnorm(cumsum(tab)/n)[-s]
  y <- as.numeric(as.factor(y))
  rho <- sqrt((n - 1)/n)*sd(y)*cor(x, y)/sum(dnorm(cuts))
  if (!ML) return(rho)
  f <- function(pars){
    rho <- pars[1]
    cuts <- c(-Inf, pars[-1], Inf)
    tau <- (matrix(cuts, n, s+1, byrow=TRUE) - matrix(rho*z, n, s+1))/
             sqrt(1 - rho^2)
    - sum(log(dnorm(z)*(pnorm(tau[cbind(indices, y+1)]) - pnorm(tau[cbind(indices, y)]))))
    }
  result <- optim(c(rho, cuts), f, control=control, hessian=std.err)
  if (std.err){
    result <- list(type="polyserial",
                   rho=result$par[1],
                   cuts=result$par[-1],
                   var=solve(result$hessian),
                   n=n)
    class(result) <- "polycor"
    return(result)
    }
  else return(as.vector(result$par[1]))
  }

