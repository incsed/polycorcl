"print.polycor" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  if (x$type == "polychoric"){
    r <- length(x$row.cuts)
    c <- length(x$col.cuts)
    se <- sqrt(diag(x$var))
    se.rho <- se[1]
    row.cuts.se <- se[2:(r+1)]
    col.cuts.se <- se[(r+2):(r+c+1)]
    cat("\nPolychoric Correlation = ", signif(x$rho, digits),
      " (", signif(se.rho, digits), ")\n", sep="")
    cat("\n  Row Thresholds\n")
    rowThresh <- signif(cbind(x$row.cuts, row.cuts.se), digits)
    colnames(rowThresh) <- c("Threshold", "Std.Err.")
    rownames(rowThresh) <- 1:r
    print(rowThresh)
    cat("\n\n  Column Thresholds\n")
    colThresh <- signif(cbind(x$col.cuts, col.cuts.se), digits)
    colnames(colThresh) <- c("Threshold", "Std.Err.")
    rownames(colThresh) <- 1:c
    print(colThresh)
    }
  else if (x$type == "polyserial"){
    se <- sqrt(diag(x$var))
    se.rho <- se[1]
    cuts.se <- se[-1]
    cat("\nPolyserial Correlation = ", signif(x$rho, digits),
      " (", signif(se.rho, digits), ")\n\n", sep="")
    thresh <- signif(rbind(x$cuts, cuts.se), digits)
    colnames(thresh) <- 1:length(x$cuts)
    rownames(thresh) <- c("Threshold", "Std.Err.")
    print(thresh)
    }
  else print(unclass(x))
  invisible(x)
  }

