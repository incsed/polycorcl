# last modified 5 Dec 04 by J. Fox

"print.polycor" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  if (x$type == "polychoric"){
    se <- sqrt(diag(x$var))
    se.rho <- se[1]
    est <- if (x$ML) "ML est." else "2-step est."
    cat("\nPolychoric Correlation, ", est, " = ", signif(x$rho, digits),
      " (", signif(se.rho, digits), ")\n", sep="")
    r <- length(x$row.cuts)
    c <- length(x$col.cuts)
    if (r == 0) return(invisible(x))
    row.cuts.se <- se[2:(r+1)]
    col.cuts.se <- se[(r+2):(r+c+1)]
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
    est <- if (x$ML) "ML est." else "2-step est."
    cat("\nPolyserial Correlation, ", est, " = ", signif(x$rho, digits),
      " (", signif(se.rho, digits), ")\n\n", sep="")
    if (length(se) == 1) return(invisible(x))
    cuts.se <- se[-1]
    thresh <- signif(rbind(x$cuts, cuts.se), digits)
    colnames(thresh) <- 1:length(x$cuts)
    rownames(thresh) <- c("Threshold", "Std.Err.")
    print(thresh)
    }
  else print(unclass(x))
  invisible(x)
  }
