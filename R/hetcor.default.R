# last modified 5 Dec 04 by J. Fox

"hetcor.default" <-
function(data, ..., ML=FALSE, std.err=TRUE, bins=4){
  dframe <- data.frame(data, ...)
  names(dframe)[1] <- deparse(substitute(data))
  hetcor(dframe, ML=ML, std.err=std.err, bins=bins)
  }
