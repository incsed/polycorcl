"hetcor.default" <-
function(data, ..., ML=FALSE){
  dframe <- data.frame(data, ...)
  names(dframe)[1] <- deparse(substitute(data))
  hetcor(dframe, ML=ML)
  }

