##' Resolve variable arguments
##'
##' @param args args passed in
##' @return variables
##' @export
##' @author from RStudio manipulate package
resolveVariableArguments <- function(args) {
  # if the first argument is an unnamed list then just use this list
  if ( (length(args) == 1L) &&
         is.list(args[[1L]])  &&
         (is.null(names(args)) || (names(args)[[1L]] == "")) )  {
    return (args[[1L]])
  } else {
    return (args)
  }
}

##' Map a vector to a control
##'
##' @param x vector defining control
##' @return an instance of ManipulateControls or an error
##' @author John Verzani (gWidgetsManipulate package)
mapVectorToControl <- function(x) UseMethod("mapVectorToControl")

##' Default control is to stop
##' 
##' @method mapVectorToControl default
##' @S3method mapVectorToControl default
##' @author John Verzani (gWidgetsManipulate package)
mapVectorToControl.default <- function(x) stop(sprintf("No control defined for object of class %s", class(x)[1]))

##' numeric to  slider
##' @method mapVectorToControl numeric
##' @S3method mapVectorToControl numeric
##' @author John Verzani (gWidgetsManipulate package)
##' @note could expand by mapping single numeric values to text input with coerce=as.numeric
mapVectorToControl.numeric <- function(x) {
  ## check if a sequence
  if(length(x) > 4) {
    y <- diff(diff(x))
    if(!all.equal(y, rep.int(0, length(y))))
      message("Expecting an arithmatic sequence, forcing it")
    n <- length(x); x <- sort(x)
    return(slider(x[1], x[n], x[1], diff(x)[1]))
  } else {
    ## min, max, step=1, inital=min
    if(length(x) == 2) {
      x[3] <- 1; x[4] <- x[1]
    } else if(length(x) == 3) {
      x[4] <- x[1]
    }
    return(slider(x[1], x[2], x[4], x[3]))
  }
}

##' logical maps to checkbox
##'
##' @method mapVectorToControl logical
##' @S3method mapVectorToControl logical
##' @author John Verzani (gWidgetsManipulate package)
mapVectorToControl.logical <- function(x) {
  checkbox(initial=x[1], label="")
}

##' character to picker
##'
##' @method mapVectorToControl character
##' @S3method mapVectorToControl character
##' @author John Verzani (gWidgetsManipulate package)
mapVectorToControl.character <- function(x) {
  do.call(picker, lapply(x, identity))
}
