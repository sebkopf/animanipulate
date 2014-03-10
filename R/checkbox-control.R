##' @include controls.R
##' @author John Verzani (gWidgetsManipulate package)
NULL

## Checkbox class
Checkbox <- setRefClass("Checkbox", contains="ManipulateControls")
Checkbox$methods(
                 validate_inputs=function(initial, label) {
                   if ( !is.logical(initial) )
                     stop("initial must be a logical")
                 },
                 initialize=function(initial=FALSE, label="") {
                   validate_inputs(initial, label)
                   checkbox <- list(type = 2)
                   callSuper(l=checkbox,  label=label, initial=initial)
                 },
                 make_gui=function(cont, handler, ...) {
                   n <- dim(cont)[1]
                   cont[n+1, 1] <- label
                   cont[n+1, 2] <- (widget <<- gcheckbox(cont=cont))
                   callSuper(cont, handler)
                 }
                 )

##' Checkbox control constructor
##'
##' @param initial logical checked or unchecked
##' @param label character If non-"", labels checkbox
##' @export
##' @return Checkbox instance
checkbox <- function(initial = FALSE, label = "") Checkbox$new(initial, label)                            

