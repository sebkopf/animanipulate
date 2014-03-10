##' @include controls.R
##' @author John Verzani (gWidgetsManipulate package)
NULL

## Button class
Button <- setRefClass("Button",
                      contains="ManipulateControls",
                      methods=list(
                        initialize=function(label=NULL) {
                          button <- list(label=label)
                          callSuper(l=button, label=label, initial=label)
                        },
                        make_gui=function(cont, handler, ...) {
                          n <- dim(cont)[1]
                          cont[n+1, 1:2, expand=TRUE] <- (widget <<- gbutton(text="", cont=cont))
                          callSuper(cont, handler)
                        }))

                      
##' Button interface
##' 
##' @param label button label
##' @export
button <- function(label="")Button$new(label=label)

