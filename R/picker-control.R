##' @include controls.R
##' @author John Verzani (gWidgetsManipulate package)
##' @author Sebastian Kopf
NULL

## Picker is a combobox
Picker <- setRefClass(
  "Picker",
  contains="ManipulateControls",
  methods=list(
    #' @param radio_cutoff - how many items are still rendered as a radio group
    initialize=function(..., initial=NULL, label="", radio_cutoff = 3) {
      
      ## get values
      values <- animanipulate:::resolveVariableArguments(list(...))
      
      ## get value names
      valueNames <- names(values)
      if (is.null(valueNames))
        valueNames <- character(length(values))
      
      ## default missing names to choice values
      missingNames <- valueNames == ""
      valueNames[missingNames] <- paste(values)[missingNames]
      names(values) <- valueNames
      validate_inputs(values, valueNames, initial,label)
      
      
      if(is.null(initial)) {
        initial <<- valueNames[1]
      } else {
        initial <<- initial
      }
      
      ## create picker
      picker <- list(choices = valueNames,
                     values = values,
                     radio = length(values) <= radio_cutoff
      )
      callSuper(l=picker, label=label, initial=.self$initial)
    },
    make_gui=function(cont, handler, ...) {
      if(is(widget, "uninitializedField")) {
        n <- dim(cont)[1]
        cont[n+1, 1] <- label
        if (l$radio)
          cont[n+1, 2] <- (widget <<- gradio(l$choices, cont=cont))
        else
          cont[n+1, 2] <- (widget <<- gcombobox(l$choices, cont=cont))
      }
      callSuper(cont, handler, ...)
    },
    validate_inputs=function(values, valueNames, initial,label) {
      if ( length(values) < 1 ) {
        return()
        stop("picker choices must contain at least one value")
      } else if ( length(valueNames) != length(unique(valueNames)) ) {
        stop("picker choices must have unique names (duplicate detected)")
      } else if ( !is.null(initial) ) {
        if (length(initial) != 1)
          stop("initial must be a single object")
        else if ( !(as.character(initial) %in% valueNames) )
          stop("initial doesn't match one of the supplied choices") 
      }
    }
    
  ))

## MultiPicker allows selection of none, one or more from choices
MultiPicker <- setRefClass("MultiPicker",
                           contains="Picker",
                           methods=list(
                             make_gui=function(cont, handler, ...) {
                               n <- dim(cont)[1]
                               cont[n+1, 1] <- label
                               cont[n+1, 2] <- (widget <<- gcheckboxgroup(l$choices, cont=cont, use.table=TRUE))
                               callSuper(cont, handler, ...)
                             },
                             get_value=function(...) {
                               value <- callSuper()
                               if(length(value) == 0 && is.na(value))
                                 value <- character(0)
                               value
                             },
                             validate_inputs=function(...) {}
                           ))

##' Picker  widget
##'
##' Widget to select a single character value from list or select values from list
##' @param ... list of values separated by comma
##' @param initial initial chocie
##' @param label label for widget
##' @param multiple logical. If \code{TRUE} use combobox, \code{FALSE} use table with checkbox
##' @return \code{Picker} object
##' @export
picker <- function(..., initial = NULL, label = "", multiple=FALSE) {
  if(multiple)
    MultiPicker$new(..., initial=initial, label=label)
  else
    Picker$new(..., initial=initial, label=label)
}
