##' @include controls.R
##' @author John Verzani (gWidgetsManipulate package)
NULL

## Slider class
Slider <- setRefClass(
  "Slider",
  contains="ManipulateControls",
  methods=list(
    validate_inputs = function(min, max, initial, step, ticks, label) {
      ## validate inputs
      if (!is.numeric(initial) || !is.numeric(min) || !is.numeric(max))
        stop("min, max, and initial must all be numeric values")
      else if (initial < min)
        stop(paste("slider initial value", initial, "is less than the specified minimum"))
      else if (initial > max)
        stop(paste("slider initial value", initial, "is greater than the specified maximum"))
      else if (min > max)
        stop(paste("slider maximum is greater than minimum"))
      else if ( !is.null(step) ) {
        if ( !is.numeric(step) )
          stop("step is not a numeric value")
        if ( step > (max - min) )
          stop("step is greater than range")
      } else if ( !is.logical(ticks) )
        stop("ticks is not a logical value")
    },
    #' @param validate to allow for derive classes
    #' @note find a better way to include the validation in the constructor 
    #' without having it tripped by deriving a child class (which automatically
    #' calls the constructor)
    initialize=function(min = NULL, max = NULL, initial=min, label="", 
                        step=-1, ticks=TRUE, validate = FALSE, ...) {
      if (validate)
        validate_inputs(min, max, initial, step, ticks, label)
      ## create slider and return it
      slider <- list(type = 0,
                     min = min,
                     max = max,
                     step = step,
                     ticks = ticks)
      callSuper(l=slider, label=label, initial=initial)
    },
    make_gui=function(cont, handler, ...) {
      n <- dim(cont)[1]
      cont[n+1,1] <- label
      if(l$step < 0) l$step <<- 1
      ##                          g <- ggroup(cont=cont, horizontal=TRUE, ...)
      cont[n+1, 2] <- (g <- ggroup(cont=cont, horizontal=TRUE, ...))
      widget <<- gslider(from=l$min, to=l$max, by=l$step, cont=g)
      ## toolkit adjustments
      switch(gtoolkit(),
             "RGtk2" = {
               size(widget) <<- c(200, -1)
             },
             "tcltk" = {
               ## add label to slider
               slider_label <- glabel(svalue(widget), cont=g)
               addHandlerChanged(widget, function(h,...) svalue(slider_label) <- svalue(h$obj))
             }
      )
      callSuper(cont, handler)
    }))


