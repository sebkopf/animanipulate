##' @include controls.R
NULL

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

## Reference class for manipulate object
##' @author John Verzani (gWidgetsManipulate package)
##' @author Sebastian Kopf
Manipulate <- setRefClass(
  "Manipulate",
  fields=list(
    .code="ANY",
    .controls="list",
    dev="ANY"
  ),
  methods=list(
    validate_controls = function() {
      "Validate that controls are specified properly"
      ##' From RStudio code
      ## validate that all controls have unique names
      controlNames <- names(.controls)
      duplicatedIndex <- anyDuplicated(controlNames)
      if (duplicatedIndex > 0)
        stop(paste("duplicated control name:", controlNames[[duplicatedIndex]]))
      
      ## iterate over the names and controls, adding the default values to the env
      for (name in names(.controls)) {
        ## check the name
        if (name == "")
          stop("all controls passed to manipulate must be named")
        ## confirm that this is in fact a control
        if(!is(.controls[[name]], "ManipulateControls")) {
          ## not a control, try to make a control
          .controls[[name]] <<- mapVectorToControl(.controls[[name]])
        }
        ## check
        if(!is(.controls[[name]], "ManipulateControls")) {
          stop("Couldn't map control")
        }
        ## default label is control name
        if(nchar(.controls[[name]]$label) == 0) 
          .controls[[name]]$label <<- name
      }
    },
    get_values=function() {
      "Get widget values as list"
      sapply(.controls, function(i) i$get_value(), 
             simplify=FALSE)
    },
    #' Change handler added to all controls
    change_handler=function(...) {
      evaluate_code()
    },
    #' Evaluate code with current control values
    evaluate_code = function() {
      if(is.null(.code)) return()
      values <- get_values()
      
      switch(gtoolkit(),
             "tcltk"={
               tkrplot:::.my.tkdev(dev$hscale, dev$vscale)
               result <- withVisible(eval(.code, envir=values))
               if (result$visible) {
                 eval(print(result$value))
               }
               .Tcl(paste("image create Rplot", dev$image))
             },{             
               result <- withVisible(eval(.code, envir=values))
               if (result$visible) {
                 eval(print(result$value))
               }
             }
      )
    },
    execute = function(cont = gwindow(gettext("ManipulateR"), visible=FALSE)) {
      "Make the GUI"
      pg <- gpanedgroup(cont = cont)
      
      f <- gframe(gettext("Controls"), horizontal=FALSE, cont=pg)
      lyt <- glayout(cont=f)
      ## add controls using make_gui interface
      sapply(.controls, function(i) {
        i$make_gui(cont=lyt, 
                   handler=.self$change_handler)
      })
      
      g <- ggroup(cont = pg, expand=TRUE)
      if(gtoolkit()=="tcltk") {
        require(tkrplot)
        dev <<- tkrplot(getToolkitWidget(g), function() {})
        add(g, dev, expand=TRUE)
      } else {
        gg <- ggraphics(cont=g, expand=TRUE, fill=TRUE)
        visible(gg, TRUE)
      }
      
      if (is(cont, "gWindow"))
        visible(cont) <- TRUE
      svalue(pg) <- 0.5
      evaluate_code()
    },
    initialize=function(code = NULL, ...) {
      controls <- animanipulate:::resolveVariableArguments(list(...))
      initFields(.code=code,
                 .controls=controls)
      validate_controls()
      callSuper()
    }))
