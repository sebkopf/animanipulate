##' @include manipulate.R
NULL

## Manipulate command ala RStudio implemented with gWidgets
## (with added animation controls).
##
## Built on codebase provided by John Verzani's gWidgetsManipulate package
## and RStudio's manipulate package.
##
## Original license for RStudio's manipulate package
##
## Copyright (C) 2009-11 by RStudio, Inc.
##
## This program is licensed to you under the terms of version 3 of the
## GNU Affero General Public License. This program is distributed WITHOUT
## ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
## MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
## AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
##
##
## THe main point of AGPL:
##   The GNU Affero General Public License is designed specifically to
## ensure that, in such cases, the modified source code becomes available
## to the community.  It requires the operator of a network server to
## provide the source code of the modified version running there to the
## users of that server.  Therefore, public use of a modified version, on
## a publicly accessible server, gives the public access to the source
## code of the modified version.

#' Reference class for manipulate object
#' Expands functionality of the Manipulate object from John Verzani's gWidgetsManipulate package
Animanipulate <- setRefClass(
  "Animanipulate",
  contains = "Manipulate",
  fields = list (
    .controls_animated = 'integer', # stores indices of animated controls
    .controls_on = 'logical', # whether any of the animated controls are turned on
    last_run = "POSIXct", # when the animation was last run 
    animating = 'logical', # stores status of animation
    interval = 'numeric' # animation interval (in seconds)
  ),
  methods = list(   
    #' constructor
    initialize = function(...) {
      callSuper(...)
      initFields(interval = 0.2, animating = FALSE, .controls_on = FALSE)
      .controls_animated <<- which(sapply(.controls, function(i) is(i, "AnimationControls")))
    },
    
    #' animate the plot
    animate = function(){
      animating <<- TRUE
      while (.controls_on == TRUE) {
        # make note of run time
        last_run <<- Sys.time()
        
        # take a time step for each animated control that is turned on
        sapply(.controls[.controls_animated], function(i) if (i$on) i$take_step())
        
        # evaluate code
        evaluate_code()
        
        # sleep for remaining part of interval (but at least 1 ms)
        Sys.sleep(max(0.001, interval - difftime(Sys.time(), last_run)))
      }
      animating <<- FALSE
    },
    
    #' Change handler added to all controls
    change_handler=function(...) {
      if (!animating)
        evaluate_code()
    },

    #' make GUI
    execute = function(cont = gwindow(gettext("AnimanipulateR"), visible=FALSE)) {
      # idle handler to updated the .controls
      addHandlerIdle(cont, function(...) .controls_on <<- any(sapply(.controls[.controls_animated], function(i) i$on)), interval = 200)
      
      # idle handler to trigger animation as soon whenever a control turns on
      addHandlerIdle(cont, function(...) if (!animating && .controls_on) animate(), interval = 200)
      
      callSuper(cont)
    }
  ))

##' Manipulate command ala RStudio
##' (with added animation controls).
##' 
##' Due to the event loop structure of tcltk, this only works in the GTK
##' toolkit at the moment.
##'
##' @param ._expr expression to produce output.
##' @param ... used to specify controls. See \code{picker},
##' \code{checkbox}, \code{slider}.
##'
##' @return makes output, returns Animanipulate object
##' @export
##' @examples
##' \dontrun{
##' manipulate(## expression
##' {
##'   size <- sqrt.size^2
##'   y <- get(distribution)(size)
##'   plot(density(y, bw = bandwidth/100, kernel=kernel), 
##'        axes = axes, ann = label,
##'        main = paste("kernel:", kernel))
##'   points(y, rep(0, size))
##' },
##' ## controls
##' distribution = picker("rnorm", "rexp"),
##' kernel = picker("gaussian", "epanechnikov", 
##'                 "rectangular", "triangular", "cosine"),
##' sqrt.size = slider(5, 30, step = 1, init = 10), 
##' bandwidth = slider(5, 200, step = 5, init = 100, 
##'                    label = "bandwith [%]"), # using % here b/c some toolkits only allow integers (GTK is fine with real)
##' axes = checkbox(TRUE, label="Draw Axes"),
##' label = checkbox(TRUE, label="Draw Labels")
##' )
##' }
manipulate <- function(._expr,...) {
  options(guiToolkit = "RGtk2") # event loop for animation does not work in tcltk
  obj <- Animanipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}



