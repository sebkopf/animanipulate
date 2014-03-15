
## Class for holding controls
#' @author John Verzani (gWidgetsManipulate package)
#' @author Sebastian Kopf
ManipulateControls <- setRefClass(
  "ManipulateControls",
  fields=list(
    l="list",
    widget = "ANY",
    label="character",
    initial="ANY",
    handlers="list"
  ),
  methods = list(
    initialize=function(l=list(), label="", initial=NULL, ...) {
      initFields(l=l, label=label, initial=initial, handlers=list())
      callSuper()
    },
    validate_inputs = function(...) {
      "Validate input code"
    },
    get_value=function(...) {
      "Get value of widget"
      svalue(widget)
    },
    #' Set value of widget
    set_value=function(value) {
      svalue(widget) <<- value
    },
    #' make the GUI elements for the control
    #' @param cont container widget
    #' @param handler control changed handler
    make_gui=function(cont, handler) {
      "Create widget, then set initial value"
      svalue(widget) <<- initial
      if (!is.null(handler))
        register_handler('changed', addHandlerChanged(widget, handler=handler))
    },
    register_handler=function(name, id, w = widget){
      handlers[[name]] <<- list(id = id, widget = w)
    },
    block_handlers=function(names = names(handlers)) {
      sapply(handlers[names], function(handler) blockHandler(handler$widget, handler$id))
    },
    unblock_handlers=function(names = names(handlers)) {
      sapply(handlers[names], function(handler) unblockHandler(handler$widget, handler$id))
    }
  ))




