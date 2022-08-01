

new_py_class <-
  function(classname,
           members = list(),
           inherit = NULL,
           parent_env = parent.frame(),
           convert = TRUE,
           inherit_expr = substitute(inherit)) {

    force(inherit_expr)
    active <- NULL
    for(nm in names(members)) {
      if(is_marked_active(members[[nm]])) {
        active[[nm]] <- members[[nm]]
        members[[nm]] <- NULL
      }
    }
    # R6Class calls substitute() on inherit
    r6_class <- eval(as.call(list(
      quote(R6::R6Class),
      classname = classname,
      public = members,
      active = active,
      inherit = inherit_expr,
      cloneable = FALSE,
      parent_env = parent_env
    )))
    maybe_delayed_r_to_py_R6ClassGenerator(r6_class, convert, parent_env)
  }

#' @rdname new-classes
#' @export
mark_active <- function(x) {
  if(!is.function(x))
    stop("Only R functions can be marked active")
  attr(x, "marked_active") <- TRUE
  x
}

is_marked_active <- function(x)
  identical(attr(x, "marked_active", TRUE), TRUE)


#' @rdname new-classes
#' @export
new_metric_class <-
function(classname, ..., initialize, update_state, result) {
  members <- capture_args(match.call(), ignore = "classname")
  new_py_class(classname, members,
              inherit = keras::keras$metrics$Metric,
              parent_env = parent.frame())
}

#' @rdname new-classes
#' @export
new_loss_class <-
function(classname, ..., call = NULL) {
  members <- capture_args(match.call(), ignore = "classname")

  has_init <- any(c("initialize", "__init__") %in% names(members))
  if(!has_init) {
    # assume this is basically a stateless function,
    # we enable calling it by patching the formals so that
    # it has the sig (y_true, y_pred)
    call_extra_args <- formals(call)
    intersect(names(formals(call)))
  }
  members$call <- call
  LossClass <- new_py_class(classname, members,
                            inherit = keras::keras$losses$Loss,
                            parent_env = parent.frame())

  # TODO: after reticulate builds R func wrappers properly, inspect
  # LayerClass$call here and make sure it's correct, (self, y_true, y_pred).
  # keras.losses.Loss.call implements an @abstractmethod with a strict
  # signature, any additional args in call throw a cryptic error downstream, we
  # should throw a more informative error here instead.

  init_sig <- formals(members$initialize %||% members$`__init__` %||% function(){})
  init_sig$reduction <- init_sig$reduction %||% "auto"
  init_sig$name      <- init_sig$name %||% CamelCase_to_snake_case(classname)

  wrapper_frmls <-  c(alist(y_true = , y_pred = , ... =),
                      init_sig)

  #if init has ..., we make sure to pull ... forward. all args to __init__ must be kwargs
  wrapper_frmls <- wrapper_frmls[!duplicated(names(wrapper_frmls))]

  mask <- new.env(parent = parent.env(environment())) # parent == package:keras namespace
  mask$LossClass <- LossClass

  init_call <- c("...", setNames(nm = names(init_sig)))
  INIT_CALL <- as.call(c(quote(LossClass), lapply(init_call, as.symbol)))

  wrapper <- as.function.default(c(wrapper_frmls, substitute({
    loss_instance <- INIT_CALL
    if (missing(y_true) && missing(y_pred))
      loss_instance
    else
      loss_instance(y_true, y_pred)
  }, list(INIT_CALL = INIT_CALL))),
  envir = mask)

  wrapper
}


new_loss_class <-
  function(classname, ..., call = NULL) {
    members <- capture_args(match.call(), ignore = "classname")


    has_init <- any(c("initialize", "__init__") %in% names(members))
    call_frmls <- formals(members$call)
    call_frmls$self <- NULL
    stopifnot(identical(call_frmls[1:2], alist(y_true =, y_pred =)))
    call_extra_args <- call_frmls[-(1:2)]
    if(length(call_extra_args) && !has_init) {
      # assume this is basically a pure stateless function, and return a wrapper with
      # that allows usage like loss(y_true, y_pred).
      call_fn <- call # methods needs a strict signature: (y_true, y_pred)
      # I heard you liked calls dawg, so I made a call for your call call
      CALL_CALL <- as.call(c(alist(call, y_true, y_pred),
                             lapply(names(call_extra_args), as.symbol)))
      # call_method_mask <-
      #   new.env(parent = parent.env(environment())) # parent == package:keras namespace
      # call_method_mask$call <- call
      call_method_mask <- list2env(c(call_extra_args, call = call),
                                   parent = parent.env(environment())) # parent == package:keras namespace)
      members$call <-
        as.function.default(c(alist(y_true = , y_pred = ),
                              CALL_CALL), envir = call_method_mask)

    }

    members$call <- call
    LossClass <- new_py_class(classname, members,
                              inherit = keras::keras$losses$Loss,
                              parent_env = parent.frame())

    # TODO: after reticulate builds R func wrappers properly, inspect
    # LayerClass$call here and make sure it's correct, (self, y_true, y_pred).
    # keras.losses.Loss.call implements an @abstractmethod with a strict
    # signature, any additional args in call throw a cryptic error downstream, we
    # should throw a more informative error here instead.

    init_sig <- formals(members$initialize %||% members$`__init__` %||% function(){})
    init_sig$reduction <- init_sig$reduction %||% "auto"
    init_sig$name      <- init_sig$name %||% CamelCase_to_snake_case(classname)

    wrapper_frmls <-  c(alist(y_true = , y_pred = , ... =),
                        init_sig)

    #if init has ..., we make sure to pull ... forward. all args to __init__ must be kwargs
    wrapper_frmls <- wrapper_frmls[!duplicated(names(wrapper_frmls))]

    mask <- new.env(parent = parent.env(environment())) # parent == package:keras namespace
    mask$LossClass <- LossClass

    init_call <- c("...", setNames(nm = names(init_sig)))
    INIT_CALL <- as.call(c(quote(LossClass), lapply(init_call, as.symbol)))

    wrapper <- as.function.default(c(wrapper_frmls, substitute({
      loss_instance <- INIT_CALL
      if (missing(y_true) && missing(y_pred))
        call(y_true, y_pred, !!!extra_args)
      else
        loss_instance(y_true, y_pred)
    }, list(INIT_CALL = INIT_CALL))),
    envir = mask)

    wrapper
  }



#' @rdname new-classes
#' @export
new_callback_class <-
function(classname,
         ...,
         on_epoch_begin = NULL,
         on_epoch_end = NULL,
         on_train_begin = NULL,
         on_train_end = NULL,
         on_batch_begin = NULL,
         on_batch_end = NULL,
         on_predict_batch_begin = NULL,
         on_predict_batch_end = NULL,
         on_predict_begin = NULL,
         on_predict_end = NULL,
         on_test_batch_begin = NULL,
         on_test_batch_end = NULL,
         on_test_begin = NULL,
         on_test_end = NULL,
         on_train_batch_begin = NULL,
         on_train_batch_end = NULL) {

  members <- capture_args(match.call(), ignore = "classname")
  members <- drop_nulls(members,
    names(which(vapply(formals(sys.function()), is.null, TRUE))))

  new_py_class(classname, members,
              inherit = keras::keras$callbacks$Callback,
              parent_env = parent.frame())
}


#' @rdname new-classes
#' @export
new_model_class <-
function(classname, ...,
         initialize = NULL, call = NULL,
         train_step = NULL, predict_step = NULL, test_step = NULL,
         compute_loss = NULL, compute_metrics = NULL) {
  members <- capture_args(match.call(), ignore = "classname")
  members <- drop_nulls(members,
    names(which(vapply(formals(sys.function()), is.null, TRUE))))

  new_py_class(classname, members,
              inherit = keras::keras$Model,
              parent_env = parent.frame())
}



#' Define new keras types
#'
#' These functions can be used to make custom objects that fit in the family of
#' existing keras types. For example, `new_layer_class()` will return a class
#' constructor, an object that behaves like other layer functions such as
#' `layer_dense()`. `new_callback_class()` will return an object that behaves
#' similarly to other callback functions, like
#' `callback_reduce_lr_on_plateau()`, and so on. All arguments with a default
#' `NULL` value are optional methods that can be provided.
#'
#' `mark_active()` is a decorator that can be used to indicate functions that
#' should become active properties of the class instances.
#'
#' @rdname new-classes
#' @param classname The classname as a string. Convention is for the classname
#'   to be a CamelCase version of the constructor.
#' @param ... Additional fields and methods for the new type.
#' @param initialize,build,call,get_config,on_epoch_begin,on_epoch_end,on_train_begin,on_train_end,on_batch_begin,on_batch_end,on_predict_batch_begin,on_predict_batch_end,on_predict_begin,on_predict_end,on_test_batch_begin,on_test_batch_end,on_test_begin,on_test_end,on_train_batch_begin,on_train_batch_end,update_state,result,train_step,predict_step,test_step,compute_loss,compute_metrics Optional methods that can be overridden.
#' @param x A function that should be converted to an active property of the class type.
#'
#' @return A new class generator object that inherits from the appropriate Keras
#'   base class.
#' @export
new_layer_class <-
function(classname, ...,
         initialize = NULL, build = NULL, call = NULL, get_config = NULL) {
  members <- capture_args(match.call(),  ignore = "classname")
  members <- drop_nulls(members,
    names(which(vapply(formals(sys.function()), is.null, TRUE))))

  type <- new_py_class(classname, members,
                      inherit = keras$layers$Layer,
                      parent_env = parent.frame())

  create_layer_wrapper(type)
}
