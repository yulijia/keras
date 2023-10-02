
library(reticulate)
library(purrr)
library(envir)
# library(TKutils)
use_virtualenv("r-tensorflow")



layers <- lapply(set_names(names(keras$layers)), function(nm) {
  obj <- keras$layers[[nm]]

  if(py_is_class(obj) &&
     py_is_subclass(obj, keras$layers$Layer)) obj #%error% browser()) obj
  else NULL
}) %>%
  keras:::drop_nulls()

layers <- map(layers, function(Layer) {

  r_wrapper_name <- sprintf("layer_%s", snakecase::to_snake_case(Layer$`__name__`))

  # layer_conv_1_d  ->  layer_conv_1d
  r_wrapper_name <- sub("_([0-9])_d$", "_\\1d", r_wrapper_name)

  transformers <- NULL
  frmls <- formals(Layer)
  for(i in seq_along(frmls)) {
    key <- names(frmls)[i]
    if(identical(unname(frmls[i]), list(quote(expr = ))))
      next
    val <- frmls[[i]]

    if(is.integer(val))
      transformers[[key]] <- quote(as.integer)
    if(key == "axis")
      transformers[[key]] <- quote(as_axis)
  }

  py_obj_expr <- substitute(keras$layers$NAME, list(NAME=as.name(Layer$`__name__`)))
  fn_body <- bquote({
    args <- capture_args(match.call(), .(transformers), ignore = "object")
    create_layer(.(py_obj_expr), object, args)
  })

  fn <- rlang::new_function(
    c(alist(object = ), formals(Layer)),
    fn_body, .GlobalEnv)

  fn

  sprintf("\n#' @export\n%s <- %s\n", r_wrapper_name, paste0(c(deparse(fn)), collapse = "\n"))

})

writeLines(paste0(unlist(layers), collapse = "\n\n"), "R/zzzz.R")

styler::style_file("R/zzzz.R")
