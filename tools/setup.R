

# ---- imports ----
# reticulate::virtualenv_remove("r-tensorflow")
envir:::set_library_default_pos(value = 3L)
library(reticulate)
tryCatch(
  use_virtualenv("r-tensorflow"),
  error = function(e) {
    reticulate::virtualenv_create(
      "r-tensorflow", "3.10",
      packages = unique(c("tensorflow", "keras-core",
                   "git+https://github.com/rr-/docstring_parser.git",
                   # "docstring_parser",
                   # "docstring-parser",
                   "ipython",

                   # keras:::default_extra_packages() %>% as.vector() %>% dput()
                   c("tensorflow-hub", "tensorflow-datasets", "scipy", "requests",
                     "Pillow", "h5py", "pandas", "pydot"),

                   "scipy", "pandas")))

    # py_install("git+https://github.com/rr-/docstring_parser.git",
               # "r-tensorflow")

    use_virtualenv("r-tensorflow")
  }
)

# library(tidyverse)
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(envir)
library(magrittr, include.only = c("%>%", "%<>%"))

inspect <- import("inspect")
docstring_parser <- import("docstring_parser") # broken in py 3.10

keras <- import("tensorflow.keras")
# keras <- import("keras_core")

`__main__` <- reticulate::import_main()
`__main__`$`_Layer` <- keras$layers$Layer
`__main__`$`_ABCMeta` <- import("abc")$ABCMeta
is_layer_class <- py_eval(
  "lambda x: isinstance(x, (type, _ABCMeta)) and issubclass(x, _Layer)")



# ---- docstrings ----
print.docstring_parser.common.Docstring <- function(x) {
  cat(docstring_parser$compose(x))
}

get_doc <- function(py_obj, style = "GOOGLE") {

  doc <- inspect$getdoc(py_obj)

  if(py_obj$`__name__` == "LSTM" &&
     py_id(py_obj) == py_id(keras$layers$LSTM)) {

    doc <- doc %>%
      sub("sample at index i", "  sample at index i", .)
    # %>%
      # strsplit("\n") %>% .[[1L]] %>%
      # trimws("right") %>% {
      #   .[-1] <- paste0("    ", .[-1])
      #   .
      # } %>%
      # sub("^  ", "    ", .) %>%
      # sub("^      ", "        ", .) %>%
      # str_flatten("\n") %>%
      # cat() %>%
      # docstring_parser$parse(., style = docstring_parser$DocstringStyle[[style]])

    # browser()
  }

  # if(grepl("mple at index i in a batch will be used as initial state for the samp", doc))
  #   browser()

  doc <- tryCatch({
    docstring_parser$parse(doc, style = docstring_parser$DocstringStyle[[style]])
  }, docstring_parser.common.ParseError = function(e) {
    message("Parsing of ", py_obj$`__name__`, " failed with ", style, " style.")
    docstring_parser$parse(doc, style = docstring_parser$DocstringStyle[["AUTO"]])
  })

  doc$object <- py_obj
  doc
  # style = docstring_parser$DocstringStyle$GOOGLE)
  # ## not all doc strings successfully parse google style,
  # ## some default to REST style
  #
  # TODO: Bug: this lumps class attributes with __init__ args
}



cleanup_description <- function(x) {

  ## TODO: try glue
  # remove leading and trailing whitespace
  # x <- gsub("^\\s+|\\s+$", "", x)

  # convert 2+ whitespace to 1 ws
  # x <- gsub("(\\s\\s+)", " ", x)

  if(!length(x))
    return(x)
    # browser()
  # convert literals
  x <- gsub("None", "NULL", x, fixed = TRUE)
  x <- gsub("True", "TRUE", x, fixed = TRUE)
  x <- gsub("False", "FALSE", x, fixed = TRUE)

  # convert tuple to list
  x <- gsub("tuple", "list", x, fixed = TRUE)
  x <- gsub("list/list", "list", x, fixed = TRUE)


  x <- unlist(strsplit(x, "\n"))
  in_code_block <- FALSE
  for (i in seq_along(x)) {
    if (!in_code_block) {
      if (startsWith(x[i], ">>>")) {
        in_code_block <- TRUE
        x[i] <- paste0("```python\n", x[i])
      }
    } else {
      if (x[i] == "") {
        x[i] <- "```\n"
        in_code_block <- FALSE
      }
    }
  }
  if(in_code_block)
    x <- c(x, "```\n")

  x <- paste0(x, collapse = "\n")
  # if(grepl(">>> tf.keras.layers.Minimum()([np.arange(5).reshape(5, 1),", x, fixed = TRUE))
  #   browser()


  # convert inline shapes to be non links
  #  `[batch_size, Tq, dim]`

  x
}

r_doc_from_py_fn <- function(py_fn, name = NULL) {
  con <- textConnection("r-doc", "w")
  on.exit(close(con))
  cat <- function(...,  file = con)
    base::cat(..., "\n", file = file)

  x <- get_doc(py_fn)


  # first sentence is taken as title
  # 2nd paragraph is taken as @description
  # 3rd paragraph + is taken as @details

  title <- cleanup_description(x$short_description)
  # title should have no trailing '.'
  if (str_sub(title, -1) == ".")
    title <- str_sub(title, end = -2)

  # cat("@title ", title)
  cat(title)

  desc <- cleanup_description(x$long_description)
  cat()

  # avoid splitting across @description and @details,
  # so put everything in @details
  if (length(desc) != 0 && str_detect(desc, "\n")) {
    # cat("@description") # description can't be empty
    cat("@details")
  }
  cat(desc)

  for (p in x$params) {
    if (p$arg_name %in% c("name", "dtype")) next
    cat("\n@param", p$arg_name)
    cat(cleanup_description(p$description))
    # cat()
  }

  cat("\n@param ... standard layer arguments.")
  # TODO: @inheritDotParams keras.layers.Layer

  cat()

  py_full_name <- paste0(py_fn$`__module__`, ".", py_fn$`__name__`)
  cat("@seealso")
  # need to make a lookup table for these urls
  cat("  +  <https://keras.io/api/layers>")

  cat("@export")

  x <- textConnectionValue(con)
  x <- stringr::str_flatten(x, "\n")
  x <- gsub("\n", "\n#' ", x)
  x <- str_c("#' ", x, "\n", name)
  x
}


transformers_registry <-
  yaml::read_yaml("tools/arg-transformers.yml") %>%
  lapply(\(args) lapply(args, function(fn) {
    fn %<>% str2lang()
    if (is.call(fn) && !identical(fn[[1]], quote(`function`)))
      fn <- as.function.default(c(alist(x =), fn))
    fn
  }))

get_arg_transformers <- function(py_obj) {
  py_obj_name <- py_obj$`__name__`


  transformers <- list()


  # if(py_obj_name == "AveragePooling3D")
  #   browser()

  frmls <- formals(py_obj)
  for (i in seq_along(frmls)) {
    key <- names(frmls)[i]

    if(!is.null(t <- transformers_registry$ALL_ARGS[[key]])) {
      transformers[[key]] <- t
      next
    }

    if (identical(unname(frmls[i]), list(quote(expr =)))) {
      # arg missing, inspect doc maybe to see if we should use
      # as.integer
      # if(py_obj_name == "RepeatVector")
      #   browser()
      doc <- get_doc(py_obj)
      for (p in doc$params) {
        if (p$arg_name == key && grepl("[Ii]nteger", p$description)) {
          transformers[[key]] <- if(grepl("None", p$description))
            quote(as_nullable_integer) else quote(as.integer)
          break
        }
      }
      next
    }

    val <- frmls[[i]]
    if (is.integer(val))
      transformers[[key]] <- quote(as.integer)

    # if (key == "axis")
      # transformers[[key]] <- quote(as_axis)
  }

  if(length(pre_registered <- transformers_registry[[py_obj_name]]))
    transformers %<>% modifyList(pre_registered)

  for (glob in grep("*", names(transformers_registry), value = TRUE)) {
    if (grepl(glob2rx(glob), py_obj_name))
      transformers %<>% modifyList(transformers_registry[[glob]])
  }


  if (!length(transformers))
    transformers <- NULL

  # if(py_obj_name == "AveragePooling3D")
  #   browser()

  transformers
}


# new_layer_wrapper <-
  function(py_obj) {

  py_obj_name <- py_obj$`__name__`

  ## get transformers
  transformers <- get_arg_transformers(py_obj) # argument transformers

  #
  #   # Conv2d and friends
  #   if (grepl("Conv", py_obj_name)) {
  #     transformers$filters <- quote(as.integer)
  #     transformers[c("kernel_size", "dilation_rate", "strides")] <-
  #       list(quote(as_integer_tuple))
  #   }
  #
  #   if (py_obj_name %in% c("Hashing"))
  #     transformers %<>% modifyList(as.list(alist(
  #       num_bins = as.integer,
  #       salt = as_nullable_integer)))
  #
  #
  #   if (py_obj_name %in% c("TextVectorization"))
  #     transformers %<>% modifyList(as.list(alist(
  #         max_tokens = as_nullable_integer,
  #         output_sequence_length = as_nullable_integer,
  #         ngrams = function(x) if(length(x) > 1) as_integer_tuple(x) else as_nullable_integer(x)
  #       )))
  #
  #
  #   transformers$input_shape <- quote(normalize_shape)
  #   transformers$batch_size <- quote(as_nullable_integer)
  #   transformers$batch_input_shape <- quote(normalize_shape)

  # if(!length(transformers))
  #   transformers <- NULL

  ## build fn
  frmls <- formals(py_obj)
  frmls$self <- NULL
  py_obj_expr <- substitute(keras$layers$NAME,
                            list(NAME=as.name(py_obj_name)))

  # if(py_obj_name == "Add")
  # browser()

  if(py_obj_name == "Input") {

    transformers$shape <- quote(normalize_shape)
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers))
      create_layer(.(py_obj_expr), NULL, args)
    })

    # fn <-
    # fn_body <- do.call(substitute, list(fn_body, list(object = NULL)))
    # # replace ignore="object" to ignore=NULL
    # #      <-   cl()
    # fn_body[[2L]][[3L]]$ignore <- NULL
    # # no need for compose_layer; --> relace create_layer() with do.call
    # fn_body[[3L]] <- bquote(do.call(.(py_obj_expr), args))

  } else if (grepl("layers.merging.", py_repr(py_obj), fixed = TRUE)) {
    frmls <- c(alist(inputs = , ... =), frmls)
    browser()
    fn_body <- bquote({
      if (missing(inputs))
        return(keras$layers$Add(...))
      if (!is.list(inputs))
        inputs <- list(inputs)
      dots <- split_dots_named_unnamed(list(...))
      inputs <- c(inputs, dots$unnamed)
      do.call(keras$layers$add, c(list(inputs), dots$named))
    })

  } else {

    frmls <- c(alist(object = ), frmls)
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers), ignore = "object")
      create_layer(.(py_obj_expr), object, args)
    })

  }

  fn <- as.function(c(frmls, fn_body))

  fn_string <- deparse(fn)

  # deparse adds a space for some reason
  # fn_string <- sub("function (", "function(", fn_string, fixed = TRUE)

  r_wrapper_name <-  snakecase::to_snake_case(py_obj$`__name__`) %>%
    # conv_1_d  ->  conv_1d
    sub("_([0-9])_d(_|$)", "_\\1d\\2", .) %>%
    # ReLu -> re_lu -> relu
    sub("(re)_(lu)", "\\1\\2", .) %>%
    sprintf("layer_%s <- ", .)

  fn_string <- str_flatten(c(r_wrapper_name, fn_string), "\n")
  docs <- r_doc_from_py_fn(py_obj)
  out <- str_flatten(c(docs, fn_string), "")
  # class(out) <-  "r_py_wrapper2"
  out
}


new_layer_wrapper <- function(py_obj) {

  py_obj_name <- py_obj$`__name__`

  ## get transformers
  transformers <- get_arg_transformers(py_obj) # argument transformers

  #
  #   # Conv2d and friends
  #   if (grepl("Conv", py_obj_name)) {
  #     transformers$filters <- quote(as.integer)
  #     transformers[c("kernel_size", "dilation_rate", "strides")] <-
  #       list(quote(as_integer_tuple))
  #   }
  #
  #   if (py_obj_name %in% c("Hashing"))
  #     transformers %<>% modifyList(as.list(alist(
  #       num_bins = as.integer,
  #       salt = as_nullable_integer)))
  #
  #
  #   if (py_obj_name %in% c("TextVectorization"))
  #     transformers %<>% modifyList(as.list(alist(
  #         max_tokens = as_nullable_integer,
  #         output_sequence_length = as_nullable_integer,
  #         ngrams = function(x) if(length(x) > 1) as_integer_tuple(x) else as_nullable_integer(x)
  #       )))
  #
  #
  #   transformers$input_shape <- quote(normalize_shape)
  #   transformers$batch_size <- quote(as_nullable_integer)
  #   transformers$batch_input_shape <- quote(normalize_shape)

  # if(!length(transformers))
  #   transformers <- NULL

  # tra

  ## build fn
  frmls <- formals(py_obj)
  frmls$self <- NULL
  py_obj_expr <- substitute(keras$layers$NAME,
                            list(NAME=as.name(py_obj_name)))

  if(py_obj_name == "Input") {

    transformers$shape <- quote(normalize_shape)
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers))
      create_layer(.(py_obj_expr), NULL, args)
    })

    # fn <-
    # fn_body <- do.call(substitute, list(fn_body, list(object = NULL)))
    # # replace ignore="object" to ignore=NULL
    # #      <-   cl()
    # fn_body[[2L]][[3L]]$ignore <- NULL
    # # no need for compose_layer; --> relace create_layer() with do.call
    # fn_body[[3L]] <- bquote(do.call(.(py_obj_expr), args))

  } else if (grepl("layers.merging.", py_repr(py_obj), fixed = TRUE)) {

    # accept tensors/layers in ..., collapse into `inputs` list for call method
    frmls <- c(alist(inputs = , ... =), frmls)
    frmls <- frmls[unique(names(frmls))]

    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers), ignore = c("...", "inputs"))
      dots <- split_dots_named_unnamed(list(...))
      if (missing(inputs))
        inputs <- NULL
      else if (!is.null(inputs) && !is.list(inputs))
        inputs <- list(inputs)
      inputs <- c(inputs, dots$unnamed)
      args <- c(args, dots$named)

      layer <- do.call(.(py_obj_expr), args)

      if(length(inputs))
        layer(inputs)
      else
        layer
    })

  } else if (grepl(".rnn.", py_repr(py_obj), fixed = TRUE) &&
             grepl("Cells?$", py_obj_name)) {
    # layer_gru_cell() and friends don't compose w/ `object`
    # #TODO: consider renaming these in keras 3, maybe something like
    # rnn_cell_{gru,simple,stacked,lstm}()
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers))
      do.call(.(py_obj_expr), args)
    })

  } else if (py_obj_name == "MultiHeadAttention") {
    # first arg is inputs, a list
    frmls <- c(alist(inputs = ), frmls)
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers), ignore = "inputs")
      layer <- do.call(.(py_obj_expr), args)
      if (missing(inputs) || is.null(inputs))
        return(layer)
      if (!is.list(inputs))
        inputs <- list(inputs)
      do.call(layer, inputs)
    })

  } else {

    frmls <- c(alist(object = ), frmls)
    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers), ignore = "object")
      create_layer(.(py_obj_expr), object, args)
    })

  }

  fn <- as.function(c(frmls, fn_body))
  fn <- rlang::zap_srcref(fn)

  fn_string <- deparse(fn)

  # deparse adds a space for some reason
  fn_string <- sub("function (", "function(", fn_string, fixed = TRUE)

  r_wrapper_name <- snakecase::to_snake_case(py_obj$`__name__`) %>%
    # conv_1_d  ->  conv_1d
    sub("_([0-9])_d(_|$)", "_\\1d\\2", .) %>%
    # ReLu -> re_lu -> relu
    sub("(re)_(lu)", "\\1\\2", .) %>%
    sprintf("layer_%s", .)

  if(grepl("UpSampling.D", py_obj_name)) {
    r_wrapper_name %<>% sub("_up_sampling_", "_upsampling_", .)
  }

  if(r_wrapper_name == "layer_activation") {

  } else if (grepl(".activation.", py_repr(py_obj), fixed = TRUE)) {
    r_wrapper_name %<>% sub("layer_", "layer_activation_", .)
  }

  if(r_wrapper_name == "layer_activation_p_relu")
    r_wrapper_name <-"layer_activation_parametric_relu"

  fn_string <- str_flatten(c(paste(r_wrapper_name, "<-"), fn_string), "\n")

  docs <- r_doc_from_py_fn(py_obj)
  out <- str_flatten(c("# ", sub(" at [0-9a-z]+>", ">", py_repr(py_obj)), "\n", docs, fn_string), "")
  # class(out) <-  "r_py_wrapper2"
  out
}


