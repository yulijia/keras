
`_COMMON_` <- NULL

# reticulate::virtualenv_remove("r-tensorflow")
envir:::set_library_default_pos(value = 3L)
library(envir)
library(magrittr, include.only = c("%>%", "%<>%"))
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
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

    # py_install("git+https://github.com/rr-/docstring_parser.git", "r-tensorflow")

    use_virtualenv("r-tensorflow")
  }
)

inspect <- import("inspect")
docstring_parser <- import("docstring_parser") # broken in py 3.10

keras <- import("tensorflow.keras")
# keras <- import("keras_core")

local({
  `__main__` <- reticulate::import_main()
  `__main__`$`_Layer` <- keras$layers$Layer
  `__main__`$`_ABCMeta` <- import("abc")$ABCMeta
})
is_layer_class <- py_eval(
  "lambda x: isinstance(x, (type, _ABCMeta)) and issubclass(x, _Layer)")

source_python("tools/common.py") # keras_class_type()
rm(r)



print.docstring_parser.common.Docstring <- function(x) {
  cat(docstring_parser$compose(x))
}

get_doc <- function(py_obj, style = "GOOGLE") {

  docstring <- doc <- inspect$getdoc(py_obj)

  if(py_obj$`__name__` == "LSTM" &&
     py_id(py_obj) == py_id(keras$layers$LSTM)) {

    # fix parse error due to bad indentation from one arg
    doc %<>% sub("sample at index i", "  sample at index i", .)
  }

  doc <- tryCatch({
    docstring_parser$parse(doc, style = docstring_parser$DocstringStyle[[style]])
  },
  docstring_parser.common.ParseError = function(e) {
    message("Parsing of ", py_obj$`__name__`, " failed with ", style, " style.")
    docstring_parser$parse(doc, style = docstring_parser$DocstringStyle[["AUTO"]])
  })

  doc$extra_description <- ""
  doc$call_params <- list()

  prep_to_compare <- . %>% strsplit("\n") %>% unlist() %>% trimws()
  doc0 <- prep_to_compare(docstring)
  doc1 <- prep_to_compare(docstring_parser$compose(doc))
  if(!all(doc0 %in% doc1)) {
    doc00 <- strsplit(docstring, "\n")[[1]]
    lost_content <- doc00[doc0 == "" | !doc0 %in% doc1]
    # if(py_obj$`__name__` == "Attention")
    #   browser()

    # try reparsing Call Arguments
    doc2 <- lost_content %>% str_flatten("\n") %>%
      sub("Call arguments:", "Arguments:\n", .) %>%
      docstring_parser$parse()

    call_params <- doc2$params
    doc$call_params <- call_params

    prep_to_compare(docstring_parser$compose(doc2))
    lost_content <- doc00[doc0 == "" | !doc0 %in% c(doc1, doc2)]
    lost_content <- lost_content %>%
      str_flatten("\n") %>%
      str_trim() %>%
      str_split_1("\n")
    if(length(lost_content)) {
      message("Parsing docstring for ", py_obj$`__name__`, " resulted in lost content:\n***")
      writeLines(lost_content)
      doc$extra_description <- glue::trim(lost_content)
      message("***")
    }
  }


  doc$object <- py_obj
  doc
  # style = docstring_parser$DocstringStyle$GOOGLE)
  # ## not all doc strings successfully parse google style,
  # ## some default to REST style
  #
  # TODO: Bug: this lumps class attributes with __init__ args
}


r"(
        docstring = docstring.replace("Args:", "# Arguments")
        docstring = docstring.replace("Arguments:", "# Arguments")
        docstring = docstring.replace("Attributes:", "# Attributes")
        docstring = docstring.replace("Returns:", "# Returns")
        docstring = docstring.replace("Raises:", "# Raises")
        docstring = docstring.replace("Input shape:", "# Input shape")
        docstring = docstring.replace("Output shape:", "# Output shape")
        docstring = docstring.replace("Call arguments:", "# Call arguments")
        docstring = docstring.replace("Returns:", "# Returns")
        docstring = docstring.replace("Example:", "# Example\n")
        docstring = docstring.replace("Examples:", "# Examples\n")

        docstring = re.sub(r"\nReference:\n\s*", "\n**Reference**\n\n", docstring)
        docstring = re.sub(r"\nReferences:\n\s*", "\n**References**\n\n", docstring)

)"

cleanup_description <- function(x) {

  # replace <- function(old, new)
    # x <<- gsub(old, new, x, fixed = TRUE)
#
#   replace("Args:", "# Arguments")
#   replace("Arguments:", "# Arguments")
#   replace("Attributes:", "# Attributes")
#   replace("Returns:", "# Returns")
#   replace("Raises:", "# Raises")
#   replace("Input shape:", "# Input shape")
#   replace("Output shape:", "# Output shape")
#   replace("Call arguments:", "# Call arguments\n")
#   replace("Returns:", "# Returns")
#   replace("Example:", "# Example\n")
#   replace("Examples:", "# Examples\n")


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

  desc <- cleanup_description(str_flatten(c(x$long_description,
                                            "\n\n",
                                            x$extra_description), "\n"))
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


    # browser()
  # if(py_fn$`__name__` == "Layer")
  if(!py_fn$`__name__` %in% c("Layer", "InputLayer"))
    cat("@export")

  x <- textConnectionValue(con)
  x <- stringr::str_flatten(x, "\n")
  x <- gsub("\n", "\n#' ", x)
  x <- str_c("#' ", x, "\n", name)
  str_trim(x)
}


transformers_registry <-
  yaml::read_yaml("tools/arg-transformers.yml") %>%
  lapply(\(args) lapply(args, function(fn) {

    fn <- str2lang(fn)
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


new_layer_wrapper <- function(py_obj) {

  py_obj_name <- py_obj$`__name__`

  ## get transformers
  transformers <- get_arg_transformers(py_obj) # argument transformers

  ## build fn
  frmls <- formals(py_obj)
  frmls$self <- NULL
  py_obj_expr <- substitute(keras$layers$NAME,
                            list(NAME=as.name(py_obj_name)))

  if(py_obj_name == "Input") {
    # no `object` as first arg

    transformers$shape <- quote(normalize_shape)
    fn_body <- bquote({
      # args <- capture_args(.(transformers))
      args <- capture_args(match.call(), .(transformers))
      create_layer(.(py_obj_expr), NULL, args)
    })

  } else if (grepl("layers.merging.", py_repr(py_obj), fixed = TRUE)) {
    # merging layers Add, Subtract, Dot, etc: accept unnamed args as tensors in ...

    frmls <- c(alist(inputs = , ... =), frmls)
    frmls <- frmls[unique(names(frmls))]

    fn_body <- bquote({
      args <- capture_args(match.call(), .(transformers), ignore = c("...", "inputs"))
      # args <- capture_args(.(transformers), ignore = c("...", "inputs"))
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
    # TODO: consider renaming these in keras 3, maybe something like
    # rnn_cell_{gru,simple,stacked,lstm}()
    fn_body <- bquote({
      # args <- capture_args(.(transformers))
      args <- capture_args(match.call(), .(transformers))
      do.call(.(py_obj_expr), args)
    })

  } else if (py_obj_name == "MultiHeadAttention") {
    # first arg is inputs, a list
    # TODO: in keras 3, change signature to:
    #    alist(query, key = query, value = key, ..., call_args = list())
    frmls <- c(alist(inputs = ), frmls)
    fn_body <- bquote({
      # args <- capture_args(.(transformers), ignore = "inputs")
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
      # args <- capture_args(.(transformers), ignore = "object")
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

  if(grepl("UpSampling.D", py_obj_name))
    r_wrapper_name %<>% sub("_up_sampling_", "_upsampling_", .)

  if (grepl(".activation.", py_repr(py_obj), fixed = TRUE))
    if(r_wrapper_name != "layer_activation")
      r_wrapper_name %<>% sub("layer_", "layer_activation_", .)

  if(r_wrapper_name == "layer_activation_p_relu")
    r_wrapper_name <-"layer_activation_parametric_relu"

  fn_string <- str_flatten(c(paste(r_wrapper_name, "<-"), fn_string), "\n")

  docs <- r_doc_from_py_fn(py_obj)

  comment <- paste("#", sub(" at [0-9a-z]+>", ">", py_repr(py_obj)))
  out <- str_flatten(c(comment, docs, fn_string), "\n")
  class(out) <-  "r_py_wrapper2"
  out
}


print.r_py_wrapper2 <- function(x, ...) {layer_random_brightness
  try(clipr::write_clip(x))
  cat(x, "\n")
}
