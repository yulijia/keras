

envir::attach_source("tools/setup.R")

# "TextVectorization" %in% names(keras$layers)


layers <- names(keras$layers) %>%
  setdiff(c("Layer")) %>% # ,"InputLayer"
  # setdiff(c("Layer")) %>% # ,"InputLayer"
  grep("GlobalAvgPool.D", ., value = TRUE, invert = TRUE) %>%  # alias for GlobalAveragePooling
  grep("GlobalMaxPool.D", ., value = TRUE, invert = TRUE) %>%  # alias for GlobalMaxPooling1D
  sort() %>%
  set_names() %>%
  lapply(function(nm) {
  obj <- keras$layers[[nm]]

  if(obj$`__name__` == "Input")
    return(obj)

  if(is_layer_class(obj)) obj
  else {
    message("skipping ", nm)
    NULL
  }
}) %>%
  keras:::drop_nulls()

lapply(layers, new_layer_wrapper) %>%
  unlist() %>%
  str_flatten(collapse = "\n\n\n") %>%
  writeLines("R/zzzz.R")

# styler::style_file("R/zzzz.R")



# print.r_py_wrapper2 <- function(x, ...) {
#   try(clipr::write_clip(x))
#   cat(x)
# }



# new_layer_wrapper(keras$layers$Embedding)
#
# new_layer_wrapper(keras$layers$BatchNormalization)

## example usage:
# new_layer_wrapper(keras$layers$TextVectorization)
# new_layer_wrapper(keras$layers$DepthwiseConv1D)
# new_layer_wrapper(keras$layers$UnitNormalization)
# new_layer_wrapper(keras$layers$Attention)
# new_layer_wrapper(keras$layers$Discretization)
# new_layer_wrapper(keras$layers$GaussianDropout) |> print()
# new_layer_wrapper(keras$layers$GaussianNoise) |> print()
# new_layer_wrapper(keras$layers$IntegerLookup) |> print()
# new_layer_wrapper(keras$layers$Normalization) |> print()
# new_layer_wrapper(keras$layers$StringLookup) |> print()

