

# py_install("openai")

openai <- import("openai")

# openai$api_key <- Sys.getenv("OPENAI_API_KEY")

# x = openai$Model$list()

# model <- "gpt-4"

prompt_template <- trimws(r"(
Follow these instructions exactally. Read this docstring for a keras
object, and output yaml detailing what type of transformation function
might be necessary to coerce that particular argument. The coersion is being done for
R objects interfacing with keras in Python using reticulate.
A coersion function is most often necessary for integers and tuples.

For example, the arguments described in the docstring for `keras.layers.Conv2D` are:

Input:
keras.layers.Conv2D
2D convolution layer (e.g. spatial convolution over images).

    This layer creates a convolution kernel that is convolved
    with the layer input to produce a tensor of
    outputs. If `use_bias` is True,
    a bias vector is created and added to the outputs. Finally, if
    `activation` is not `None`, it is applied to the outputs as well.

    When using this layer as the first layer in a model,
    provide the keyword argument `input_shape`
    (tuple of integers or `None`, does not include the sample axis),
    e.g. `input_shape=(128, 128, 3)` for 128x128 RGB pictures
    in `data_format="channels_last"`. You can use `None` when
    a dimension has variable size.

    Examples:

    >>> # The inputs are 28x28 RGB images with `channels_last` and the batch
    >>> # size is 4.
    >>> input_shape = (4, 28, 28, 3)
    >>> x = tf.random.normal(input_shape)
    >>> y = tf.keras.layers.Conv2D(
    ... 2, 3, activation='relu', input_shape=input_shape[1:])(x)
    >>> print(y.shape)
    (4, 26, 26, 2)

    >>> # With `dilation_rate` as 2.
    >>> input_shape = (4, 28, 28, 3)
    >>> x = tf.random.normal(input_shape)
    >>> y = tf.keras.layers.Conv2D(
    ...     2, 3,
    ...     activation='relu',
    ...     dilation_rate=2,
    ...     input_shape=input_shape[1:])(x)
    >>> print(y.shape)
    (4, 24, 24, 2)

    >>> # With `padding` as "same".
    >>> input_shape = (4, 28, 28, 3)
    >>> x = tf.random.normal(input_shape)
    >>> y = tf.keras.layers.Conv2D(
    ... 2, 3, activation='relu', padding="same", input_shape=input_shape[1:])(x)
    >>> print(y.shape)
    (4, 28, 28, 2)

    >>> # With extended batch shape [4, 7]:
    >>> input_shape = (4, 7, 28, 28, 3)
    >>> x = tf.random.normal(input_shape)
    >>> y = tf.keras.layers.Conv2D(
    ... 2, 3, activation='relu', input_shape=input_shape[2:])(x)
    >>> print(y.shape)
    (4, 7, 26, 26, 2)


    Args:
      filters: Integer, the dimensionality of the output space (i.e. the number
        of output filters in the convolution).
      kernel_size: An integer or tuple/list of 2 integers, specifying the height
        and width of the 2D convolution window. Can be a single integer to
        specify the same value for all spatial dimensions.
      strides: An integer or tuple/list of 2 integers, specifying the strides of
        the convolution along the height and width. Can be a single integer to
        specify the same value for all spatial dimensions. Specifying any stride
        value != 1 is incompatible with specifying any `dilation_rate` value !=
        1.
      padding: one of `"valid"` or `"same"` (case-insensitive).
        `"valid"` means no padding. `"same"` results in padding with zeros
        evenly to the left/right or up/down of the input. When `padding="same"`
        and `strides=1`, the output has the same size as the input.
      data_format: A string, one of `channels_last` (default) or
        `channels_first`.  The ordering of the dimensions in the inputs.
        `channels_last` corresponds to inputs with shape `(batch_size, height,
        width, channels)` while `channels_first` corresponds to inputs with
        shape `(batch_size, channels, height, width)`. If left unspecified, it
        uses the `image_data_format` value found in your Keras config file at
        `~/.keras/keras.json` (if exists) else 'channels_last'.
        Note that the `channels_first` format is currently not
        supported by TensorFlow on CPU. Defaults to 'channels_last'.
      dilation_rate: an integer or tuple/list of 2 integers, specifying the
        dilation rate to use for dilated convolution. Can be a single integer to
        specify the same value for all spatial dimensions. Currently, specifying
        any `dilation_rate` value != 1 is incompatible with specifying any
        stride value != 1.
      groups: None, or a positive integer specifying the number of groups in which the
        input is split along the channel axis. Each group is convolved
        separately with `filters / groups` filters. The output is the
        concatenation of all the `groups` results along the channel axis. Input
        channels and `filters` must both be divisible by `groups`.
      activation: Activation function to use. If you don't specify anything, no
        activation is applied (see `keras.activations`).
      use_bias: Boolean, whether the layer uses a bias vector.
      kernel_initializer: Initializer for the `kernel` weights matrix (see
        `keras.initializers`). Defaults to 'glorot_uniform'.
      bias_initializer: Initializer for the bias vector (see
        `keras.initializers`). Defaults to 'zeros'.
      kernel_regularizer: Regularizer function applied to the `kernel` weights
        matrix (see `keras.regularizers`).
      bias_regularizer: Regularizer function applied to the bias vector (see
        `keras.regularizers`).
      activity_regularizer: Regularizer function applied to the output of the
        layer (its "activation") (see `keras.regularizers`).
      kernel_constraint: Constraint function applied to the kernel matrix (see
        `keras.constraints`).
      bias_constraint: Constraint function applied to the bias vector (see
        `keras.constraints`).

    Input shape:
      4+D tensor with shape: `batch_shape + (channels, rows, cols)` if
        `data_format='channels_first'`
      or 4+D tensor with shape: `batch_shape + (rows, cols, channels)` if
        `data_format='channels_last'`.

    Output shape:
      4+D tensor with shape: `batch_shape + (filters, new_rows, new_cols)` if
      `data_format='channels_first'` or 4+D tensor with shape: `batch_shape +
        (new_rows, new_cols, filters)` if `data_format='channels_last'`.  `rows`
        and `cols` values might have changed due to padding.

    Returns:
      A tensor of rank 4+ representing
      `activation(conv2d(inputs, kernel) + bias)`.

    Raises:
      ValueError: if `padding` is `"causal"`.
      ValueError: when both `strides > 1` and `dilation_rate > 1`.

Output:

Conv2D:
  filters:       as.integer
  kernel_size:   as.integer
  dilation_rate: as.integer
  strides:       as.integer
  groups:        as_nullable_integer

)")

as_input <- function(py_obj) {
  sprintf(
    "\n\trepr: %s\n\tformals: %s\ndocstring: %s",
    py_repr(py_obj),
    str_flatten(capture.output(str(formals(py_obj))), "\n"),
    py_obj$`__doc__`
  )
}

# ex <- keras$layers$Conv2D
messages <- list(
  list(role = "system", content = trimws(r"(
Follow these instructions exactally. Parse this docstring for a keras
object, and output yaml detailing what type of transformation function
might be necessary to coerce that particular argument. The coersion is being done for
R objects interfacing with keras in Python using reticulate. Strings, None/NULL, and lists
get converted automatically and don't need a custom transformer.
A coersion function is most often necessary for integers and tuples, and to convert axes
from zero-based to one-based positions. Your most commonly used functions are:
as.integer, as_nullable_integer, normalize_shape, as_axis, but any R function, or R expression is acceptable.
)")),
  list(role = "user", content = as_input(keras$layers$Conv2D)),
  list(role = "assistant", content = r"(
Conv2D:
  filters:       as.integer
  kernel_size:   as.integer
  dilation_rate: as.integer
  strides:       as.integer
  groups:        as_nullable_integer
)"),
  list(role = "user", content = as_input(keras$layers$Dense))
)

# openai$ChatCompletion$create |>
#   reticulate::py_help()

if(FALSE) {

response <- openai$ChatCompletion$create(
  model = "gpt-4",
  messages = messages,
  temperature = 0
)
}
library(glue)
messages_2_prompt <- function(x) {
  system <- glue(trimws(r"(
ASSISTANT's RULE: {x[[1]]$content}
USER: {x[[2]]$content}
ASSISTANT: {x[[3]]$content}
USER:  {x[[4]]$content}
ASSISTANT:
)"))
}


messages_2_prompt2 <- function(x) {
  system <- glue(
r"(
<<SYS>> <s> {x[[1]]$content} </s> <</SYS>>
[INST] <s> user: {x[[2]]$content} </s> [/INST] assistant: <s> {x[[3]]$content} <\s>
[INST] <s> user: {x[[4]]$content} </s> [/INST] assistant: <s> )" )  }



library(httr2)
resp <- request("192.168.1.15:9090/completion") %>%
  req_method("POST") %>%
  req_body_json(list(prompt = messages_2_prompt(messages),
                     temperature = 0,
                     n_predict = 128)) %>%
  req_perform()

resp %>%
  resp_body_json() %>%
  .$content %>%
  cat()
  req_dry_run()

# map_chr(x$data, "id") %>%
#   + grep("gpt", ., value=T)
# [1] "gpt-4-0314"                  "gpt-4-0613"                  "gpt-4"
# [4] "gpt-3.5-turbo-instruct-0914" "gpt-3.5-turbo-instruct"      "gpt-3.5-turbo-0301"
# [7] "gpt-3.5-turbo"               "gpt-3.5-turbo-16k"           "gpt-3.5-turbo-16k-0613"
# [10] "gpt-3.5-turbo-0613"

layers <- names(keras$layers) %>%
  setdiff(c("Layer")) %>% # ,"InputLayer"
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



map(layers, \(l) nchar(l$`__doc__`)) |>
  unlist() %>%
  sort() %>%
  cbind()
  list_c()


  map(layers, \(l) nchar(l$`__doc__`)) |>
    unlist() %>%
    sort() %>%
    head(20) %>%
    names() %>%
    lapply(\(nm) {cat(nm, "\n"); keras$layers[[nm]]$`__doc__` |> cat("\n---\n")}) %>%
      invisible()

responses <- list()
for(i in seq_along(layers) |> tail(length(responses))) {
  cat(py_repr(layers[[i]]), "\n")
  messages[[4]]$content <- as_input(layers[[i]])
  response <- list(openai$ChatCompletion$create(
    model = "gpt-3.5-turbo",
    messages = messages,
    temperature = 0
  ))
  names(response) <- names(layers)[[i]]
  responses[i] <- response
  Sys.sleep(1)
}



responses2 <- list()
for(i in seq_along(layers)) { #|> tail(length(responses2))) {
  cat(py_repr(layers[[i]]), "\n")
  messages[[4]]$content <- as_input(layers[[i]])
  response <-  request("192.168.1.15:9090/completion") %>%
    req_method("POST") %>%
    req_body_json(list(prompt = messages_2_prompt(messages),
                       temperature = 0,
                       n_predict = 512)) %>%
    req_perform() %>%
    list()
  names(response) <- names(layers)[[i]]
  responses2[i] <- response
  # Sys.sleep(1)
}

r <- responses[[1]]

library(future)
library(promises)
plan(multisession)


n_predict <- 1024
responses3 <- list()
for(i in seq(1, length(layers), by = 2)) { #|> tail(length(responses2))) {

  # slots <- list(NULL, NULL)
  l <- layers[[i]]
  cat("\n----", py_repr(l), "----\n")

  messages[[4]]$content <- as_input(l)
  req0 <- request("192.168.1.15:9090/completion") %>%
    req_method("POST") %>%
    req_body_json(list(prompt = messages_2_prompt2(messages),
                       temperature = 0,
                       n_predict = n_predict))

  # resp0 <- futureCall(req_perform, req0) # future(req_perform(req0))

  l <- layers[[i+1]]
  cat("\n----", py_repr(l), "----\n")


  messages[[4]]$content <- as_input(l)
  req1 <- request("192.168.1.15:9091/completion") %>%
    req_method("POST") %>%
    req_body_json(list(prompt = messages_2_prompt2(messages),
                       temperature = 0,
                       n_predict = n_predict))

  # resp1 <- futureCall(req_perform, req1)

  resps <- multi_req_perform(list(req0, req1))   # list(value(resp0), value(resp0))
  # req0p <- promise(req_perform(req0))
  # req1p <- promise(req_perform(req1))

  # resps <- promise_all(req0p, req1p)
  names(resps) <- names(layers)[i:(i+1)]
  responses3[i:(i+1)] <- resps

  lapply(resps, \(r) cat("\n****\n", resp_body_json(r)$content, "\n****\n", sep = ""))

  # response <- req_perform(req)

  # response <- promises::promise_race(gpu1, gpu2)
  # gpu1 <- gpu2 <- NULL
  #   req_perform() %>%
  #   list()
  # names(response3) <- names(layers)[[i]]
  # responses[i] <- response
  # Sys.sleep(1)
}


for()
r %>%
  resp_body_json() %>%
  .$content %>%
  cat()
r <- responses2[[1]]
r$content
lapply(responses, \(r) resp_body_json(r)$content) -> z
 # why is layer_global_average_pooling duplicated in the layers?

# use future for async fetching of responsesh
#




