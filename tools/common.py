

# import keras

def keras_class_type(x):
  if not isinstance(x, (type, _ABCMeta)):
    return ""
  if issubclass(x, keras.layers.Layer):
    return "Layer"
  if issubclass(x, keras.callbacks.Callback):
    return "Callback"
  if issubclass(x, keras.constraints.Constraint):
    return "Constraint"
  if issubclass(x, keras.initializers.Initializer):
    return "Initializer"
  if issubclass(x, keras.optimizers.schedules.LearningRateSchedule):
    return "LearningRateSchedule"
  if issubclass(x, keras.optimizers.Optimizer):
    return "Optimizer"
  if issubclass(x, keras.losses.Loss):
    return "Loss"
  if issubclass(x, keras.metrics.Metric):
    return "Metric"
  return ""

  # if issubclass(x, keras.):
  #   return "Activation"
  # if issubclass(x, keras.applications):
  #   return "Application"

  
