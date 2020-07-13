rm(list = ls())

FLAGS = flags(
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_string("activation1", "relu"),
  flag_string("activation2", "relu")
  )

model = keras_model_sequential()
model %>%
  layer_dense(units = 20, activation = FLAGS$activation1, input_shape = c(15)) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = 10, activation = FLAGS$activation2,
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(lr = 0.001, beta_1 = 0.9, beta_2 = 0.999),
  metrics = c('accuracy')
)


history = model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 20,
  view_metrics = TRUE,
  validation_split = 0.2
)


score = model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

cat('Test loss:', score$loss, '\n')
cat('Test accuracy:', score$acc, '\n')