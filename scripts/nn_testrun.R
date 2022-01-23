library(tidyverse)
library(tensorflow)
library(keras)
library(tfdatasets)

source("scripts/util.R")

model_df <- read_csv("data_working/lagosus/lagos_us_model_df.csv") %>%
  filter(area < 1e7)

# the prediction formula comes from Boruta
load("data_working/lagosus/lagos_us_boruta")

formula <- str_c("maxdepth", "~", str_c(confirmed, collapse="+")) %>%
  as.formula()

# log transform confirmed variables with skew > 5
to_transform_ind <- unlist(lapply(
  model_df, function(x) (is.numeric(x) && skew(x) > 5)
))
to_transform_names <- names(model_df)[to_transform_ind]
to_transform_names <- to_transform_names[to_transform_names %in% confirmed]

model_df_small <- model_df %>%
  mutate(across(
    !!to_transform_names,
    safe_log_transform
  )) %>%
  select(maxdepth, !!confirmed)

trainInd <- caret::createDataPartition(model_df_small$maxdepth, p=0.9,
                                       groups=10, list=F)

traindf <- model_df_small[ trainInd, ]
validdf <- model_df_small[-trainInd, ]

my_spec <- feature_spec(
  traindf, 
  -maxdepth, maxdepth
) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>%
  step_categorical_column_with_vocabulary_list(
    all_nominal()
  ) %>%
  fit()

my_spec

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 3)

# Wrap everything up into a function
build_model <- function() {
  input <- layer_input_from_dataset(traindf %>% select(-maxdepth))
  
  output <- input %>% 
    layer_dense_features(dense_features(my_spec)) %>% 
    layer_dense(units = 16, activation="relu",
                kernel_regularizer = regularizer_l2(1)) %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  summary(model)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}

model <- build_model()

history <- model %>% fit(
  x = traindf %>% select(-maxdepth),
  y = traindf$maxdepth,
  epochs = 25,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(early_stop)
)

plot(history)

c(loss, mae) %<-% (
  model %>% 
    evaluate(
      validdf %>% select(-maxdepth), 
      validdf$maxdepth, verbose = 0
    )
)

# make some predictions
validdf$nn_depth <- predict(model, validdf %>% select(-maxdepth))[, 1]

ggplot(validdf, aes(x=maxdepth, y=nn_depth)) + 
  geom_abline(slope=1, intercept=0, color="red") +
  geom_point() +
  xlim(0, 100) +
  coord_equal()

cor(validdf$maxdepth, validdf$nn_depth) ^ 2
sqrt(mean((validdf$maxdepth - validdf$nn_depth) ^ 2))
