# Oliver et al. (2017) describe a regional modeling approach where individual
# models are produced within each HUC4 watershed. If a particular HUC4 region
# had few samples, they defaulted to using the population-wide model. We will take
# a similar approach but with a higher sample size threshold (n = 100).

library(tidyverse)
library(tidymodels)
library(tidypredict)
library(Boruta)
library(ranger)
library(randomForest)
library(broom)

# Read the data in using the same procedure as in Boruta.R
parse_huc <- function(s) {
  # convert a possibly mangled HUC string to its correct 12-char string
  if (grepl("+", s, fixed=T)) {
    # it's a malformed float - the leading 4 digits *should* have a valid HUC04
    pow <- str_sub(s, start=str_length(s)-1) %>% as.integer()
    (as.double(s) / (10^pow)) %>% as.character() %>% str_remove(fixed(".")) %>%
      str_pad(12, "right", "0")
  } else {
    # it's a string so just pad it
    return(str_pad(s, 12, side="left", pad="0"))
  }
}

model_df <- read_csv("data_out/lagos_ne_shape.csv") %>%
  inner_join(
    read_csv("data_out/lagos_ne_terrain.csv"),
    by="lagoslakei"
  ) %>%
  # paucity of data, leave it out
  # inner_join(
  #   read_csv("data_out/mglp_lagos_network.csv"),
  #   by="lake_nhdid"
  # ) %>%
  inner_join(
    read_csv(
      "data_out/lagos_ne_geography.csv",
      col_types=cols(lake_huc12=col_character())),
    by=c("lagoslakei"="lagoslakeid")
  ) %>%
  inner_join(
    read_csv("data_working/lagos_ne_depths.csv") %>%
      select(lagoslakei, zmaxobs),
    by="lagoslakei"
  ) %>%
  select(
    -contains("lagoslakei"), -contains("dam"), #-OBJECTID 
    #-nhdplusv2_comid,
    #-lake_nets_upstreamlake_km, -lake_nets_downstreamlake_km,
    #net_dams_n, lake_nets_damonlake_flag
  ) %>%
  mutate(
    lake_huc12_pad = map_chr(lake_huc12, parse_huc),
    lake_huc02 = str_sub(lake_huc12_pad, 1, 2),
    lake_huc04 = str_sub(lake_huc12_pad, 1, 4),
    lake_huc06 = str_sub(lake_huc12_pad, 1, 6),
    #net_id = factor(net_id)
  ) %>% select(-lake_huc12_pad, -lake_huc12)

# train validation split is 80/20
trainInd <- sample(1:nrow(model_df), (nrow(model_df) * 0.8) %>% floor())

traindf <- model_df[ trainInd, ]
validdf <- model_df[-trainInd, ]

# 1. Determine which variables are important with a Boruta run
boruta_run <- Boruta(zmaxobs ~ ., data=model_df, doTrace=2, maxRuns=30)
confirmed <- names(
  boruta_run$finalDecision[boruta_run$finalDecision == "Confirmed"]
)
boruta_formula <- str_c("zmaxobs", "~", str_c(confirmed, collapse="+")) %>%
  as.formula()

# 2. Determine the top 20 variables by importance
imp_rf <- ranger(
  zmaxobs ~ ., #boruta_formula,
  data=traindf,
  importance="permutation"
)

varImp <- imp_rf$variable.importance %>%
  data.frame(variable=names(.), importance=.)

varImp_top <- varImp %>%
  arrange(desc(importance)) %>%
  head(20)

form <- str_c("zmaxobs ~ ", str_c(varImp_top$variable, collapse="+")) %>%
  as.formula()

# 3. Train using the top 20 in each group
n_obs_cutoff <- 100

rf_engine <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

grouped_rf <- traindf %>%
  group_by(lake_huc04) %>%
  mutate(n = n(),
         model_group = ifelse(n > n_obs_cutoff, lake_huc04, "default")) %>%
  group_by(model_group) %>%
  nest() %>%
  mutate(
    rf_model = map(
      data,
      function(d) {
        rf_engine %>%
          fit(form, data=d)
      }
    ),
    augmented = map2(
      data, rf_model,
      function(d, r) {
        d$predicted_zmax <- r$fit$predictions
        return(d)
      }
    )
  )

# pull out the original models
rf_models <- grouped_rf %>% select(model_group, rf_model)
# pool all the training data together
train_result <- grouped_rf %>% select(model_group, augmented) %>%
  unnest(augmented)

cor(train_result$zmaxobs, train_result$predicted_zmax) ^ 2
sqrt(mean((train_result$zmaxobs - train_result$predicted_zmax)^2))

# Doesn't seem to do that much better - now let's try on the validation data.
# It's a bit of a challenge to get it to work at all.
validdf_predicted <- validdf %>%
  mutate(model_group = 
           ifelse(lake_huc04 %in% grouped_rf$model_group, lake_huc04, "default")) %>%
  group_by(model_group) %>%
  nest() %>%
  inner_join(rf_models, by="model_group") %>%
  mutate(
    predicted = map2(
      data, rf_model,
      function(d, r) {
        d$predicted_zmax <- predict(r, d)$.pred
        return(d)
      }
     )
  ) %>%
  select(model_group, predicted) %>%
  unnest(predicted)

cor(validdf_predicted$zmaxobs, validdf_predicted$predicted_zmax) ^ 2
sqrt(mean((validdf_predicted$zmaxobs - validdf_predicted$predicted_zmax)^2))

# Doesn't work at all!
