library(tidyverse)
library(Boruta)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3spatiotempcv)
library(paradox)

# Many of the routines in this file are very slow, but don't need to be repeated
# that often. If a modification has been made to the input data, use these flags
# to set whether certain routines run.
DO_BORUTA     <- TRUE
FIT_LMS       <- TRUE
FIT_RFS       <- TRUE
DO_PREDICTION <- TRUE

# Read in the modeling dataframe. To save on memory, we won't load the prediction
# dataframe until modeling is done.
model_df <- read_csv("data_out/model_results/meandepth/meandepth_modeling_df.csv") %>%
  drop_na()

# feature selection via the boruta algorithm
if (DO_BORUTA) {
  boruta <- Boruta(
    meandepth ~ . - lagoslakeid - logarea - log_elev_change - oliver_model_group,
    data=model_df
  )
  save(boruta, file="data_out/model_results/meandepth/boruta_meandepth")
} else {
  load("data_out/model_results/meandepth/boruta_meandepth")
}

boruta_importance <- boruta$ImpHistory %>%
  as_tibble() %>%
  pivot_longer(everything()) %>%
  # get rid of Infs - these appear when Boruta rejects a variable
  filter(!is.infinite(value)) %>%
  group_by(name) %>%
  dplyr::summarize(median_inc_rmse = median(value, na.rm=T),
            sd_inc_rmse = sd(value, na.rm=T),
            n = n()) %>%
  arrange(desc(median_inc_rmse))

important_vars <- boruta_importance %>%
  filter(median_inc_rmse >= quantile(median_inc_rmse, 0.8)) %>% pull(name)

# pick vars that have correlations < 0.7
in_model_vars <- c()
for (variable in important_vars) {
  # if this is the first variable, we must include it
  if (length(in_model_vars) == 0) {
    in_model_vars <- c(in_model_vars, variable)
    next
  }
  # if candidate is a factor, just include it
  if (!is.numeric(model_df[[variable]])) {
    in_model_vars <- c(in_model_vars, variable)
  } else {
    # calculate correlations between candidate and all other variables
    subset <- model_df %>% select(in_model_vars) %>% select_if(is.numeric)
    correlations <- sapply(subset, function(x) cor(x, model_df[[variable]]))
    # accept variable if it is sufficiently uncorrelated with everything else
    if (max(abs(correlations)) < 0.7) {
      in_model_vars <- c(in_model_vars, variable)
    }
  }
}

# After 12.5 in Geocomputation with R, we use spatial CV to get a performance
# estimate and then train the final model on all data.

# Define the prediction task
st_args <- list(
  crs="EPSG:4326",
  coordinate_names=c("lake_lat_decdeg", "lake_lon_decdeg")
)

task_meandepth <- TaskRegrST$new(
  "meandepth",
  model_df,
  "meandepth",
  extra_args=st_args
)
task_meandepth$col_roles$stratum <- "oliver_model_group"

# Messager is just a dummy model to regress on the zmean estimate they provide
lrn_messager <- po("select", selector=selector_name("messager_zmean")) %>>%
  po("learner", learner=lrn("regr.lm")) %>%
  GraphLearner$new(id="messager")

# Khazaei
lrn_khazaei <- po("select", selector=selector_name(
  c("meandepth", "perimeter", "area", "messager_volume", 
    "lake_elevation_m", "ws_area_ha"))) %>>%
  po("learner", learner=lrn("regr.ranger")) %>%
  GraphLearner$new(id="khazaei")

# Lastly, our RF model
lrn_rf <- po("select", selector=selector_name(in_model_vars)) %>>%
  po("learner", learner=lrn("regr.ranger")) %>%
  GraphLearner$new(id="rf")

# We now have all the learners defined, so we can benchmark all of them
# simultaneously to get performance estimates. We now define training
# behaviors.
rsmp_spcv <- rsmp("repeated_spcv_coords", folds=5, repeats=10)

# Model performance metrics
msr_meandepth <- c(msr("regr.rmse"), msr("regr.rsq"), 
                   msr("regr.mae"), msr("regr.pbias"))

if (FIT_LMS) {
  # None of the linear models have hyperparameter tuning, so we can use
  # spatial CV with no inner resampling to derive performance estimates
  design_lm <- data.table::data.table(
    task       = list(task_meandepth),
    learner    = list(lrn_messager),
    resampling = list(rsmp_spcv)
  )

  # Instantiate the resmaplings
  design_lm$resampling = Map(
    function(task, resampling) resampling$clone()$instantiate(task),
    task = design_lm$task, resampling = design_lm$resampling
  )
  
  # And run the benchmark
  bmr <- benchmark(design_lm)
  
  # Generate results as mean of all models
  bmr$aggregate(measures=msr_meandepth) %>%
    select(-resample_result) %>%
    write_csv("data_out/model_results/meandepth/meandepth_lm_performance_metrics.csv")

  # Final train on all data and save the models
  lrn_messager$train(task_meandepth)
  
  save(lrn_messager, file="data_out/model_results/meandepth/model_messager")
} else {
  # All of these should be fitted already
  load("data_out/model_results/meandepth/model_messager")
}


if (FIT_RFS) {
  # The RFs have hyperparameters, so to derive performance estimates for a model
  # fit on all the data, we have to use two resampling loops. The outer loop ensures
  # unbiased performance metrics, while the inner loop ensures unbiased hyperparameter
  # tuning.
  
  # Search space is equivalent for both RF models except for mtry
  search_space_rf <- paradox::ps(
    regr.ranger.mtry = paradox::p_int(lower = 1, upper = length(in_model_vars) - 1),
    regr.ranger.sample.fraction = paradox::p_dbl(lower = 0.2, upper = 0.9),
    regr.ranger.min.node.size = paradox::p_int(lower = 1, upper = 10)
  )
  search_space_khazaei <- paradox::ps(
    regr.ranger.mtry = paradox::p_int(lower = 1, upper = 6 - 1),
    regr.ranger.sample.fraction = paradox::p_dbl(lower = 0.2, upper = 0.9),
    regr.ranger.min.node.size = paradox::p_int(lower = 1, upper = 10)
  )
  
  # Random tuner with 10 iterations - this is verrrrry slow
  tuner <- tnr("random_search")
  terminator <- trm("evals", n_evals=10)
  measure <- msr("regr.rmse")
  inner_resampling <- rsmp("repeated_spcv_coords", folds=4)
  outer_resampling <- rsmp("repeated_spcv_coords", folds=4)
  
  at_rf <- AutoTuner$new(
    lrn_rf, inner_resampling, measure, terminator, tuner, search_space_rf
  )
  at_khazaei <- AutoTuner$new(
    lrn_khazaei, inner_resampling, measure, terminator, tuner, search_space_khazaei
  )
  
  rr_rf <- resample(
    task_meandepth, at_rf, outer_resampling, store_models=TRUE
  )
  rr_khazaei <- resample(
    task_meandepth, at_khazaei, outer_resampling, store_models=TRUE
  )
  
  # Save the resampling results - this is what we use to report model performance
  list(rf=rr_rf$aggregate(measure=msr_meandepth),
       khazaei=rr_khazaei$aggregate(measure=msr_meandepth)) %>%
    as.data.frame() %>% t() %>%
    as.data.frame() %>%
    mutate(learner=rownames(.)) %>%
    write_csv("data_out/model_results/meandepth/meandepth_rf_performance_metrics.csv")
  
  # And do the final tune without the outer loop so we have a usable model
  at_rf$train(task_meandepth)
  at_khazaei$train(task_meandepth)
  
  # save out the results
  save(at_rf, file="data_out/model_results/meandepth/model_rf")
  save(at_khazaei, file="data_out/model_results/meandepth/model_khazaei")
} else {
  load("data_out/model_results/meandepth/model_rf")
  load("data_out/model_results/meandepth/model_khazaei")
}

if (DO_PREDICTION) {
  # Generate new predictions
  obs_df <- read_csv("data_out/model_results/meandepth/meandepth_prediction_df.csv") %>%
    filter(area < 1e7) %>%
    drop_na()
  obs_df$prediction_messager <- lrn_messager$predict_newdata(obs_df)$response
  obs_df$prediction_khazaei   <- at_khazaei$predict_newdata(obs_df)$response
  obs_df$prediction_rf        <- at_rf$predict_newdata(obs_df)$response
  obs_df$in_training <- obs_df$lagoslakeid %in% model_df$lagoslakeid
  
  write_csv(obs_df, "data_out/model_results/meandepth/meandepth_prediction_results.csv")
} else {
  obs_df <- read_csv("data_out/model_results/meandepth/meandepth_prediction_results.csv")
}
