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
DO_BORUTA     <- FALSE
FIT_LMS       <- TRUE
FIT_RFS       <- TRUE
DO_PREDICTION <- TRUE

# Read in the modeling dataframe. To save on memory, we won't load the prediction
# dataframe until modeling is done.
model_df <- read_csv("data_out/model_results/maxdepth/maxdepth_modeling_df.csv")

# feature selection via the boruta algorithm
if (DO_BORUTA) {
  boruta <- Boruta(
    maxdepth ~ . - lagoslakeid - logarea - log_elev_change - oliver_model_group,
    data=model_df
  )
  save(boruta, file="data_out/model_results/maxdepth/boruta_maxdepth")
} else {
  load("data_out/model_results/maxdepth/boruta_maxdepth")
}

boruta_importance <- boruta$ImpHistory %>%
  as_tibble() %>%
  pivot_longer(everything()) %>%
  # get rid of Infs - these appear when Boruta rejects a variable
  filter(!is.infinite(value)) %>%
  group_by(name) %>%
  summarize(median_inc_rmse = median(value, na.rm=T),
            sd_inc_rmse   = sd(value, na.rm=T),
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

task_maxdepth <- TaskRegrST$new(
  "maxdepth",
  model_df,
  "maxdepth",
  extra_args=st_args
)
task_maxdepth$col_roles$stratum <- "oliver_model_group"

# Make pipelines that select the relevant features etc.
# Heathcote 
lrn_heathcote <- po("mutate", 
                    mutation=list(
                      log_elev_change =~ log(pmax(elev_median - elev_min, 1)))
                    ) %>>%
    po("select", selector=selector_name(c("maxdepth", "log_elev_change"))) %>>%
    po("learner", learner=lrn("regr.lm")) %>%
  ppl("targettrafo", graph=.) %>%
  GraphLearner$new(id="heathcote")

# set the target transformation and inversion  
lrn_heathcote$graph$param_set$values$targetmutate.trafo <- 
  function(x) log(x)
lrn_heathcote$graph$param_set$values$targetmutate.inverter <- 
  function(x) list(response=exp(x$response))

# Sobek
lrn_sobek <- po("mutate", mutation=list(logarea =~ log(area))) %>>%
  po("select", selector=selector_name(c("maxdepth", "logarea", "slope_max"))) %>>%
  po("learner", learner=lrn("regr.lm")) %>%
  GraphLearner$new(id="sobek")

# Khazaei
lrn_khazaei <- po("select", selector=selector_name(
  c("maxdepth", "perimeter", "area", "messager_volume", 
    "lake_elevation_m", "ws_area_ha"))) %>>%
  po("learner", learner=lrn("regr.ranger")) %>%
  GraphLearner$new(id="khazaei")

# Oliver
# this is a little harder because of the scaling and mixed effects syntax
lrn_oliver <- po("select", selector=selector_name(
    c("maxdepth", "area", "sdi", "ws_lake_arearatio", "slope_max", 
      "oliver_model_group"))
  ) %>>%
  po("colapply", applicator=function(x) log(pmax(x, 0.01)),
     affect_columns=selector_name(
       c("area", "sdi", "ws_lake_arearatio", "slope_max"))
  ) %>>%
  po("scale") %>>%
  po("encodelmer", affect_columns=selector_name("oliver_model_group")) %>>%
  po("learner", learner=lrn("regr.lm")) %>%
  ppl("targettrafo", graph=.) %>%
  GraphLearner$new(id="oliver")

# We don't know what distribution of depths will be provided by new data, 
# so we have to hardcode the scaling constants to match the training set.
# I don't think it matters much if centering/scaling isn't *exact* across CV
# folds.
maxdepth_scaled <- scale(log(model_df$maxdepth))
attr(maxdepth_scaled, "scaled:center")
attr(maxdepth_scaled, "scaled:scale")
lrn_oliver$param_set$values$targetmutate.trafo <- 
    function(x) (log(x) - 1.873154) * 0.8399446
lrn_oliver$param_set$values$targetmutate.inverter <- 
    function(x) list(response=exp(x$response * 0.8399446 + 1.873154))

# Hollister
lrn_hollister <- po("select", selector=selector_name("linear_term")) %>>%
  po("learner", learner=lrn("regr.lm")) %>%
  GraphLearner$new(id="hollister")

# Lastly, our RF model
lrn_rf <- po("select", selector=selector_name(in_model_vars)) %>>%
  po("learner", learner=lrn("regr.ranger")) %>%
  GraphLearner$new(id="rf")

# We now have all the learners defined, so we can benchmark all of them
# simultaneously to get performance estimates. We now define training
# behaviors.
rsmp_spcv <- rsmp("repeated_spcv_coords", folds=5, repeats=10)
rsmp_stra <- rsmp("repeated_cv", folds=5, repeats=10) # resample using strata

# Model performance metrics
msr_maxdepth <- c(msr("regr.rmse"), msr("regr.rsq"), msr("regr.mae"), msr("regr.pbias"))

if (FIT_LMS) {
  # None of the linear models have hyperparameter tuning, so we can use
  # spatial CV with no inner resampling to derive performance estimates
  design_lm <- data.table::data.table(
    task       = list(task_maxdepth),
    learner    = list(lrn_hollister, lrn_heathcote, lrn_sobek, lrn_oliver),
    resampling = list(rsmp_spcv, rsmp_spcv, rsmp_spcv, rsmp_stra)
  )

  
  # Instantiate the resmaplings
  design_lm$resampling = Map(
    function(task, resampling) resampling$clone()$instantiate(task),
    task = design_lm$task, resampling = design_lm$resampling
  )
  
  # And run the benchmark
  bmr <- benchmark(design_lm)
  
  # Generate results as mean of all models
  bmr$aggregate(measures=msr_maxdepth) %>%
    select(-resample_result) %>%
    write_csv("data_out/model_results/maxdepth/maxdepth_lm_performance_metrics.csv")

  # Final train on all data and save the models
  lrn_oliver$train(task_maxdepth)
  lrn_heathcote$train(task_maxdepth)
  lrn_sobek$train(task_maxdepth)
  lrn_hollister$train(task_maxdepth)
  
  save(lrn_oliver, file="data_out/model_results/maxdepth/model_oliver")
  save(lrn_heathcote, file="data_out/model_results/maxdepth/model_heathcote")
  save(lrn_sobek, file="data_out/model_results/maxdepth/model_sobek")
  save(lrn_hollister, file="data_out/model_results/maxdepth/model_hollister")
} else {
  # All of these should be fitted already
  load("data_out/model_results/maxdepth/model_oliver")
  load("data_out/model_results/maxdepth/model_heathcote")
  load("data_out/model_results/maxdepth/model_sobek")
  load("data_out/model_results/maxdepth/model_hollister")
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
    task_maxdepth, at_rf, outer_resampling, store_models=TRUE
  )
  rr_khazaei <- resample(
    task_maxdepth, at_khazaei, outer_resampling, store_models=TRUE
  )
  
  # Save the resampling results - this is what we use to report model performance
  list(rf=rr_rf$aggregate(measure=msr_maxdepth),
       khazaei=rr_khazaei$aggregate(measure=msr_maxdepth)) %>%
    as.data.frame() %>% t() %>%
    as.data.frame() %>%
    mutate(learner=rownames(.)) %>%
    write_csv("data_out/model_results/maxdepth/maxdepth_rf_performance_metrics.csv")
  
  # And do the final tune without the outer loop so we have a usable model
  at_rf$train(task_maxdepth)
  at_khazaei$train(task_maxdepth)
  
  # save out the results
  save(at_rf, file="data_out/model_results/maxdepth/model_rf")
  save(at_khazaei, file="data_out/model_results/maxdepth/model_khazaei")
} else {
  load("data_out/model_results/maxdepth/model_rf")
  load("data_out/model_results/maxdepth/model_khazaei")
}

if (DO_PREDICTION) {
  # Generate new predictions
  obs_df <- read_csv("data_out/model_results/maxdepth/maxdepth_prediction_df.csv") %>%
    filter(area < 1e7) %>%
    drop_na()
  obs_df$prediction_heathcote <- lrn_heathcote$predict_newdata(obs_df)$response
  obs_df$prediction_hollister <- lrn_hollister$predict_newdata(obs_df)$response
  obs_df$prediction_khazaei   <- at_khazaei$predict_newdata(obs_df)$response
  obs_df$prediction_oliver    <- lrn_oliver$predict_newdata(obs_df)$response
  obs_df$prediction_sobek     <- lrn_sobek$predict_newdata(obs_df)$response
  obs_df$prediction_rf        <- at_rf$predict_newdata(obs_df)$response
  obs_df$in_training <- obs_df$lagoslakeid %in% model_df$lagoslakeid
  
  write_csv(obs_df, "data_out/model_results/maxdepth/maxdepth_prediction_results.csv")
} else {
  obs_df <- read_csv("data_out/model_results/maxdepth/maxdepth_prediction_results.csv")
}
