library(tidyverse)

# The full predictor/prediction dataframes are kept here for posterity's sake, 
# but for making figures etc. we only really care about area, true depth,
# and modeled depth. This file takes the modeling results and combines everything
# together.
depths <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv")
zmax   <- read_csv("data_out/model_results/maxdepth/maxdepth_prediction_results.csv")
zmean  <- read_csv("data_out/model_results/meandepth/meandepth_prediction_results.csv")

model_comp <- zmax %>%
  select(lagoslakeid, in_training, area, contains("prediction")) %>%
  rename_with(~ str_c("maxdepth_", .x), .cols=contains("prediction")) %>%
  rename(in_maxdepth_training=in_training) %>%
  inner_join(
    zmean %>%
      select(lagoslakeid, in_training, contains("prediction")) %>%
      rename_with(~ str_c("meandepth_", .x), .cols=contains("prediction")) %>%
      rename(in_meandepth_training=in_training),
    by="lagoslakeid"
  ) %>%
  left_join(
    depths %>% select(lagoslakeid, lake_meandepth_m, lake_maxdepth_m),
    by="lagoslakeid"
  ) %>%
  # pick out the best max/mean depth depending on if an observed one is available
  mutate(
    best_maxdepth  = ifelse(is.na(lake_maxdepth_m), maxdepth_prediction_rf,
                            lake_maxdepth_m),
    best_meandepth = ifelse(is.na(lake_meandepth_m), meandepth_prediction_rf,
                            lake_meandepth_m)
  ) %>%
  write_csv("data_out/model_results/compiled_predictions.csv")