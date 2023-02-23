library(tidyverse)

# The full predictor/prediction dataframes are kept here for posterity's sake, 
# but for making figures etc. we only really care about area, true depth,
# and modeled depth. This file takes the modeling results and combines everything
# together.
depths <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv")
zmax   <- read_csv("data_out/model_results/maxdepth_lt1000ha/maxdepth_prediction_results.csv")
zmean  <- read_csv("data_out/model_results/meandepth_lt1000ha/meandepth_prediction_results.csv")

model_comp <- zmax %>%
  select(lagoslakeid, in_training, contains("prediction")) %>%
  rename_with(~ str_c("maxdepth_m_", .x), .cols=contains("prediction")) %>%
  rename(in_maxdepth_training=in_training) %>%
  inner_join(
    zmean %>%
      select(lagoslakeid, in_training, contains("prediction")) %>%
      rename_with(~ str_c("meandepth_m_", .x), .cols=contains("prediction")) %>%
      rename(in_meandepth_training=in_training),
    by="lagoslakeid"
  ) %>%
  full_join(
    depths %>% select(lagoslakeid, lake_meandepth_m, lake_maxdepth_m),
    by="lagoslakeid"
  ) %>%
  # add in area
  left_join(
    read_csv("data_out/lagos_us_shape.csv") %>% select(lagoslakei, area),
    by=c("lagoslakeid"="lagoslakei")
  ) %>%
  # pick out the best max/mean depth depending on if an observed one is available
  mutate(
    best_maxdepth  = ifelse(is.na(lake_maxdepth_m), maxdepth_m_prediction_rf,
                            lake_maxdepth_m),
    best_meandepth = ifelse(is.na(lake_meandepth_m), meandepth_m_prediction_rf,
                            lake_meandepth_m),
    cone_volume = best_maxdepth * area / 3
  ) %>%
  write_csv("data_out/model_results/lt1000ha_compiled_predictions.csv")
