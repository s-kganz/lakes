library(tidyverse)

source("scripts/util.R")

join_column <- "lagoslakeid"
obs_df <- read_csv("data_out/lagos_us_shape.csv") %>%
  rename(lagoslakeid=lagoslakei) %>%
  inner_join(
    read_csv("data_out/lagos_us_terrain_fabdem_100m.csv") %>%
      rename(lagoslakeid=lagoslakei),
    by=join_column
  ) %>%
  inner_join(
    read_csv(
      "data_out/lagos_us_geography.csv",
      col_types=cols(lake_huc12=col_character())),# %>%
    #rename(lagoslakei = lagoslakeid),
    by=join_column
  ) %>%
  # Swap this out with your depth dataset of choice
  inner_join(
    read_csv("data_out/lagos_us_temperature.csv"),
    by=join_column
  ) %>%
  inner_join(
    read_csv("data_out/lagos_us_reflectance.csv") %>%
      rename(lagoslakeid=lagoslakei),
    by=join_column
  ) %>%
  mutate(
    lake_huc12_pad = map_chr(lake_huc12, parse_huc),
    lake_huc02 = str_sub(lake_huc12_pad, 1, 2),
    lake_huc04 = str_sub(lake_huc12_pad, 1, 4),
    lake_huc06 = str_sub(lake_huc12_pad, 1, 6),
    #net_id = factor(net_id)
  ) %>%
  # join regional stats by huc04 code
  dynamic_join("data_out/lagos_us_huc_terrain_fabdem_100m.csv", yjoin="huc04") %>%
  dynamic_join("data_out/huc_depth.csv", yjoin="huc04") %>%
  dynamic_join("data_out/huc_rd.csv", yjoin="huc04") %>%
  dynamic_join("data_out/lagos_us_huc_shape.csv", yjoin="huc04") %>%
  select(
    -lake_huc12_pad, -contains("dam"), -OBJECTID
  ) %>%
  # add in linear and quad terms from the cone model
  mutate(
    linear_term = dist_pole * slope_median,
    quad_term   = dist_pole^2 * VerticalCurvature_max
  )

model_df <- obs_df %>% inner_join(
  # add in the depth data
  read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
    select(lagoslakeid, lake_maxdepth_m) %>%
    rename(maxdepth=lake_maxdepth_m),
  by=join_column
) %>%
  # apply constraints on area and maxdepth
  filter(area < 1e7, maxdepth > 0.5) %>%
  write_csv("data_out/model_results/maxdepth/maxdepth_modeling_df.csv")

obs_df %>% write_csv("data_out/model_results/maxdepth/maxdepth_prediction_df.csv")
