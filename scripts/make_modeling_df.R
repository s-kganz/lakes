library(tidyverse)

source("scripts/util.R")

join_column <- "lagoslakeid"
obs_df <- read_csv("data_out/lagos_us_shape.csv") %>%
  rename(lagoslakeid=lagoslakei) %>%
  inner_join(
    read_csv("data_out/lagos_us_terrain.csv") %>%
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
    read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
      select(lagoslakeid, lake_maxdepth_m) %>%
      rename(maxdepth=lake_maxdepth_m),
    by=join_column
  ) %>%
  select(
    -contains("lagoslakei"), -contains("dam"), -OBJECTID 
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
  ) %>% drop_na() %>% select(-lake_huc12_pad)

# join regional stats by huc04 code
model_df <- obs_df %>%
  dynamic_join("data_out/huc4_terrain.csv") %>%
  dynamic_join("data_out/huc_depth.csv", yjoin="huc04") %>%
  dynamic_join("data_out/huc_rd.csv", yjoin="huc04") %>%
  dynamic_join("data_out/lagos_us_huc_shape.csv", yjoin="huc04")

# add in the linear and quad terms bc they seem important
model_df$linear_term <- model_df$dist_pole   * model_df$slope_median
model_df$quad_term   <- model_df$dist_pole^2 * model_df$vcurv_max

write_csv(model_df, "data_working/lagosus/lagos_us_model_df.csv")
