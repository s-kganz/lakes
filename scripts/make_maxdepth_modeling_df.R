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
      "data_out/lagos_us_geography.csv", # missing 1985 rows
      col_types=cols(lake_huc12=col_character())),# %>%
    #rename(lagoslakei = lagoslakeid),
    by=join_column
  ) %>%
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
  ) %>%
  # add in terms needed for other literature models
  mutate(
    # sobek
    logarea = log10(area),
    # heathcote
    log_elev_change = log(pmax(elev_median - elev_min, 0.01))
  ) %>%
  # drop duplicate columns
  select(-contains("Elevation", ignore.case=F))

# add the messager volume and mean depth estimates
messager_coeffs <- read_csv("data_working/messager_volume_coefficients.csv")
obs_df <- obs_df %>%
  mutate(logarea_km2 = log10(area/1e6),
         size_class = ifelse(logarea_km2 < 1, 1,
                            ifelse(logarea_km2 < 10, 2,
                                   ifelse(logarea_km2 < 100, 3, 4)))) %>%
  inner_join(messager_coeffs, by="size_class") %>%
  mutate(
    messager_zmean = 10^(int + loga * log10(area) + logs * log10(slope_median)),
    messager_volume = area * messager_zmean
  ) %>%
  # discard intermediates
  select(-logarea_km2, -size_class, -int, -loga, -logs, -amin_km2, -amax_km2)

# remove NAs by imputing any missing values - only needed for ~2% of data
colSums(is.na(obs_df))
obs_df <- obs_df %>%
  mutate(across(where(is.numeric),
                function(x) {
                  ifelse(is.na(x), median(x, na.rm=T), x)
                }))
colSums(is.na(obs_df))

model_df <- obs_df %>% inner_join(
  # add in the depth data
  read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
    select(lagoslakeid, lake_maxdepth_m) %>%
    rename(maxdepth=lake_maxdepth_m),
  by=join_column
) %>%
  # compute the modeling gruops for the mixed effects model down the line
  group_by(lake_huc04) %>%
  mutate(
    oliver_model_group = ifelse(n() < 10, "default", lake_huc04)
  ) %>%
  # apply constraints on area and maxdepth
  filter(area < 1e7, maxdepth > 0.5) %>%
  write_csv("data_out/model_results/maxdepth/maxdepth_modeling_df.csv")

obs_df %>%
  mutate(oliver_model_group = ifelse(lake_huc04 %in% model_df$oliver_model_group,
                                     lake_huc04, "default")) %>%
  write_csv("data_out/model_results/maxdepth/maxdepth_prediction_df.csv")
