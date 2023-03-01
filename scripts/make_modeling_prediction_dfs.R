library(tidyverse)

source("scripts/util.R")

# Prepare are the dataframes to be joined together
shape   <- read_csv("data_out/lagos_us_shape.csv") %>% select(-OBJECTID) %>%
  rename(lagoslakeid=lagoslakei)

terrain <- read_csv("data_out/lagos_us_terrain_fabdem_100m.csv") %>%
  rename(lagoslakeid=lagoslakei) %>%
  # Drop duplicate elevation data
  select(-contains("Elevation", ignore.case=FALSE))

geo     <- read_csv(
  "data_out/lagos_us_geography.csv",
  col_types=cols(lake_huc12=col_character())
  ) %>%
  # Compute HUC substrings
  mutate(
    lake_huc12_pad = map_chr(lake_huc12, parse_huc),
    lake_huc02 = str_sub(lake_huc12_pad, 1, 2),
    lake_huc04 = str_sub(lake_huc12_pad, 1, 4),
    lake_huc06 = str_sub(lake_huc12_pad, 1, 6),
    #net_id = factor(net_id)
  ) %>% select(-lake_huc12_pad)

# Mean depth uses polygon reflectance/temperature, max depth uses point
# reflectance/temperature.
temp_poly <- read_csv("data_out/lagos_us_polygon_temperature.csv") %>%
  rename(lagoslakeid="polygon_lagoslakeid")

ref_poly <- read_csv("data_out/lagos_us_polygon_reflectance.csv") %>%
  rename(lagoslakeid="polygon_lagoslakei")

temp_point <- read_csv("data_out/lagos_us_temperature.csv")
ref_point  <- read_csv("data_out/lagos_us_reflectance.csv") %>%
  rename(lagoslakeid="lagoslakei")

# Predictor variables by HUC4 unit
huc_terrain <- read_csv("data_out/lagos_us_huc_terrain_fabdem_100m.csv")
huc_depth   <- read_csv("data_out/huc_depth.csv")
huc_rd      <- read_csv("data_out/huc_rd.csv")
huc_shape   <- read_csv("data_out/lagos_us_huc_shape.csv")

# Coefficients to replicate the model from Messager et al. (2016)
messager_coeffs <- read_csv("data_out/messager_volume_coefficients.csv")

# Finally, the depths we want to predict
depths <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
  select(lagoslakeid, lake_maxdepth_m, lake_meandepth_m)

all_predictors <- shape %>%
  inner_join(geo, by="lagoslakeid") %>%
  inner_join(terrain, by="lagoslakeid") %>%
  inner_join(temp_poly, by="lagoslakeid") %>%
  inner_join(ref_poly, by="lagoslakeid") %>%
  inner_join(temp_point, by="lagoslakeid") %>%
  inner_join(ref_point, by="lagoslakeid") %>%
  inner_join(huc_terrain, by=c("lake_huc04"="huc04")) %>%
  inner_join(huc_depth, by=c("lake_huc04"="huc04")) %>%
  inner_join(huc_rd, by=c("lake_huc04"="huc04")) %>%
  inner_join(huc_shape, by=c("lake_huc04"="huc04")) %>%
  # Temporary join vars for the messager coefficients
  mutate(logarea_km2 = log10(area/1e6),
         size_class = ifelse(logarea_km2 < 1, 1,
                             ifelse(logarea_km2 < 10, 2,
                                    ifelse(logarea_km2 < 100, 3, 4)))) %>%
  # Add in the coefficients
  inner_join(messager_coeffs, by="size_class") %>%
  # Compute the estimated zmean and lake volume from the coefficients
  mutate(
    messager_zmean = 10^(int + loga * log10(area) + logs * log10(slope_median)),
    messager_volume = area * messager_zmean
  ) %>%
  # Drop the coefficients and intermediate variables
  select(-logarea_km2, -size_class, -int, -loga, -logs, -amin_km2, -amax_km2) %>%
  # Add in terms used in other literature models. We don't compute the modeling
  # groups for the mixed effects model in Oliver et al. (2016) here because
  # later filtering options may break the n > 10 requirement in their paper.
  mutate(
    # For Sobek
    logarea = log10(area),
    # For Heathcote (it's possible that elev_median == elev_min which would
    # give -Inf, so we use pmax() to force -1 in that case.
    log_elev_change = log(pmax(elev_median - elev_min, exp(-1))),
    # Cone model term + an extension (see Eq. 4 in our paper)
    linear_term = dist_pole * tan(slope_median * pi / 180),
    quad_term   = dist_pole^2 * VerticalCurvature_max
  ) %>%
  # Use median imputation to remove NAs from numeric variables
  # remove NAs by imputing any missing values - only needed for ~2% of data
  # at worst
  mutate(across(where(is.numeric),
                function(x) {
                  ifelse(is.na(x), median(x, na.rm=T), x)
                }))

# Verify that there are no NAs
stopifnot(all(colSums(is.na(all_predictors)) == 0))

get_oliver_model_groups <- function(df) {
  df %>%
    group_by(lake_huc04) %>%
    mutate(oliver_model_group = ifelse(n() < 10, "default", lake_huc04)) %>%
    ungroup() %>%
    return()
}

# We are generating 4 prediction tasks in a 2x2 cross of (maxdepth/meandepth) x
# (alldata/lakes < 1000 ha). In each case, compute the Oliver *for the modeling
# dataframe* right before saving.

predictors_depth <- all_predictors %>%
  left_join(depths, by='lagoslakeid') %>%
  rename(maxdepth=lake_maxdepth_m,
         meandepth=lake_meandepth_m)

# Maxdepth case
model_df_maxdepth_alldata <- predictors_depth %>%
  select(-meandepth, -starts_with("polygon")) %>%
  filter(!is.na(maxdepth)) %>%
  get_oliver_model_groups() %>%
  write_csv("data_out/model_results/maxdepth_alldata/maxdepth_modeling_df.csv")

model_df_maxdepth_lt1000ha <- model_df_maxdepth_alldata %>%
  filter(area <= 1e7) %>%
  get_oliver_model_groups() %>%
  write_csv("data_out/model_results/maxdepth_lt1000ha/maxdepth_modeling_df.csv")

# Meandepth case
model_df_meandepth_alldata <- predictors_depth %>%
  select(-maxdepth, -matches("^t_anomaly"), -matches("^B", ignore.case=FALSE)) %>%
  filter(!is.na(meandepth)) %>%
  get_oliver_model_groups() %>%
  write_csv("data_out/model_results/meandepth_alldata/meandepth_modeling_df.csv")

model_df_meandepth_lt1000ha <- model_df_meandepth_alldata %>%
  filter(area <= 1e7) %>%
  get_oliver_model_groups() %>%
  write_csv("data_out/model_results/meandepth_lt1000ha/meandepth_modeling_df.csv")

# Now generate the prediction DFs that the models will be applied to. Select
# the same predictors and apply the area constraint if needed. Note that we 
# compute the Oliver model groups as in the relevant modeling DF.
pred_df_maxdepth_alldata <- predictors_depth %>%
  select(-maxdepth, -meandepth, -starts_with("polygon")) %>%
  mutate(oliver_model_group = ifelse(
      lake_huc04 %in% model_df_maxdepth_alldata$oliver_model_group, 
      lake_huc04, 
      "default"
    )
  ) %>%
  write_csv("data_out/model_results/maxdepth_alldata/maxdepth_prediction_df.csv")

pred_df_maxdepth_lt1000ha <- pred_df_maxdepth_alldata %>%
  filter(area <= 1e7) %>%
  mutate(oliver_model_group = ifelse(
      lake_huc04 %in% model_df_maxdepth_lt1000ha$oliver_model_group, 
      lake_huc04, 
      "default"
    )
  ) %>%
  write_csv("data_out/model_results/maxdepth_lt1000ha/maxdepth_prediction_df.csv")

pred_df_meandepth_alldata <- predictors_depth %>%
  select(-maxdepth, -meandepth, 
         -matches("^t_anomaly"), -matches("^B", ignore.case=FALSE)) %>%
  mutate(oliver_model_group = ifelse(
      lake_huc04 %in% model_df_meandepth_alldata$oliver_model_group, 
      lake_huc04, 
      "default"
    )
  ) %>%
  write_csv("data_out/model_results/meandepth_alldata/meandepth_prediction_df.csv")

pred_df_meandepth_lt1000ha <- pred_df_meandepth_alldata %>%
  filter(area <= 1e7) %>%
  mutate(oliver_model_group = ifelse(
      lake_huc04 %in% model_df_meandepth_lt1000ha$oliver_model_group, 
      lake_huc04, 
      "default"
    )
  ) %>%
  write_csv("data_out/model_results/meandepth_lt1000ha/meandepth_prediction_df.csv")

# Merge the predictors from max/meandepth into one file to upload to EDI.
unique_names <- setdiff(
  names(pred_df_meandepth_alldata), 
  names(pred_df_maxdepth_alldata)
)

all_predictors <- pred_df_maxdepth_alldata %>%
  full_join(
    pred_df_meandepth_alldata %>% 
      select(all_of(c("lagoslakeid", unique_names))),
    by="lagoslakeid"
  ) %>%
  write_csv("data_out/model_results/all_lake_predictors.csv")
