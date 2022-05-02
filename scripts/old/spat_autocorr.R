# Oliver et al. chose HUC4 units for their mixed model because it offered
# a balance of spatial autocorrelation and sufficient observations for each unit.
# However, they didn't report anything quantitative. The goal of this script
# is to assess spatial autocorrelation at different HUC scales and at different
# length scales.
library(tidyverse)
library(corrplot)
source("scripts/util.R")

calculate_neighborhood_mean <- function(df, scale) {
  group_var <- str_c("lake_huc", scale)
  
  df %>%
    group_by(.data[[group_var]]) %>%
    mutate(
      "huc{scale}_n" := n(),
      # Subtracting (this depth / n) makes it so the mean excludes the
      # individual, so we don't detect correlation between identical
      # observations.
      "huc{scale}_mean" := mean(lake_maxdepth_m) - (lake_maxdepth_m / n())   
    )
}

# First, calc Moran's I at all the HUC scales
depths <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
  select(lagoslakeid, lake_maxdepth_m)
info   <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_information.csv") %>%
  select(lagoslakeid, lake_lon_decdeg, lake_lat_decdeg, lake_huc12) %>%
  mutate(lake_huc12 = map_chr(lake_huc12, parse_huc))

model_df <- depths %>%
  inner_join(info, by="lagoslakeid") %>%
  # calculate all the HUC zones
  mutate(
    lake_huc2  = map_chr(lake_huc12, function(x) str_sub(x, 1, 2)),
    lake_huc4  = map_chr(lake_huc12, function(x) str_sub(x, 1, 4)),
    lake_huc6  = map_chr(lake_huc12, function(x) str_sub(x, 1, 6)),
    lake_huc8  = map_chr(lake_huc12, function(x) str_sub(x, 1, 8)),
    lake_huc10 = map_chr(lake_huc12, function(x) str_sub(x, 1, 10))
  )

for (scale in seq(2, 12, 2)) {
  model_df <- calculate_neighborhood_mean(model_df, scale)
}

# How many lakes per group at each scale?
model_df %>%
  ungroup() %>%
  select(contains("_n")) %>%
  pivot_longer(contains("_n"), names_to="scale", values_to="n") %>%
  #filter(scale == "huc10_n") %>%
  ggplot(aes(x=n)) + geom_histogram() + facet_wrap(~ scale)

# What is the correlation between an observation and its neighborhood mean?
corr_mtx <- model_df %>%
  ungroup() %>%
  select(lake_maxdepth_m, contains("_mean")) %>%
  cor()

corr_mtx

# Correlation is pretty garbage at all scales
# Log transform to see the point cloud better
model_df %>%
  ungroup() %>%
  select(lake_maxdepth_m, contains("_mean")) %>%
  mutate(across(everything(), log)) %>%
  pivot_longer(contains("_mean"), names_to="scale", values_to="neighborhood") %>%
  ggplot(aes(x=lake_maxdepth_m, y=neighborhood)) + geom_point() +
  facet_wrap(~ scale)
  
  