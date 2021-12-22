# There is probably some degree of spatial autocorrelation in lake depth, compute
# regional stats at the HUC4 scale to include this in the model
library(tidyverse)
source("scripts/util.R")

# apply the typical cleaning procedure to huc4 terrain
terrain <- read_csv("data_in/gee/huc4_terrain.csv") %>%
  select(huc4, max, median, min, stat) %>%
  replace_na(list(
    max=0, median=0, min=0
  )) %>%
  rename(variable=stat) %>%
  pivot_longer(c(max, min, median), names_to="stat") %>%
  mutate(colname = str_c(variable, "_", stat)) %>%
  select(-variable, -stat) %>%
  pivot_wider(names_from=colname, values_from=value)

terrain %>% write_csv('data_out/huc4_terrain.csv')

# Make a crosswalk table
lagos_huc_xwalk <- read_csv("data_out/lagos_us_geography.csv") %>%
  select(lagoslakeid, lake_huc12) %>%
  mutate(lake_huc12 = map_chr(lake_huc12, parse_huc),
         huc02 = str_sub(lake_huc12, 1, 2),
         huc04 = str_sub(lake_huc12, 1, 4),
         huc06 = str_sub(lake_huc12, 1, 6),
         huc08 = str_sub(lake_huc12, 1, 8),
         huc10 = str_sub(lake_huc12, 1, 10))

lagos_huc_xwalk %>% write_csv("data_out/lagos_huc_crosswalk.csv")

# Take the mean of the shape metrics
huc_shape <- read_csv("data_out/lagos_us_shape.csv") %>%
  inner_join(
    lagos_huc_xwalk, by=c("lagoslakei"="lagoslakeid")
  ) %>%
  select(huc04, perimeter, area, bbox_lw, sdi, dist_pole) %>%
  group_by(huc04) %>%
  summarize(
    across(.cols=c(perimeter, area, bbox_lw, sdi, dist_pole),
           .fns =list("min"=min, "max"=max, "median"=median, "sd"=sd),
           .names="{.col}_{.fn}")
  )

huc_shape %>% write_csv("data_out/lagos_us_huc_shape.csv")

# Min, max, mean lake depth
huc_depth <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
  inner_join(lagos_huc_xwalk, by="lagoslakeid") %>%
  select(huc04, lake_maxdepth_m) %>%
  group_by(huc04) %>%
  summarize(min_depth = min(lake_maxdepth_m),
            max_depth = max(lake_maxdepth_m),
            median_depth= median(lake_maxdepth_m))

huc_depth %>% write_csv("data_out/huc_depth.csv")

# Min, max, mean relative depth
# RD = (depth * sqrt(pi)) / (20 * sqrt(area))
huc_rd <- huc_depth <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
  inner_join(lagos_huc_xwalk, by="lagoslakeid") %>%
  select(huc04, lake_waterarea_ha, lake_maxdepth_m) %>%
  mutate(lake_waterarea_m2 = lake_waterarea_ha * 1e4,
         rd = (lake_maxdepth_m * sqrt(3.14159)) / (20 * sqrt(lake_waterarea_m2))) %>%
  group_by(huc04) %>%
  summarize(min_rd  = min(rd),
            max_rd  = max(rd),
            median_rd = median(rd))

huc_rd %>% write_csv("data_out/huc_rd.csv")
