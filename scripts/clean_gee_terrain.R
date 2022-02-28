# GEE generates a very long (in the dplyr sense) data file with all the terrain
# attributes. We would like to make it wider for use in a RF model

library(tidyverse)
library(vroom)

source("scripts/util.R")

ID_COL <- "lagoslakei"

# If the terrain is all in one file, just read it in
#gee_terrain <- read_csv("data_in/gee/lagos_ne_terrain_metrics.csv") %>%

# Otherwise, use vroom to concat many files together
files <- list.files("data_in/gee/lagos_us_terrain_fabdem_100m_by_huc8/", "*.csv",
                    include.dirs=T, full.names=T)

gee_terrain <- vroom(files)

# get curvature as well
files <- list.files("data_in/gee/lagos_us_curvature_fabdem_100m_strips_by_huc8/", "*.csv",
                    include.dirs=T, full.names=T)

gee_curvature <- vroom(files)

# If the files do not all have the same columns
# gee_temp    <- map_df(files, ~vroom(.x, progress=FALSE, show_col_types=FALSE))

terrain_cleaned <- gee_terrain %>%
  select(-c(`system:index`, .geo, hu8_zoneid)) %>%
  rename(metric=stat) %>%
  pivot_longer(c(min, max, median, stdDev), names_to="stat") %>%
  mutate(colname = str_c(metric, "_", stat)) %>%
  select(-c(metric, stat)) %>%
  # if there are duplicate polygons, their metrics should be the same
  # so, just take the first from each column
  pivot_wider(id_cols=c(ID_COL, colname), names_from=colname, values_from=value,
              values_fn = first) %>%
  #select(-`NA`) %>%
  # throw out the one NA row at the bottom
  filter(!is.na(ID_COL))

gee_join <- terrain_cleaned %>%
  inner_join(
    gee_curvature %>%
      select(-contains("Slope"), -`system:index`, -hu8_zoneid, -.geo),
    by="lagoslakei"
    )

gee_join %>%
  write_csv("data_out/lagos_us_terrain_fabdem_100m.csv")
