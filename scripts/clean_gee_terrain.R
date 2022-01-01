# GEE generates a very long (in the dplyr sense) data file with all the terrain
# attributes. We would like to make it wider for use in a RF model

library(tidyverse)
library(vroom)

source("scripts/util.R")

ID_COL <- "lagoslakei"

# If the terrain is all in one file, just read it in
#gee_terrain <- read_csv("data_in/gee/lagos_ne_terrain_metrics.csv") %>%

# Otherwise, use vroom to concat many files together
files <- list.files("data_in/gee/lagosus_by_huc8/lagos_us_by_huc8/", "*.csv",
                    include.dirs=T, full.names=T)

gee_terrain <- vroom(files)

cleaned <- gee_terrain %>%
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

cleaned %>%
  write_csv("data_out/lagos_us_terrain.csv")
