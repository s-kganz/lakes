# GEE generates a very long (in the dplyr sense) data file with all the terrain
# attributes. We would like to make it wider for use in a RF model

library(tidyverse)

ID_COL <- "lagoslakei"

gee_terrain <- read_csv("data_in/gee/lagos_ne_terrain_metrics.csv") %>%
  select(-c(`system:index`, .geo, random)) %>%
  rename(metric=stat) %>%
  pivot_longer(c(min, max, median), names_to="stat") %>%
  mutate(colname = str_c(metric, "_", stat)) %>%
  select(-c(metric, stat)) %>%
  # if there are duplicate polygons, their metrics should be the same
  # so, just take the first from eahc column
  pivot_wider(id_cols=c(ID_COL, colname), names_from=colname, values_from=value,
              values_fn = first) %>%
  select(-`NA`) %>%
  # throw out the one NA row at the bottom
  filter(!is.na(ID_COL))

gee_terrain %>%
  write_csv("data_out/lagos_ne_terrain.csv")
