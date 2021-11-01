# GEE generates a very long (in the dplyr sense) data file with all the terrain
# attributes. We would like to make it wider for use in a RF model

library(tidyverse)

gee_terrain <- read_csv("data_in/gee/nla_combined_all_terrain_metrics.csv") %>%
  select(-c(`system:index`, .geo)) %>%
  rename(metric=stat) %>%
  pivot_longer(c(min, max, median), names_to="stat") %>%
  mutate(colname = str_c(metric, "_", stat)) %>%
  select(-c(metric, stat)) %>%
  pivot_wider(id_cols=c(NLAID, colname), names_from=colname, values_from=value) %>%
  select(-`NA`) %>%
  # throw out the one NA row at the bottom
  filter(!is.na(NLAID)) %>%
  # make everything not a list column
  mutate(across(.cols=-NLAID, unlist)) %>%
  write_csv("data_out/nla_combined_terrain.csv")
