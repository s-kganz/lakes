library(tidyverse)
library(vroom)
library(pwiser)

source("scripts/util.R")

id_col <- "lagoslakei"

files <- list.files("data_in/gee/lagos_us_reflectance", "*.csv",
                    include.dirs=T, full.names=T)

gee_reflectance <- vroom(files)

gee_reflectance %>%
  select(-`system:index`, -hu8_zoneid, -.geo) %>%
  # log transform all five bands
  mutate(across(contains("B"), log)) %>%
  # compute all possible band ratios
  mutate(pairwise(contains("B"),
                  list(ratio="/"))) %>%
  select(-B1, -B2, -B3, -B4, -B5) %>%
  write_csv("data_out/lagos_us_reflectance.csv")
