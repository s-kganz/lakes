library(tidyverse)
library(stringr)
library(sf)

path_to_files <- "data_in/gee/hollister_tagee"
gee_to_process <- list.files(
  path_to_files,
  include.dirs = TRUE,
  pattern="*.csv")

process_gee_file <- function(path) {
  # trim "hollister_" and ".csv" to get the column prefix
  column_prefix <- str_remove(path, "hollister_") %>% str_remove(".csv")
  df <- read_csv(file.path(path_to_files, path)) %>%
    select(-.geo, -`system:index`) %>%
  # rename all the non-ID columns with the prefix
    rename_with(
      function(names) {str_c(column_prefix, names, sep="_")}, 
      -c("NLAID")) %>%
    return()
}

df <- NULL

for (path in gee_to_process) {
  new_df <- process_gee_file(path)
  if (is.null(df)) {
    df <- new_df
  }
  else {
    df <- inner_join(df, new_df, by="NLAID")
  }
}

# all the terrrain metrics are joined, now add in maxdepth from the previous dataframe
nla_depth <- read_csv("data_out/nla_terrain_gee.csv") %>%
  select(NLAID, maxdepth, dist_pole)

nla_hollister_df <- df %>%
  inner_join(nla_depth, by="NLAID")

write_csv(nla_hollister_df, "data_out/nla_hollister_gee.csv")
