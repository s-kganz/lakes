library(tidyverse)
library(vroom)

source("scripts/util.R")

files <- list.files("data_in/gee/lagos_us_polygon_temperature_anomaly/", "*.csv",
                    include.dirs=T, full.names=T)

# Sometimes we get unlucky and every point in a file has no imagery. In that
# case, map the vroom call to concat everything and fill with NAs.
gee_temp    <- map_df(files, ~vroom(.x, progress=FALSE, show_col_types=FALSE))

gee_wide <- gee_temp %>%
  select(lagoslakei, median, month) %>%
  rename(lagoslakeid=lagoslakei,
         anomaly=median) %>%
  mutate(month = str_c("t_anomaly_month_", month)) %>%
  pivot_wider(names_from=month, values_from=anomaly) %>%
  rename_with(~str_c("polygon_", .), everything()) %>%
  write_csv("data_out/lagos_us_polygon_temperature.csv")
