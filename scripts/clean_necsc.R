library(tidyverse)

# pull in all the raw depth tables from the NECSC GitHub repo
# https://github.com/USGS-R/necsc-lake-modeling/tree/master/data/depth_data_raw

wi_swims <- read_tsv("data_in/necsc/wisconsin_swims_lakes_depth.txt") %>%
  select(`Official Max Depth`, Latitude, Longitude) %>%
  rename(
    latitude=Latitude,
    longitude=Longitude,
    maxdepth_m = `Official Max Depth`
  ) %>%
  # round to not introduce artifical precision
  mutate(maxdepth_m = round(maxdepth_m * 0.3048))

kevin    <- read_csv("data_in/necsc/kevinMichiganMaxDepth.csv") %>%
  select(Lat_Cent, Long_Cent, MaxDepth_f) %>%
  mutate(maxdepth_m = round(MaxDepth_f * 0.3048)) %>%
  rename(latitude=Lat_Cent,
         longitude=Long_Cent) %>%
  select(latitude, longitude, maxdepth_m)

mn_list  <- read_tsv("data_in/necsc/depth_mn_lakelist.txt") %>%
  select(LAKE_CENTER_LAT_DD5, LAKE_CENTER_LONG_DD5, MAX_DEPTH_FEET) %>%
  rename(latitude=LAKE_CENTER_LAT_DD5,
         longitude=LAKE_CENTER_LONG_DD5,
         maxdepth_ft = MAX_DEPTH_FEET) %>%
  mutate(maxdepth_m = maxdepth_ft * 0.3048) %>%
  select(-maxdepth_ft)

bathybase <- read_tsv("data_in/necsc/bathybase_depth.txt") %>%
  select(lat, lon, depth_max) %>%
  rename(maxdepth_m = depth_max,
         latitude=lat,
         longitude=lon) 

bind_rows(bathybase, kevin, mn_list, wi_swims) %>%
  drop_na() %>%
  filter(maxdepth_m > 0) %>%
  # everything needs the same precision
  mutate(maxdepth_m = round(maxdepth_m)) %>%
  write_csv("data_working/necsc_all_depths.csv")

# Do the spatial join in QGIS and export
read_csv("data_working/lagos_necsc_all_depths.csv") %>%
  select(lagoslakei, maxdepth_m) %>%
  group_by(lagoslakei) %>%
  mutate(depth_range = max(maxdepth_m) - min(maxdepth_m),
         n= n()) %>%
  filter(depth_range == 0) %>%
  summarize(maxdepth_m = first(maxdepth_m)) %>%
  select(lagoslakei, maxdepth_m) %>%
  write_csv("data_working/lagos_necsc_grouped_depths.csv")
