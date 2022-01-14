# NLA maxdepths come spread across 3 files with very different formats. This
# cleans everything up and dumps it into one file.
library(tidyverse)

nla07 <- read_csv(
  "data_in/nla/nla2007/nla2007_sampledlakeinformation_20091113.csv"
) %>%
  select(SITE_ID, DEPTHMAX, LON_DD, LAT_DD) %>%
  mutate(YEAR=2007)

nla12 <- read_csv(
  "data_in/nla/nla2012/nla12_keyvariables_data.txt"
) %>%
  select(SITE_ID, INDEX_LAT_DD, INDEX_LON_DD, INDEX_SITE_DEPTH) %>%
  rename(LAT_DD = INDEX_LAT_DD,
         LON_DD = INDEX_LON_DD,
         DEPTHMAX = INDEX_SITE_DEPTH) %>%
  mutate(YEAR=2012)

nla17 <- read_csv(
  "data_in/nla/nla2017/nla_2017_profile-data.csv"
) %>%
  filter(!is.na(INDEX_SITE_DEPTH)) %>%
  filter(DEPTH_METHOD != "Estimate") %>%
  select(INDEX_SITE_DEPTH, INDEX_LAT_DD, INDEX_LON_DD, SITE_ID) %>%
  rename(DEPTHMAX=INDEX_SITE_DEPTH,
         LAT_DD = INDEX_LAT_DD,
         LON_DD = INDEX_LON_DD) %>%
  mutate(LON_DD = parse_double(LON_DD)) %>%
  mutate(YEAR=2017)

nla07 %>%
  bind_rows(nla12) %>%
  bind_rows(nla17) %>%
  write_csv("data_working/nla/nla_combined_depths.csv")
