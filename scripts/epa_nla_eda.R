library(tidyverse)
library(sf)
library(sp)
library(magrittr)

keyvars <- read_csv("data_in/nla_2012_sites_sampled/nla12_keyvariables_data.txt")
keyvars_summary <- keyvars %>%
  group_by(SITE_ID) %>%
  summarize(
    index_depth_m = mean(INDEX_SITE_DEPTH),
    elev_m = first(ELEVATION),
    index_lat = first(INDEX_LAT_DD),
    index_lon = first(INDEX_LON_DD)
  ) %>%
  drop_na()

lakepolys <- st_read("data_in/nla_2012_sites_sampled/NLA2012_Sampled_Polys_20131217.shp") %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1e6)

lakes_join <- inner_join(keyvars_summary, lakepolys,
                         by=c("SITE_ID"="NLA12_ID"))

ggplot(lakes_join, aes(x=area_sqkm, y=index_depth_m)) + geom_point() +
  scale_x_log10()

lakes_join %>%
  filter(area_sqkm < 1e2) %$%
  cor(area_sqkm, index_depth_m)
