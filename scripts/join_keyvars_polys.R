library(tidyverse)
library(sf)
library(sp)
library(magrittr)
library(tidyjson)

keyvars <- read_csv("data_in/nla_2012_sites_sampled/nla12_keyvariables_data.txt")
keyvars_summary <- keyvars %>%
  group_by(SITE_ID) %>%
  summarize(
    index_depth_m = mean(INDEX_SITE_DEPTH),
    elev_m = first(ELEVATION),
    index_lat = first(INDEX_LAT_DD),
    index_lon = first(INDEX_LON_DD)
  ) %>%
  drop_na() %>%
  rename(NLA12_ID = SITE_ID)

lakepolys <- st_read("data_in/nla_2012_sites_sampled/NLA2012_Sampled_Polys_20131217.shp") %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1e6)

lakeslope <- read_csv("data_working/nla12_median_slope.csv") %>%
  rename(buffer_median_slope = median) %>%
  select(-.geo)

lakegrad <- read_csv("data_working/nla12_gradient.csv") %>%
  rename(buffer_min_elev=min, buffer_max_elev=max) %>%
  select(-.geo)

lakebbox <- st_read("data_working/nla2012_oriented_bbox.shp") %>%
  rename(bbox_width = width,
         bbox_height = height,
         bbox_area = area,
         bbox_angle = angle,
         bbox_perimeter = perimeter,
         NLA12_ID = SITE_ID) %>%
  select(-geometry)

lakegeo <- read_csv("data_working/nla2012_1000m_geo.csv") %>%
  select(-.geo, -`system:index`)

parse_geo_string <- function(str) {
  # parse the geology histogram
  # throw out beginning/ending brackets
  str <- substr(str, 2, str_length(str)-1)
  # split by comma
  str_vec <- str_split(str, ",")[[1]]
  ret_list <- list()
  for (item in str_vec) {
    # remove whitespace
    item <- str_trim(item)
    # find the equals sign
    eq_idx <- str_locate(item, "=")[1, 1]
    # pick out key/val
    key <- substr(item, 1, eq_idx-1)
    val <- as.numeric(substr(item, eq_idx+1, str_length(item)))
    # insert in list
    ret_list[[key]] <- val
  }
  return(ret_list)
}

# convert the EE dictionary string into a list-column
lakegeo$histogram <- map(lakegeo$histogram, parse_geo_string)

# spread the column into rows
lakegeo <- lakegeo %>%
  unnest(histogram) %>%
  mutate(value = as.numeric(flatten_chr(histogram)),
         key   = names(histogram)) %>%
  select(-histogram) %>%
  mutate(key = str_c("geo_type", key)) %>%
  spread(key, value, fill=0) %>%
  # remove a zero column
  select(-`<NA>`) %>%
  # normalize within each row
  mutate(row_sum = rowSums(select(., contains("geo_type"))),
         across(.cols=contains("geo_type"), ~ .x / row_sum)) %>%
  select(-row_sum)

lakes_join <- inner_join(keyvars_summary, lakepolys,
                         by="NLA12_ID") %>%
  inner_join(lakebbox, by="NLA12_ID") %>%
  inner_join(lakeslope, by="NLA12_ID") %>%
  inner_join(lakegrad, by="NLA12_ID") %>%
  inner_join(lakegeo, by="NLA12_ID") %>%
  select(NLA12_ID, contains("bbox"), contains("geo_type"), median_slope, 
         area_sqkm, index_depth_m, buffer_min_elev, buffer_max_elev, PERIM_KM_1) %>%
  rename(perimeter_km = PERIM_KM_1)

# Hypothesis: max slope w/in 100m of the lake explains depth moreso than area
ggplot(lakes_join, aes(x=median_slope, y=index_depth_m)) + geom_point()
ggplot(lakes_join, aes(x=area_sqkm, y=index_depth_m)) + geom_point() +
  scale_x_log10()
cor(log10(lakes_join$area_sqkm), lakes_join$index_depth_m)
cor(lakes_join$median_slope, lakes_join$index_depth_m)

# Calculate characteristic dimension from the bbox
# CD = min_bbox_dimension * (lake_area/bbox_area) * 0.5
lakes_join <- lakes_join %>% mutate(
  min_bbox_dim = pmin(bbox_width, bbox_height),
  bbox_area_sqkm = bbox_area/1e6,
  char_dim = min_bbox_dim * (area_sqkm / bbox_area_sqkm) * 0.5,
  pred_max_depth = sin(median_slope * 2*pi / 180) * char_dim
)

ggplot(lakes_join, aes(x=pred_max_depth, index_depth_m)) + geom_point()
cor(lakes_join$pred_max_depth, lakes_join$index_depth_m)

# Calculate gradient within the buffer zone around the lake
lakes_join <- lakes_join %>% mutate(
  grad_100m = buffer_max_elev - buffer_min_elev
)
ggplot(lakes_join, aes(x=grad_100m, y=index_depth_m)) + geom_point()
cor(lakes_join$index_depth_m, lakes_join$grad_100m)

# Calculate shoreline development index
lakes_join <- lakes_join %>% mutate(
  dev_index = perimeter_km / (2*sqrt(pi * area_sqkm))
)

# save the data as a regular CSV since we threw out the geometry already
lakes_join %>%
  write_csv("data_out/nla2012_lakes_join.csv")
