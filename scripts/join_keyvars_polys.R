library(tidyverse)
library(sf)
library(sp)
library(lwgeom)
library(magrittr)
library(tidyjson)

# depth information for NLA12 lakes
nla12_keyvars <- read_csv("data_in/nla/nla2012/nla12_keyvariables_data.txt")
nla12_keyvars_summary <- nla12_keyvars %>%
  group_by(SITE_ID) %>%
  summarize(
    maxdepth = mean(INDEX_SITE_DEPTH),
    elev_m = first(ELEVATION),
    index_lat = first(INDEX_LAT_DD),
    index_lon = first(INDEX_LON_DD)
  ) %>%
  drop_na() %>%
  rename(NLAID = SITE_ID)

# depth information for NLA07 lakes
nla07_depth <- read_csv("data_in/nla/nla2007/nla2007_sampledlakeinformation_20091113.csv") %>%
  select(SITE_ID, DEPTHMAX, LAT_DD, LON_DD, ELEV_PT) %>%
  rename(maxdepth = DEPTHMAX,
         NLAID = SITE_ID,
         index_lat = LAT_DD,
         index_lon = LON_DD,
         elev_m = ELEV_PT) %>%
  group_by(NLAID) %>%
  summarize_all(mean)

# merge the depths
nla_depth <- bind_rows(nla12_keyvars_summary, nla07_depth) %>%
  drop_na()

process_gee_csv <- function(path) {
  csv <- read_csv(path, col_types = cols(NLA12_ID=col_character(), SITEID=col_character()))
  label <- str_c(csv$stat[1], csv$length[1], "_")
  csv %>%
    rename_with(~str_c(label, .), .cols=c(max, median, min, contains("p"))) %>%
    select(-`system:index`, -NLA12_ID, -SITEID, -.geo, -length, -stat) %>%
    return()
}

gee_csvs <- list.files(path="data_in/gee/", full.names=T)
gee_csvs <- gee_csvs[!str_detect(gee_csvs, "geo")]

gee_join <- NULL
for (gee in gee_csvs) {
  new_csv <- process_gee_csv(gee)
  if (is.null(gee_join)) { gee_join <- new_csv }
  else {gee_join <- inner_join(gee_join, new_csv, by="NLAID")}
}

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

lakegeo <- read_csv("data_in/gee/geo1000.csv",
                    col_types = cols(NLA12_ID=col_character())) %>%
  select(NLAID, histogram)

# convert the EE dictionary string into a list-column
lakegeo$histogram_parsed <- map(lakegeo$histogram, parse_geo_string)

# spread the column into rows
lakegeo <- lakegeo %>%
  unnest(histogram_parsed) %>%
  mutate(value = as.numeric(flatten_chr(histogram_parsed)),
         key   = names(histogram_parsed)) %>%
  select(-histogram_parsed) %>%
  mutate(key = str_c("geo_type", key)) %>%
  spread(key, value, fill=0) %>%
  # remove a zero column
  select(-`<NA>`) %>%
  # normalize within each row
  mutate(row_sum = rowSums(select(., contains("geo_type"))),
         across(.cols=contains("geo_type"), ~ .x / row_sum)) %>%
  select(-row_sum, -histogram)

# spatial resources
lakepolys <- st_read("data_working/nla_combined.shp")
lakebbox  <- st_read("data_working/nla_combined_oriented_bbox.shp") %>%
  tibble() %>%
  select(-geometry, -SITEID, -NLA12_ID)
names(lakebbox)[2:6] <- str_c("bbox_", names(lakebbox[2:6]))
# Poles are placed with +/- 5m tolerance
lakepoles <- st_read("data_working/nla_poles_inaccessibility.shp") %>%
  tibble() %>%
  dplyr::select(-geometry)

nla_join <- lakepolys %>%
  inner_join(lakebbox, by="NLAID") %>%
  inner_join(lakegeo,  by="NLAID") %>%
  inner_join(gee_join, by="NLAID") %>%
  inner_join(nla_depth, by="NLAID") %>%
  inner_join(lakepoles, by="NLAID") %>%
  mutate(lakearea_m2 = st_area(geometry),
         lakeperim_m = st_perimeter(geometry),
         dev_index = 0.5 * lakeperim_m  / sqrt(lakearea_m2 * pi)) %>%
  rowwise() %>%
  mutate(bbox_mindim = min(bbox_width, bbox_height),
         bbox_chardim = 0.5 * bbox_mindim * (lakearea_m2 / bbox_area))


# determine which polygons are duplicated, drop the duplicates from 2007
nla07_dupes <- st_read("data_working/nla07_duplicates.shp")

nla_join_nodupes <- nla_join %>%
  filter(!(NLAID %in% nla07_dupes$SITEID)) %>%
  tibble() %>%
  dplyr::select(-geometry, -NLA12_ID, -SITEID)

nla_join_nodupes %>%
  write_csv("data_out/nla_gee.csv")
