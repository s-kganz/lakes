# LAGOS US has a bunch of great data in it. We built a crosswalk between
# LAGOSID and NLAID with a spatial join. Now we want to grab the relevant
# variables from LAGOS, join them, and save out the resulting table in
# a few categories.

library(tidyverse)

# Read data - pull in the crosswalk and all lagos tables
# they're really big, so immediately filter down to the lakes we care about
# xwalk <- read_csv(
#   "data_working/lagos_mglp_info_join.csv", col_types=cols(lagoslakeid=col_character())
#   ) %>%
#     select(lagoslakeid, lake_nhdid)

# If not using a crosswalk, grab the depths you care about here
lagoslakeids <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>% 
  pull(lagoslakeid)

read_lake_subset <- function(file, lakeids) {
  f <- read_csv(file, col_types = cols(.default=col_character())) %>% 
    filter(lagoslakeid %in% lakeids) %>% return()
}

lagos_net <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/nets_networkmetrics_medres.csv",
  lagoslakeids)

lagos_ws <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_watersheds.csv",
  lagoslakeids
)

lagos_char <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_characteristics.csv",
  lagoslakeids
)

lagos_info <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_information.csv",
  lagoslakeids
)

# First build the geography table - Lat/lon, glaciation, NHD Unit, WS Area, 
# Ws:Lake area ratio
lagos_geography <- lagos_char %>% 
  select(lagoslakeid, lake_glaciatedlatewisc, lake_connectivity_class,
         contains("upstream")) %>%
  inner_join(
    lagos_info %>% 
      select(lagoslakeid, lake_lat_decdeg, lake_lon_decdeg, 
             lake_huc12, lake_elevation_m),
    by="lagoslakeid") %>%
  inner_join(
    lagos_ws %>%
      select(lagoslakeid, ws_area_ha, ws_lake_arearatio),
    by="lagoslakeid"
  )

# This table is kind of small unfortunately. We can't join into the above
# tables without losing some data, so we should just hang on to everything.
lagos_network <- lagos_net # no joining necessary

# Save out all this delicious data
write_csv(lagos_geography, "data_out/lagos_us_geography.csv")
write_csv(lagos_network, "data_out/lagos_us_network.csv")
