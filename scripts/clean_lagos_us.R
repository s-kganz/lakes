# LAGOS US has a bunch of great data in it. We built a crosswalk between
# LAGOSID and NLAID with a spatial join. Now we want to grab the relevant
# variables from LAGOS, join them, and save out the resulting table in
# a few categories.

library(tidyverse)

# read data - pull in the crosswalk and all lagos tables
# they're really big, so immediately filter down to the lakes we care about
xwalk <- read_csv(
  "data_working/lagos_mglp_info_join.csv", col_types=cols(lagoslakeid=col_character())
  ) %>%
    select(lagoslakeid, lake_nhdid)

read_lake_subset <- function(file, lakeids) {
  f <- read_csv(file, col_types = cols(.default=col_character())) %>% 
    filter(lagoslakeid %in% lakeids) %>% return()
}

lagos_net <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/nets_networkmetrics_medres.csv",
  xwalk$lagoslakeid)

lagos_ws <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_watersheds.csv",
  xwalk$lagoslakeid
)

lagos_char <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_characteristics.csv",
  xwalk$lagoslakeid
)

lagos_info <- read_lake_subset(
  "data_in/lagos/LAGOS_US_LOCUS/lake_information.csv",
  xwalk$lagoslakeid
)

# First build the geography table - Lat/lon, glaciation, NHD Unit, WS Area, 
# Ws:Lake area ratio
mglp_lagos_geography <- xwalk %>%
  inner_join(
    lagos_char %>% 
      select(lagoslakeid, lake_glaciatedlatewisc), 
    by="lagoslakeid") %>%
  inner_join(
    lagos_info %>% 
      select(lagoslakeid, lake_lat_decdeg, lake_lon_decdeg, lake_huc12),
    by="lagoslakeid") %>%
  inner_join(
    lagos_ws %>%
      select(lagoslakeid, ws_area_ha, ws_lake_arearatio),
    by="lagoslakeid"
  )

# This table is kind of small unfortunately. We can't join into the above
# tables without losing some data, so we should just hang on to everything.
mglp_lagos_network <- xwalk %>%
  inner_join(
    lagos_net,
    by="lagoslakeid"
  )

# Save out all this delicious data
write_csv(mglp_lagos_geography, "data_out/mglp_lagos_geography.csv")
write_csv(mglp_lagos_network, "data_out/mglp_lagos_network.csv")
