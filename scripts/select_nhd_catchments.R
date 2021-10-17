# The goal of this script is to read in a series of shapefiles, extract the
# the polygons that intersect the nla_combined.shp dataset, then spit out
# a single shapefile containing all the intersecting polygons. It is basically
# the same as select by location, except we are selecting across many shapefiles.

library(sf)
# https://stackoverflow.com/questions/68478179/
# how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
# No one on the internet would ever lie, right?
sf::sf_use_s2(FALSE)

select_shp_dir <- "data_in/nhd_plus_simplified_catchments"
select_shps    <- list.files(select_shp_dir, pattern="*.shp", recursive=T)

query_shp <- "data_working/nla_combined.shp"
query_df <- st_read(query_shp)
result_df <- NULL

for (select_shp in select_shps) {
  select_df <- st_read(file.path(select_shp_dir, select_shp)) %>%
    # fix invalid geometries
    st_make_valid()
  # transform the query dataframe if the projections do not match
  if (st_crs(query_df) != st_crs(select_df)) {
    query_df <- st_transform(query_df, crs=st_crs(select_df))
  }
  # get what catchments intersect the query layer
  this_intersect <- select_df[query_df, ]
  # join with the output df
  if (is.null(result_df)) {
    result_df <- this_intersect
  } else {
    result_df <- rbind(result_df, this_intersect)
  }
}

st_write(result_df, "data_working/nhdplus_catchments_simplified_select.shp")
