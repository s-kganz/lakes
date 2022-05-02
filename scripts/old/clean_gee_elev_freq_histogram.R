library(tidyverse)

# Need the convex/concave classification from Bathybase, lake elev table,
# and the elevation frequency histogram of the basin

mn_geo <- read_csv("data_working/mn_lake_geometry.csv") %>%
  # DOWLKNUM should be a character, zero-padded to 8 digits
  mutate(DOWLKNUM = str_pad(DOWLKNUM, side="left", width=8, pad="0"))

mn_elev  <- read_csv("data_in/gee/mn/mn_lake_elev.csv") %>%
  select(DOWLKNUM, LAKE_NAME, min) %>%
  rename(lake_elev = min)

mn_hypso <- read_csv("data_in/gee/mn/mn_basin_hypsometry.csv") %>%
  select(DOWLKNUM, LAKE_NAME, histogram)

mn_hi    <- read_csv("data_working/bathybase.csv") %>%
  filter(domain=="www.mngeo.state.mn.us") %>%
  rename(DOWLKNUM=DOWLKNUM.)

clean_frequency_histogram <- function(hist, label) {
  # make a vector of each histogram entry
  str_split(str_sub(hist, 2, str_length(hist)-2), ", ")[[1]] %>%
    str_split("=", simplify=T) %>%
    data.frame(label=label) %>% 
    return()
}

hypso_df <- map2_df(mn_hypso$histogram, 
                    mn_hypso$DOWLKNUM, 
                    clean_frequency_histogram) %>%
  rename(elev=X1,
         area=X2,
         DOWLKNUM=label) %>%
  mutate(elev=parse_number(elev),
         # pixel size is 30m, so 900m^2 per pixel
         area = parse_number(area) * 900) %>%
  inner_join(mn_elev, by="DOWLKNUM") %>%
  inner_join(
    mn_hi %>% select(DOWLKNUM, Max.Depth) %>% rename(depth=Max.Depth),
    by="DOWLKNUM"
  ) %>%
  inner_join(
    mn_geo %>% select(DOWLKNUM, area) %>% rename(lake_area=area),
    by="DOWLKNUM"
  )

# now do the hypsometric calculations
hypso_df <- hypso_df %>%
  group_by(DOWLKNUM) %>%
  arrange(elev) %>%
  mutate(cumarea = cumsum(area)) %>%
  mutate(
    # round to prevent adding precision where we don't have any
    elev_feet = round(elev * 3.28084),
    lake_elev_feet = round(lake_elev * 3.28084),
    feet_above_water = elev_feet - lake_elev_feet,
    perc_area_increase = (cumarea / lake_area) - 1,
    perc_depth_increase = feet_above_water / depth
  )

# When area is equal to lake area, elev above water == 0. We need to explicitly
# add this data point to each group.
hypso_df %>%
  summarize(DOWLKNUM=last(DOWLKNUM)) %>%
  mutate(feet_above_water = 0, perc_area_increase = 0, perc_depth_increase = 0) %>%
  bind_rows(hypso_df, .) %>%
  filter(perc_area_increase >= 0, perc_depth_increase <= 1) %>%
  ggplot(aes(x=perc_area_increase, y=perc_depth_increase)) +
  geom_line(aes(group=DOWLKNUM), alpha=0.5)
