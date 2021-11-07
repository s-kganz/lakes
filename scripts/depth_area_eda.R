library(sf)
library(tidyverse)

contours <- st_read("data_in/ct_bathymetry/lake_contour_geoms.shp")
lakes    <- st_read("data_in/ct_bathymetry/lake_outline_geoms.shp") %>%
  rename(lake_area=area)

plot_df <- contours %>%
  tibble() %>%
  inner_join(lakes %>% select(WB_NO, lake_area), by="WB_NO") %>%
  tibble() %>%
  select(-contains("geometry")) %>%
  arrange(WB_NO, desc(DEPTH_FT)) %>%
  group_by(WB_NO) %>%
  mutate(cumulative_area=cumsum(area),
         maxdepth = max(DEPTH_FT)) %>%
  ungroup() %>%
  mutate(perc_area = cumulative_area / lake_area * 100,
         perc_depth = DEPTH_FT / maxdepth * 100)

plot_df %>%
  ggplot(aes(x=perc_area, y=perc_depth)) + 
  geom_line(aes(group=WB_NO), size=1.2, alpha=0.5) +
  geom_abline() + scale_y_reverse()

