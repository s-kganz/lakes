library(tidyverse)
library(ggridges)

hypso <- read_csv("data_in/stachelek_hypso/hypso.csv") %>%
  # attach areas
  left_join(
    read_csv("data_out/lagos_us_shape.csv") %>% select(lagoslakei, area),
    by=c("llid"="lagoslakei"))
obs_df <- read_csv("data_out/model_results/maxdepth/compiled_predictions.csv")
stachelek_ids <- unique(hypso$llid)

obs_df %>%
  mutate(form_factor = 3 * best_meandepth / best_maxdepth,
         shapetype   = case_when(
           form_factor < 1.5 ~ "convex",
           form_factor >= 1.5 ~ "concave"
         )) %>%
  pull(shapetype) %>% table()
  #ggplot(aes(x=form_factor)) + geom_histogram()

calc_zmean <- function(area_pct, depth_pct) {
  # sort both vectors in order of increasing area
  sorted <- sort(area_pct, index.return=TRUE)
  # normalize from 0 - 1
  area_sort  <- sorted$x / 100
  depth_sort <- depth_pct[sorted$ix] / 100
  pracma::trapz(area_sort, depth_sort)
}

# reproduce the distribution of concave/convex they report
hypso_ff <- hypso %>%
  group_by(llid) %>%
  summarize(zmax=first(maxdepth),
            area=first(area),
            hypso_int = calc_zmean(area_percent, depth_percent),
            zmean = hypso_int * zmax) %>%
  mutate(form_factor = zmean * 3 / zmax,
         shapetype = ifelse(form_factor < 1.5, "convex", "concave"))

hypso_ff %>%  
  # it matches
  pull(shapetype) %>% table()

# draw a similar area distribution as their data
# we only model up to 1e7 m^2, so filter first
sample_weights <- hypso_ff %>%
  filter(area < 1e7) %>%
  mutate(sizeclass=floor(log10(area))) %>%
  group_by(sizeclass) %>%
  summarize(sample_count = n() / nrow(.))

group_sizes <- nrow(hypso_ff) * sample_weights$sample_count

obs_df_sample <- obs_df %>% 
  filter(area < 1e7) %>%
  filter(!(lagoslakeid %in% stachelek_ids),
         is.na(lake_maxdepth_m)) %>%
  mutate(form_factor = 3 * best_meandepth / best_maxdepth,
         sizeclass=floor(log10(area))) %>%
  left_join(sample_weights, by="sizeclass") %>%
  group_split(sizeclass) %>%
  map2_dfr(group_sizes, ~slice_sample(.x, n=.y)) %>%
  select(lagoslakeid, form_factor, area, sizeclass) %>%
  mutate(group="RF modeled lakes")
  
table(obs_df_sample$sizeclass)

stachelek_sample <- hypso_ff %>%
  filter(area < 1e7) %>%
  mutate(sizeclass = floor(log10(area))) %>%
  select(llid, form_factor, area, sizeclass) %>%
  rename(lagoslakeid=llid) %>%
  mutate(group="Stachelek lakes")

# compare the distribution of form factors with their real data
rbind(
  obs_df_sample,
  stachelek_sample
) %>%
  ggplot(aes(x=form_factor, y=group)) + geom_density_ridges() +
  xlim(0, 4)

# compare the area distributions
rbind(
  obs_df_sample,
  stachelek_sample
) %>%
  ggplot(aes(x=area, y=group)) + geom_density_ridges() +
  scale_x_log10()

obs_df_sample %>% pull(sizeclass) %>% table()
stachelek_sample %>% pull(sizeclass) %>% table()

obs_df_sample %>% pull(form_factor) %>% median()
