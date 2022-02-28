# Make figures showing the distribution of the number of satellite observations
# for each lake.
library(tidyverse)

wst <- read_csv("data_working/lagosus/lagos_us_2k_wst_counts.csv") %>%
  select(lagoslakei, first) %>%
  rename(wst_count=first)

ref <- read_csv("data_working/lagosus/lagos_us_2k_reflectance_counts.csv") %>%
  select(lagoslakei, first) %>%
  rename(ref_count=first)

wst %>%
  inner_join(ref, by="lagoslakei") %>%
  pivot_longer(contains("count")) %>%
  mutate(name=recode(name,
                     wst_count="Water Surface Temperature",
                     ref_count="Reflectance")) %>%
  ggplot(aes(x=value)) + geom_histogram() +
  facet_wrap(~ name, scales="free_x") +
  labs(x="Number of satellite observations", y="Number of lakes")
