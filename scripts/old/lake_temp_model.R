library(tidyverse)
library(lme4)
library(mgcv)
library(RobustLinearReg)

source("scripts/util.R")

depths <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv")
fall   <- read_csv("data_working/lagosus/fall_temp_anomaly.csv")
spring <- read_csv("data_working/lagosus/spring_temp_anomaly.csv")
info   <- read_csv("data_out/lagos_us_geography.csv")

biginfo <- read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_information.csv")

biginfo %>%
  group_by(hu4_zoneid) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

model_df <- depths %>%
  select(lagoslakeid, lake_maxdepth_m) %>%
  left_join(
    fall, by="lagoslakeid"
  ) %>%
  left_join(
   spring, by="lagoslakeid"
  ) %>%
  left_join(
    info %>%
      select(lagoslakeid, lake_huc12)
  ) %>%
  mutate(lake_huc12 = map_chr(lake_huc12, parse_huc)) %>%
  mutate(lake_huc4  = str_sub(lake_huc12, end=4),
         lake_huc2  = str_sub(lake_huc12, end=2),
         lake_huc8  = str_sub(lake_huc12, end=6)) %>%
  filter(!is.na(fall_temp_anomaly_k),
         !is.na(spring_temp_anomaly_k),
         lake_maxdepth_m < 200)
  #group_by(lake_huc12) %>%
  #mutate(across(contains("anomaly"), ~scale(.x)))
  

model_df %>%
  group_by(lake_huc12) %>%
  filter(n() > 10) %>%
  summarize(tcorr = cor(spring_temp_anomaly_k,
                        fall_temp_anomaly_k,
                        use="c")) %>%
  #filter(abs(tcorr) != 1) %>%
  ggplot(aes(x=tcorr)) + geom_histogram() +
  geom_vline(xintercept=0)

model_df %>%
  rename(Fall=fall_temp_anomaly_k,
         Spring=spring_temp_anomaly_k) %>%
  pivot_longer(c(Fall, Spring)) %>%
  ggplot(aes(y=value, x=lake_maxdepth_m)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ name) +
  labs(x="Maximum Depth (m)", y="Temperature Anomaly (K)")

depth_gam <- gam(lake_maxdepth_m ~ s(fall_temp_anomaly_k) + s(spring_temp_anomaly_k),
                 data=model_df)

summary(depth_gam)

depth_lm <- lm(lake_maxdepth_m ~ spring_temp_anomaly_k,
               data=model_df)

depth_ts <- theil_sen_regression(
  lake_maxdepth_m ~ spring_temp_anomaly_k,
  data=model_df
)
