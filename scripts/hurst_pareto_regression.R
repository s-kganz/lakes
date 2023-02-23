library(tidyverse)
library(doParallel)
library(foreach)
library(latex2exp)

# This file derives coefficients for Hurst and Pareto regressions. Hurst is
# bootstrapped for consistency with Cael and Seekell (2017), but the Pareto
# model is just fit once.

set.seed(12012022) # for consistent resamples
n_trials <- 1e3

df <- read_csv("data_out/model_results/lt1000ha_compiled_predictions.csv") %>%
  rename(maxdepth=lake_maxdepth_m,
         meandepth=lake_meandepth_m) %>%
  mutate(cone_volume = best_maxdepth * area / 3,
         box_volume  = best_meandepth * area,
         rf_cone_volume = maxdepth_m_prediction_rf * area / 3,
         rf_box_volume  = meandepth_m_prediction_rf * area) %>%
  mutate(logarea  = log10(area),
         log_model_zmax  = log10(best_maxdepth),
         log_model_zmean = log10(best_meandepth),
         log_obs_zmax    = log10(maxdepth),
         log_obs_zmean   = log10(meandepth),
         logvol          = log10(cone_volume)) %>%
  mutate(zmax_group = ifelse(is.na(maxdepth), "modeled", "observed"),
         zmean_group = ifelse(is.na(maxdepth), "modeled", "observed"))

# We are interested in the Hurst exponent in four cases:
# observed zmax, pooled observed/modeled zmax, and modeled zmax & zmean with a 
# similar area distribution to the observations.
# Make dfs containing no NAs for the first two cases
df_zmax_obs  <- df %>% filter(!is.na(maxdepth))
df_zmean_obs <- df %>% filter(!is.na(meandepth))
df_zmax_all  <- df %>% filter(!is.na(best_maxdepth))
df_zmax_mod  <- df %>% filter(is.na(maxdepth) & !is.na(best_maxdepth))
df_zmean_mod <- df %>% filter(is.na(meandepth) & !is.na(best_meandepth))

# need to derive sampling weights to do the modeled-only regression right
zmax_area_sample_weights <- (
  df_zmax_obs %>%
    filter(area < 1e7) %>%
    mutate(areaclass = floor(log10(area))) %>%
    pull(areaclass) %>% table() / nrow(df_zmax_obs)
) %>%
  as.data.frame() %>%
  rename(sample_weight = 2,
         areaclass = 1) %>%
  # areaclass needs to not be a factor type
  mutate(areaclass = parse_number(as.character(areaclass)))

zmean_area_sample_weights <- (
  df_zmean_obs %>%
    filter(area < 1e7) %>%
    mutate(areaclass=floor(log10(area))) %>%
    pull(areaclass) %>% table() / nrow(df_zmean_obs)
) %>%
  as.data.frame() %>%
  rename(sample_weight=2,
         areaclass=1) %>%
  mutate(areaclass = parse_number(as.character(areaclass)))

# attach the sampling weights to modeled depths
df_zmax_mod <- df_zmax_mod %>%
  mutate(areaclass = floor(log10(area))) %>%
  inner_join(zmax_area_sample_weights, by="areaclass")

df_zmean_mod <- df_zmean_mod %>%
  mutate(areaclass = floor(log10(area))) %>%
  inner_join(zmean_area_sample_weights, by="areaclass")

# register the parallel backend
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

df_all_slopes <- foreach(i=1:n_trials, .combine=rbind,
                         .packages=c("dplyr"), .verbose=T) %dopar% {
  # make the bootstraps
  df_zmax_obs_sample <- slice_sample(df_zmax_obs, prop=1, replace=T)
  df_zmax_all_sample <- slice_sample(df_zmax_all, prop=1, replace=T)
  # these bootstraps will have an areal distribution representative of the 
  # observed depths
  df_zmax_mod_sample <- slice_sample(df_zmax_mod, prop=1, replace=T, 
                                     weight_by=sample_weight)
  df_zmean_mod_sample <- slice_sample(df_zmean_mod, prop=1, replace=T,
                                      weight_by=sample_weight)
  
  # run the regressions
  lm_zmax_obs <- lm(log(maxdepth) ~ log(area), data=df_zmax_obs_sample)
  lm_zmax_all <- lm(log(best_maxdepth) ~ log(area), data=df_zmax_all_sample)
  lm_zmax_mod <- lm(log(best_maxdepth) ~ log(area), data=df_zmax_mod_sample)
  lm_zmean_mod <- lm(log(best_maxdepth) ~ log(area), data=df_zmean_mod_sample)
  
  # Do all the other models too as comparison
  lm_zmax_hollister <- lm(log(maxdepth_m_prediction_hollister) ~ log(area),
                          data=df_zmax_mod_sample)
  lm_zmax_sobek     <- lm(log(maxdepth_m_prediction_sobek) ~ log(area),
                          data=df_zmax_mod_sample)
  lm_zmax_oliver    <- lm(log(maxdepth_m_prediction_oliver) ~ log(area),
                          data=df_zmax_mod_sample)
  lm_zmax_khazaei   <- lm(log(maxdepth_m_prediction_khazaei) ~ log(area),
                          data=df_zmax_mod_sample)
  lm_zmax_heathcote <- lm(log(maxdepth_m_prediction_heathcote) ~ log(area),
                          data=df_zmax_mod_sample)
  lm_zmean_khazaei  <- lm(log(meandepth_m_prediction_khazaei) ~ log(area),
                          data=df_zmean_mod_sample)
  lm_zmean_messager <- lm(log(meandepth_m_prediction_messager) ~ log(area),
                          data=df_zmean_mod_sample)
  
  # Collect the coefficients as output.
  # We don't really care about the intercept, but 2x the slope gives us the
  # Hurst exponent.
  list(
   zmax_obs_slope =lm_zmax_obs$coefficients[2],
   zmax_obs_int   =lm_zmax_obs$coefficients[1],
   zmax_all_slope =lm_zmax_all$coefficients[2],
   zmax_all_int   =lm_zmax_all$coefficients[1],
   zmax_mod_slope =lm_zmax_mod$coefficients[2],
   zmax_mod_int   =lm_zmax_mod$coefficients[1],
   zmax_hollister_slope = lm_zmax_hollister$coefficients[2],
   zmax_heathcote_slope = lm_zmax_heathcote$coefficients[2],
   zmax_sobek_slope = lm_zmax_sobek$coefficients[2],
   zmax_oliver_slope = lm_zmax_oliver$coefficients[2],
   zmax_khazaei_slope = lm_zmax_khazaei$coefficients[2],
   zmean_mod_slope = lm_zmean_mod$coefficients[2],
   zmean_khazaei_slope = lm_zmean_khazaei$coefficients[2],
   zmean_messager_slope = lm_zmean_messager$coefficients[2]
  )
} %>% as.data.frame() 

df_all_slopes <- df_all_slopes %>% mutate(across(everything(), unlist))

stopCluster(cl)

df_all_slopes %>%
  # convert slope to hurst exponent
  mutate(across(contains("slope"), ~.x * 2)) %>%
  # rename to the exponent for clarity
  rename_with(function(x) str_replace(x, "slope", "hurst"),
              contains("slope")) %>%
  pivot_longer(everything()) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(median = median(value),
            low_95ci  = quantile(value, probs=0.05),
            high_95ci = quantile(value, probs=0.95)) %>%
  write_csv("data_out/model_results/lt1000ha_hurst_coefficients.csv")

# Now do the Pareto regressions on area, volume, and depth.
# This model uses observed depths if they are available, and uses the
# modeled depths if the lake is smaller than 1000 ha. If a lake larger than
# 1000 ha has no observed depth, then it is excluded.
minarea <- round(log10(min(df$area)))
maxarea <- round(log10(max(df$area)))
minvol  <- round(log10(min(df$cone_volume)))
maxvol  <- round(log10(max(df$cone_volume)))
minz    <- log10(1)
maxz    <- round(log10(max(df$best_maxdepth)))

pareto_df <- rbind(
  data.frame(stat=c("area"), log_minimum=seq(minarea, maxarea, length.out=50)),
  data.frame(stat=c("volume"), log_minimum=seq(minvol, maxvol, length.out=50)),
  data.frame(stat=c("depth"), log_minimum=seq(minz, maxz, length.out=50))
) %>%
  mutate(count = map2_dbl(stat, log_minimum, function(s, lm) {
    minimum <- 10^lm
    if (s == "area") sum(df$area > minimum)
    else if (s == "volume") sum(df$cone_volume > minimum)
    else sum(df$best_maxdepth > minimum)
  })) %>%
  # can't have zero lakes
  filter(count > 0)

# make sure the fits look reasonable
ggplot(pareto_df, aes(x=log_minimum, y=count)) +
  geom_point() + scale_y_log10() +
  facet_wrap(~ stat, ncol=3, scales="free_x")

# After Messager et al. (2016), exclude points at the extreme ends of the
# range since those are probably underobserved
pareto_df <- pareto_df %>%
  mutate(
    in_model_fitting = map2_lgl(stat, log_minimum, function(s, lm) {
      if (s == "area") {lm <= 9}
      else if (s == "depth") {(lm >= 0.5) & (lm <= 2.5)}
      else {(lm >= 4) & (lm <= 10)}
    })
  )

pareto_fits <- pareto_df %>%
  filter(in_model_fitting) %>%
  group_by(stat) %>%
  nest() %>%
  mutate(linmod = map(data, function(d) {lm(log10(count) ~ log_minimum, data=d)}),
         int    = map_dbl(linmod, function(m) m$coefficients[1]),
         slope  = map_dbl(linmod, function(m) m$coefficients[2])) %>%
  select(-data, -linmod)

# Manually create the equations for each stat - there's probably a smarter
# way to do this but oh well.
pareto_fits$facet_label <- c(
  paste0("Area~(list(m^2,N == 10^", signif(pareto_fits$int[1], 3),"%*%A^", 
         signif(pareto_fits$slope[1], 3),"))"),
  paste0("Volume~(list(m^3,N == 10^", signif(pareto_fits$int[2], 3),"%*%V^",
         signif(pareto_fits$slope[2], 3),"))"),
  paste0("Depth~(list(m,N == 10^", signif(pareto_fits$int[3], 3),"%*%Z^",
         signif(pareto_fits$slope[3], 3),"))")
)

pareto_df %>%
  mutate(minimum = 10^log_minimum) %>%
  left_join(pareto_fits, by="stat") %>%
  mutate(pred_count = 10^int * minimum^slope) %>%
  write_csv("data_out/model_results/lt1000ha_pareto_fits.csv")

# Bootstrapped regression of the box volume (from mean depth) and the cone
# volume (from max depth)

volume_df <- df %>%
  filter(!is.na(rf_cone_volume) & !is.na(rf_box_volume)) %>%
  select(rf_cone_volume, rf_box_volume)

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

volume_slopes <- foreach(i=1:n_trials, .combine=c, .packages=c("dplyr"), .verbose=T) %dopar% {
  volume_slice <- slice_sample(volume_df, prop=1, replace=T)
  volume_lm <- lm(rf_cone_volume ~ rf_box_volume, data=volume_slice)
  volume_lm$coefficients[2]
}

stopCluster(cl)

print(median(volume_slopes))

volume_df %>%
  slice_sample(prop=0.1) %>%
  ggplot(aes(x=rf_cone_volume, y=rf_box_volume)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(x=TeX("$V_{cone} = \\frac{1}{3} \\times Z_{max} \\times A$"),
       y=TeX("$V_{box} = Z_{mean} \\times A$"))
  
sum(volume_df$rf_box_volume / 1e9)
sum(volume_df$rf_cone_volume / 1e9)

sum((df$maxdepth_m_prediction_hollister * df$area / 3) / 1e9, na.rm=T)
sum((df$maxdepth_m_prediction_oliver * df$area / 3) / 1e9, na.rm=T)
sum((df$maxdepth_m_prediction_sobek * df$area / 3) / 1e9, na.rm=T)
sum((df$maxdepth_m_prediction_heathcote * df$area / 3) / 1e9, na.rm=T)
sum((df$maxdepth_m_prediction_khazaei * df$area / 3) / 1e9, na.rm=T)
sum(df$meandepth_m_prediction_khazaei * df$area / 1e9, na.rm=T)
sum(df$meandepth_m_prediction_messager * df$area / 1e9, na.rm=T)
