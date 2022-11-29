library(tidyverse)
library(doParallel)
library(foreach)

# This file derives coefficients for Hurst and Pareto regressions. Hurst is
# bootstrapped for consistency with Cael and Seekell (2017), but the Pareto
# model is just fit once.

n_trials <- 1e3

df <- read_csv("data_out/model_results/compiled_predictions.csv") %>%
  rename(maxdepth=lake_maxdepth_m,
         meandepth=lake_meandepth_m) %>%
  mutate(cone_volume = best_maxdepth * area / 3) %>%
  mutate(logarea  = log10(area),
         log_model_zmax  = log10(best_maxdepth),
         log_model_zmean = log10(best_meandepth),
         log_obs_zmax    = log10(maxdepth),
         log_obs_zmean   = log10(meandepth),
         logvol          = log10(cone_volume)) %>%
  mutate(zmax_group = ifelse(is.na(maxdepth), "modeled", "observed"),
         zmean_group = ifelse(is.na(maxdepth), "modeled", "observed"))

# We are interested in the Hurst exponent in three cases:
# observed zmax, pooled observed/modeled zmax, and modeled zmax with a similar
# area distribution to the observations.
# make dfs containing no NAs for the first two cases
df_zmax_obs <- df %>% filter(!is.na(maxdepth))
df_zmax_all <- df %>% filter(!is.na(best_maxdepth))
df_zmax_mod <- df %>% filter(is.na(maxdepth) & !is.na(best_maxdepth))

# need to derive sampling weights to do the modeled-only regression right
area_sample_weights <- (df_zmax_obs %>%
                          filter(area < 1e7) %>%
                          mutate(areaclass = floor(log10(area))) %>%
                          pull(areaclass) %>% table() / nrow(df_zmax_obs)) %>%
  as.data.frame() %>%
  rename(sample_weight=2,
         areaclass=1) %>%
  # areaclass needs to not be a factor type
  mutate(areaclass = parse_number(as.character(areaclass)))

# attach the sampling weights to modeled depths
df_zmax_mod <- df_zmax_mod %>%
  mutate(areaclass = floor(log10(area))) %>%
  inner_join(area_sample_weights, by="areaclass")

# register the parallel backend
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

df_all_slopes <- foreach(i=1:n_trials, .combine=rbind,
                         .packages=c("dplyr"), .verbose=T) %dopar% {
  # make the bootstraps
  df_zmax_obs_sample <- slice_sample(df_zmax_obs, prop=1, replace=T)
  df_zmax_all_sample <- slice_sample(df_zmax_all, prop=1, replace=T)
  # this bootstrap will have an areal distribution representative of the observed depths
  df_zmax_mod_sample <- slice_sample(df_zmax_mod, prop=1, replace=T, weight_by=sample_weight)
  
  # run the regressions
  lm_zmax_obs <- lm(log(maxdepth) ~ log(area), data=df_zmax_obs_sample)
  lm_zmax_all <- lm(log(best_maxdepth) ~ log(area), data=df_zmax_all_sample)
  lm_zmax_mod <- lm(log(best_maxdepth) ~ log(area), data=df_zmax_mod_sample)
  
  # collect the coefficients as output
  list(
   zmax_obs_slope =lm_zmax_obs$coefficients[2],
   zmax_obs_int   =lm_zmax_obs$coefficients[1],
   zmax_all_slope =lm_zmax_all$coefficients[2],
   zmax_all_int   =lm_zmax_all$coefficients[1],
   zmax_mod_slope =lm_zmax_mod$coefficients[2],
   zmax_mod_int   =lm_zmax_mod$coefficients[1]
  )
} %>% as.data.frame() 

df_all_slopes <- df_all_slopes %>% mutate(across(everything(), unlist))

stopCluster(cl)

df_all_slopes %>%
  # convert slope to hurst exponent
  mutate(across(contains("slope"), ~.x * 2)) %>%
  pivot_longer(everything()) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(hurst_median = median(value),
            hurst_lowci  = quantile(value, probs=0.05),
            hurst_highci = quantile(value, probs=0.95)) %>%
  write_csv("data_out/model_results/hurst_coefficients.csv")

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
  write_csv("data_out/model_results/pareto_fits.csv")
