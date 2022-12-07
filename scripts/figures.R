library(tidyverse)
library(latex2exp)
library(scales)
library(ggridges)

obs_df  <- read_csv("data_out/model_results/compiled_predictions.csv")
depths  <- read_csv("data_out/lagos_us_shape.csv") %>%
  left_join(read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv"),
            by=c("lagoslakei"="lagoslakeid"))
load("data_out/model_results/maxdepth/boruta_maxdepth")
maxdepth_boruta <- boruta
load("data_out/model_results/meandepth/boruta_meandepth")
meandepth_boruta <- boruta

pareto_fits <- read_csv("data_out/model_results/pareto_fits.csv")

wst <- read_csv("data_out/lagos_us_2k_wst_counts.csv") %>%
  select(lagoslakei, first) %>%
  rename(wst_count = first)

ref <- read_csv("data_out/lagos_us_2k_reflectance_counts.csv") %>%
  select(lagoslakei, first) %>%
  rename(ref_count = first)

# change the global ggplot2 theme to black and white to remove
# the gray background
theme_set(theme_bw())

paper_theme <- theme(
  axis.title  = element_text(size=12),
  axis.text   = element_text(size=10),
  legend.text = element_text(size=12),
  strip.text  = element_text(size=10)
)

poster_theme <- theme(
  axis.title   = element_text(size=48),
  axis.text    = element_text(size=36),
  legend.text  = element_text(size=36),
  strip.text   = element_text(size=36),
  legend.title = element_text(size=48)
)

obs_df %>%
  # Filter down to both observed and modeled depths
  # Maxdepth version
  filter(!is.na(lake_maxdepth_m) & !is.na(maxdepth_prediction_rf)) %>%
  select(lake_maxdepth_m, contains("maxdepth_prediction")) %>%
  pivot_longer(contains("maxdepth_prediction")) %>%
  # Meandepth version
  # filter(!is.na(lake_meandepth_m) & !is.na(meandepth_prediction_rf)) %>%
  # select(lake_meandepth_m, contains("meandepth_prediction")) %>%
  # pivot_longer(contains("meandepth_prediction")) %>%
  mutate(name=recode(name,
                     maxdepth_prediction_heathcote="Heathcote et al. (2015)",
                     maxdepth_prediction_hollister="Hollister et al. (2011)",
                     maxdepth_prediction_khazaei="Khazaei et al. (2022)",
                     maxdepth_prediction_oliver="Oliver et al. (2016)",
                     maxdepth_prediction_sobek="Sobek (2011)",
                     maxdepth_prediction_rf="This study",
                     meandepth_prediction_khazaei="Khazaei et al. (2022)",
                     meandepth_prediction_rf="This study",
                     meandepth_prediction_messager="Messager et al. (2016)")) %>%
  ggplot(aes(x=lake_maxdepth_m, y=(value-lake_maxdepth_m)/lake_maxdepth_m*100)) +
  geom_abline(slope=0, intercept=0, linetype="dashed", color="red", size=0.75) +
  geom_point(size=1, alpha=0.3) +
  facet_wrap(~ name) + xlim(0, 100) + ylim(-200, 200) +
  labs(x="Maximum depth (m)",
       y="Model error (%)",
       pch="") + 
  paper_theme +
  theme(strip.text.x = element_text(size=8),
        axis.title = element_text(size=10))
  
# ggsave("notebooks/paper/figures/maxdepth_one_to_one_error.png",
#        width=4.5, height=3, dpi=300)

# density of depth by area class
density_df <- obs_df %>%
  filter(!is.na(maxdepth_prediction_rf)) %>%
  mutate(sizeclass = factor(floor(log10(area))),
         sizeclass = recode_factor(sizeclass,
                                   `4`="10^4 m",
                                   `5`="10^5 m",
                                   `6`="10^6 m"))

# mode depths according to the density estimation
density_df %>%
  group_by(sizeclass) %>%
  dplyr::summarize(
    mode = density(best_maxdepth)$x[which.max(density(best_maxdepth)$y)]
  )

density_df %>%
  ggplot(aes(x=best_maxdepth)) + 
  geom_density(aes(color=sizeclass, group=sizeclass), size=1) +
  xlim(0, 30) +
  labs(x="Predicted maximum depth (m)",
       y="Density",
       color="Lake area") +
  scale_color_discrete(
    labels = unname(
      TeX(
        c(
          "10$^4$ - 10$^5$ m$^2$",
          "10$^5$ - 10$^6$ m$^2$",
          "10$^6$ - 10$^7$ m$^2$"
        )
      )
    )
  ) + paper_theme

#ggsave("notebooks/paper/figures/depth_distribution.png", width=4, height=2, dpi=300)

pareto_fits %>%
  ggplot() +
  geom_point(aes(x=minimum, y=count, pch=in_model_fitting)) +
  geom_line(aes(x=minimum, y=pred_count), color="red", size=0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  facet_wrap(~ facet_label, scales="free_x",
             labeller=label_parsed) +
  scale_shape_manual(
    name="",
    labels=c(FALSE, TRUE),
    values=c(1, 19)
  ) +
  paper_theme +
  theme(legend.position="none") +
  labs(x="Minimum value",
       y="N larger than minimum") +
  theme(strip.text.x = element_text(size=8))

#ggsave("notebooks/paper/figures/pareto.png", width=6.5, height=2.5, dpi=300)

# Determine if there is any relationship between residual and ecoregion
# Calculate mean absolute residual by ecoregion

obs_df %>%
  # Mean depth version
  # filter(!is.na(lake_meandepth_m), !is.na(meandepth_prediction_rf)) %>%
  # select(meandepth_prediction_rf, lake_meandepth_m, lagoslakeid) %>%
  # mutate(resid = meandepth_prediction_rf-lake_meandepth_m) %>%
  # Max depth version
  filter(!is.na(lake_maxdepth_m), !is.na(maxdepth_prediction_rf)) %>%
  select(maxdepth_prediction_rf, lake_maxdepth_m, lagoslakeid) %>%
  mutate(resid = maxdepth_prediction_rf-lake_maxdepth_m) %>%
  inner_join(
    read_csv("data_working/lagosus/lagos_us_ecoregions.csv") %>%
      select(lagoslakei, NA_L1NAME) %>%
      rename(ecoregion = NA_L1NAME,
             lagoslakeid=lagoslakei),
    by="lagoslakeid"
  ) %>%
  mutate(ecoregion = str_to_title(ecoregion)) %>%
  group_by(ecoregion) %>%
  mutate(n=n(),
         mar = signif(mean(abs(resid)), digits=3)) %>%
  mutate(title = str_c(ecoregion, " (", mar, " m)")) %>%
  filter(n > 10) %>%
  ggplot(aes(x=resid)) + 
  geom_histogram(aes(fill=title)) +
  facet_wrap(~ ecoregion, scales="free_y") +
  labs(x="Residual (m)", y="Count", fill="Ecoregion") +
  paper_theme +
  # mean depth version
  # xlim(-10, 10) +
  # max depth version
   xlim(-30, 30) +
  theme(strip.text.x = element_blank(),
        legend.text = element_text(size=8),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))
#ggsave("notebooks/paper/figures/maxdepth_residual_ecoregion.png", width=6, height=2.25, dpi=300)

# surface temperature animation
# not for the paper but it's pretty cool

# p <- traindf %>%
#   sample_n(5000) %>%
#   filter(maxdepth < 100) %>%
#   select(maxdepth, contains("t_anomaly")) %>%
#   pivot_longer(contains("t_anomaly")) %>%
#   mutate(month = parse_number(name)) %>%
#   select(-name) %>%
#   ggplot(aes(x=maxdepth, y=value)) + geom_point() +
#   transition_states(
#     factor(month),
#     state_length=1
#   ) +
#   labs(
#     title="Temperature Anomaly in Month: {closest_state}",
#     y="Air-water Temperature Difference (K)",
#     x="Max Depth"
#   )
# 
# anim_save("temp_anomaly.gif", p)

maxdepth_varimp <- maxdepth_boruta$ImpHistory %>%
  as_tibble() %>%
  pivot_longer(everything()) %>%
  # get rid of Infs - these appear when Boruta rejects a variable
  filter(!is.infinite(value)) %>%
  group_by(name) %>%
  dplyr::summarize(mean_inc_rmse = mean(value, na.rm=T),
                   sd_inc_rmse   = sd(value, na.rm=T),
                   n = n()) %>%
  arrange(desc(mean_inc_rmse)) %>%
  head(20) %>%
  rename(variable=name) %>%
  mutate(
    category = strsplit("tttrphrhlllplrrttocr", "")[[1]],
    model="Maximum depth model"
  )

meandepth_varimp <- meandepth_boruta$ImpHistory %>%
  as_tibble() %>%
  pivot_longer(everything()) %>%
  # get rid of Infs - these appear when Boruta rejects a variable
  filter(!is.infinite(value)) %>%
  group_by(name) %>%
  dplyr::summarize(mean_inc_rmse = mean(value, na.rm=T),
                   sd_inc_rmse   = sd(value, na.rm=T),
                   n = n()) %>%
  arrange(desc(mean_inc_rmse)) %>%
  head(20) %>%
  rename(variable=name) %>%
  mutate(
    category = strsplit("tltltpllphcthtkoptkk", "")[[1]],
    model="Mean depth model"
  )

rbind(meandepth_varimp, maxdepth_varimp) %>%
  arrange(desc(mean_inc_rmse)) %>%
  mutate(
    category = recode(category,
                      l="Local terrain metric",
                      c="Curvature terrain metric",
                      k="Kernel terrain metric",
                      p="Polygon attribute",
                      t="Surface temperature",
                      r="Surface reflectance",
                      h="HUC4 statistic",
                      o="Other"),
    variable=recode(variable, 
                    linear_term="Cone model linear term",
                    messager_volume="Geostat. volume estimate",
                    relief100_max="Max 100m relief",
                    messager_zmean="Geostat. mean depth estimate",
                    area="Lake area",
                    quad_term="Cone model quadratic term",
                    MeanCurvature_max="Max mean curvature",
                    slope_max="Max slope",
                    GaussianCurvature_min="Min Gaussian curvature",
                    MaximalCurvature_max="Max maximal curvature",
                    t_anomaly_month_11="November temperature",
                    B2_B4_ratio="B2:B4 reflectance ratio",
                    B4_B2_ratio="B4:B2 reflectance ratio",
                    dist_pole="Dist. to lake center",
                    relief50_max="Max 50m relief",
                    MeanCurvature_min="Min mean curvature",
                    VerticalCurvature_max="Max vertical curvature",
                    perimeter="Lake perimeter",
                    elev_stdDev="SD of elevation",
                    relief500_max="Max 500m relief",
                    HorizontalCurvature_max="Max horizontal curvature",
                    GaussianCurvature_max="Max Gaussian curvature",
                    relief100_stdDev="SD of 100m relief",
                    t_anomaly_month_5="May temperature",
                    t_anomaly_month_10="October temperature",
                    Hillshade_median="Median hillshade",
                    tri_median="Median terrain roughness index",
                    relief50_median="Median 50m relief",
                    relief500_min="Min 500m relief",
                    slope_median="Median slope",
                    slope_stdDev="SD of slope",
                    huc4_median_rd="Median rel. depth in HUC4",
                    huc4_median_depth="Median depth in HUC4",
                    B1_B4_ratio="B1:B4 reflectance ratio",
                    B3_B4_ratio="B3:B4 reflectance ratio",
                    B2_B3_ratio="B2:B3 reflectance ratio",
                    t_anomaly_month_12="December temperature",
                    MaximalCurvature_median="Median maximal curvature",
                    t_anomaly_month_6="June temperature",
                    B1_B3_ratio="B1:B3 reflectance ratio",
                    ws_lake_arearatio="Watershed to lake area ratio",
                    polygon_t_anomaly_month_11="November temperature",
                    polygon_t_anomaly_month_10="October temperature",
                    polygon_t_anomaly_month_5="May temperature",
                    polygon_t_anomaly_month_6="June temperature",
                    polygon_t_anomaly_month_4="April temperature",
                    polygon_t_anomaly_month_12="December temperature",
                    polygon_t_anomaly_month_3="February temperature",
                    dev500_median="Median 500m terrain deviation",
                    dev100_median="Median 100m terrain deviation",
                    dev50_median="Median 50m terrain deviation"
    )
  ) %>%
  ggplot(aes(y=reorder(variable, mean_inc_rmse), x=mean_inc_rmse,
             color=category)) + 
  geom_point() +
  geom_errorbar(aes(
    xmin=mean_inc_rmse-sd_inc_rmse*1.68,
    xmax=mean_inc_rmse+sd_inc_rmse*1.68
  )) +
  facet_wrap(~ model, scales="free") +
  labs(x="Increase RMSE (m)",
       y="",
       color="") +
  paper_theme + 
  theme(legend.text=element_text(size=10), legend.position="bottom") +
  scale_color_discrete(
    limits=c(
      "Local terrain metric",
      "Curvature terrain metric",
      "Kernel terrain metric",
      "Polygon attribute",
      "Surface temperature",
      "Surface reflectance",
      "HUC4 statistic",
      "Other"
    )
  )
ggsave("notebooks/paper/figures/var_importance_merged.png", dpi=600, width=9, height=4)

# From area 1e4 to 1e9, what % of lakes larger than the cutoff have depth measurements?
cutoffs <- seq(4, 9, by=0.5)
logarea <- log10(depths$area)
pct_depth <- c()
for (lg in cutoffs) {
  n_gt <- sum(logarea > lg)
  gt_depth <- sum(!is.na(depths$lake_maxdepth_m[logarea > lg]))
  pct_depth <- c(pct_depth, gt_depth / n_gt)
}
cutoffs_ha <- cutoffs - 4

ggplot(data=NULL, aes(x=cutoffs_ha, y=pct_depth)) + geom_point() +
  labs(y=TeX("Proportion with known $Z_{max}$"),
       x="Minimum lake area (ha)") +
  geom_vline(xintercept = 3, linetype="dashed") +
  scale_x_continuous(label=function(x) {
    parse(text=str_c("10^", as.character(x)))
  }) +
  paper_theme +
  annotation_logticks(sides="b")
#ggsave("notebooks/paper/figures/depth_proportion.png", width=2.5, height=2.5, dpi=300)

wst %>%
  inner_join(ref, by="lagoslakei") %>%
  pivot_longer(contains("count")) %>%
  mutate(name=recode(name,
                     wst_count="Water Surface Temperature",
                     ref_count="Reflectance")) %>%
  ggplot(aes(x=value)) + geom_histogram() +
  facet_wrap(~ name, scales="free_x") +
  labs(x="Number of satellite observations", y="Number of lakes") +
  paper_theme +
  # prevent cutting off the text
  theme(
    plot.margin = margin(5.5, 10, 5.5, 5.5, "pt")
  )
# ggsave("notebooks/paper/figures/satellite_obs_distribution.png", 
#        width=4.5, height=2, dpi=300)

# Calculate volume associated with slices of depth
calc_volume_section <- function(D, A, d1, d2) {
  # trim to the max depth of the lake if the desired section is deeper
  if (d2 > D) d2 <- D
  if (d1 > D) {
    # none of the volume is within the slice
    return(0)
  }
  else {
    return(
      A / (3 * D^2) * ((D - d1)^3 - (D - d2)^3)
    )
  }
}

volume_slices <- c()
for (i in 1:10) {
  slice_volume <- sum(map2_dbl(obs_df$best_maxdepth, obs_df$area, 
                               calc_volume_section, d1=i-1, d2=i))/1e9
  volume_slices <- c(volume_slices, slice_volume)
}

tot_volume_km3 <- sum(obs_df$best_maxdepth * obs_df$area / 3) * 1e-9
tot_volume_1m  <- sum(map2_dbl(obs_df$best_maxdepth, obs_df$area,
                               calc_volume_section, d1=0, d2=1)) * 1e-9
tot_volume_10m <- sum(map2_dbl(obs_df$best_maxdepth, obs_df$area,
                               calc_volume_section, d1=0, d2=10)) * 1e-9

tot_volume_km3_meandepth <- sum(obs_df$best_meandepth * obs_df$area,
                                na.rm=T) * 1e-9

have_meandepth <- obs_df$lagoslakeid[which(!is.na(obs_df$best_meandepth))]
have_maxdepth  <- obs_df$lagoslakeid[which(!is.na(obs_df$best_maxdepth))]

missing_meandepth <- have_maxdepth[which(!(have_maxdepth %in% have_meandepth))]

obs_df %>%
  filter(lagoslakeid %in% have_meandepth) %>%
  mutate(maxdepth_vol_km3 = (best_maxdepth * area / 3) * 1e-9,
         meandepth_vol_km3 = (best_meandepth * area) * 1e-9,
         vol_diff_m3 = abs(maxdepth_vol_km3 - meandepth_vol_km3)*1e9) %>%
  # summarize(
  #   sum_maxdepth_vol_km3  = sum(maxdepth_vol_km3),
  #   sum_meandepth_vol_km3 = sum(meandepth_vol_km3) 
  # ) %>%
  ggplot(aes(x=vol_diff_m3)) + geom_histogram() +
  scale_x_log10(limits=c(1e2, 1e8))

volume_slices_df <- data.frame(value=volume_slices) %>%
  mutate(r = row_number() - 0.5,
         prop = str_c(signif(100 * volume_slices/tot_volume_km3, 2), "%"),
         label = str_c("[", r-0.5, ", ", r+0.5, "]"))

volume_slices_df %>%
  ggplot(aes(x=r, group=1)) +
  geom_bar(aes(y=value), stat="identity") +
  geom_text(aes(y=value, label=prop), nudge_y=4.0, size=0.36 * 8) +
  labs(
    y=TeX("Volume (km$^3$)"),
    x="Depth (m)"
  ) + 
  ylim(0, NA) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  paper_theme
#ggsave("notebooks/paper/figures/volume_slices.png", height=2, width=4, dpi=300)

# Distribution of max depth volume
obs_df %>%
  mutate(cone_volume = best_maxdepth * area / 3,
         box_volume  = best_meandepth * area,
         areaclass = floor(log10(area)),
         areaclass_label = paste0("10^",areaclass,"-10^",areaclass+1," m^2")) %>%
  filter(!is.na(cone_volume) & !is.na(box_volume)) %>%
  select(cone_volume, box_volume, areaclass, areaclass_label) %>%
  pivot_longer(contains("volume")) %>%
  ggplot(aes(x=value)) + geom_density_ridges(aes(y=areaclass_label)) + 
  paper_theme + scale_y_discrete(labels=unname(TeX(c(
    "10$^4$-10$^5$ m$^2$",
    "10$^5$-10$^6$ m$^2$",
    "10$^6$-10$^7$ m$^2$",
    "10$^7$-10$^8$ m$^2$",
    "10$^8$-10$^9$ m$^2$",
    "$>$10$^9$ m$^2$"
  )))) + scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(y="Area class", x=TeX("Lake volume (m$^3$)"))
#ggsave("notebooks/paper/figures/vol_density.png", width=3, height=2, dpi=300)

# Distribution of lake form factor
obs_df %>%
  #filter(is.na(lake_maxdepth_m)) %>%
  mutate(form_factor = 3 * best_meandepth / best_maxdepth,
         sizeclass = floor(log10(area)),
         sizeclass_label = paste0("10^",sizeclass,"-10^",sizeclass+1," m^2"),
         shapetype = ifelse(form_factor < 1.5, "convex", "concave")) %>%
  pull(shapetype) %>% table()
  ggplot(aes(x=form_factor)) + geom_density_ridges(aes(y=sizeclass_label)) +
  geom_vline(xintercept=1.5, color="red", linetype="dashed") +
  labs(x="Volume development ratio", y="Area class") +
  xlim(0, 4) +
  paper_theme + scale_y_discrete(labels=unname(TeX(c(
    "10$^4$-10$^5$ m$^2$",
    "10$^5$-10$^6$ m$^2$",
    "10$^6$-10$^7$ m$^2$",
    "10$^7$-10$^8$ m$^2$",
    "10$^8$-10$^9$ m$^2$",
    "$>$10$^9$ m$^2$"
  ))))
#ggsave("notebooks/paper/figures/vol_dev_density.png", width=3, height=2, dpi=300)
