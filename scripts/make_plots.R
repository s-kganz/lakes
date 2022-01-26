library(tidyverse)
library(latex2exp)

shape <- read_csv("data_out/lagos_us_shape.csv") %>%
  rename(lagoslakeid = lagoslakei) %>%
  full_join(
    read_csv("data_in/lagos/LAGOS_US_LOCUS/lake_depth.csv") %>%
      select(lagoslakeid, lake_maxdepth_m) %>%
      rename(maxdepth = lake_maxdepth_m),
    by="lagoslakeid"
  ) %>%
  mutate(hasDepth = is.na(maxdepth))

#area_cdf <- ecdf(log10(shape$area))
#plot(area_cdf)

# From area 1e4 to 1e9, what % of lakes larger than the cutoff have depth measurements?
cutoffs <- seq(4, 9, by=0.5)
logarea <- log10(shape$area)
pct_depth <- c()
for (lg in cutoffs) {
  n_gt <- sum(logarea > lg)
  gt_depth <- sum(!is.na(shape$maxdepth[logarea > lg]))
  pct_depth <- c(pct_depth, gt_depth / n_gt)
}
cutoffs_ha <- cutoffs - 4

plot(cutoffs_ha, pct_depth)

ggplot(data=NULL, aes(x=cutoffs_ha, y=pct_depth)) + geom_point() +
  labs(y=TeX("Proportion with known $Z_{max}$"),
       x="Minimum lake area (log ha)") +
  geom_vline(xintercept = 3, linetype="dashed")

ggsave("notebooks/paper/figures/depth_proportion.png")

save(depth_proportion, file="notebooks/paper/figures/depth_proportion")

ggplot(NULL, aes(x=logarea)) + geom_histogram()
