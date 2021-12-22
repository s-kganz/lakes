library(tidyverse)
library(Boruta)
library(ranger)
library(foreach)
library(doParallel)
library(lme4)
library(MuMIn)
library(ggridges)

source("scripts/util.R")

join_column <- "NLAID"
model_df <- read_csv("data_out/nla_combined_shape.csv") %>%
  inner_join(
    read_csv("data_out/nla_combined_terrain.csv"),
    by=join_column
  ) %>%
  # inner_join(
  #   read_csv("data_out/lagos_ne_network.csv"),
  #   by=join_column
  # ) %>%
  inner_join(
    read_csv(
      "data_out/nla_combined_geography.csv",
      col_types=cols(lake_huc12=col_character())),# %>%
      #rename(lagoslakei = lagoslakeid),
    by=join_column
  ) %>%
  inner_join(
    read_csv("data_out/nla_hollister_gee.csv") %>%
      select(NLAID, maxdepth),# %>%
      #rename(maxdepth=zmaxobs),
    by=join_column
  ) %>%
  select(
    -contains("lagoslakei"), -contains("dam") #-OBJECTID 
     #-nhdplusv2_comid,
     #-lake_nets_upstreamlake_km, -lake_nets_downstreamlake_km,
     #net_dams_n, lake_nets_damonlake_flag
  ) %>%
  mutate(
    lake_huc12_pad = map_chr(lake_huc12, parse_huc),
    lake_huc02 = str_sub(lake_huc12_pad, 1, 2),
    lake_huc04 = str_sub(lake_huc12_pad, 1, 4),
    lake_huc06 = str_sub(lake_huc12_pad, 1, 6),
    #net_id = factor(net_id)
  ) %>% drop_na()

apply(model_df, 2, function(x) sum(is.na(x)) / length(x))

# This is slow, run the formula below
boruta <- Boruta(maxdepth ~ ., data=model_df, doTrace=2, maxRuns=30)
confirmed <- names(
  boruta$finalDecision[boruta$finalDecision == "Confirmed"]
)
boruta_formula <- str_c("maxdepth", "~", str_c(confirmed, collapse="+")) %>%
  as.formula()

trainInd <- sample(1:nrow(model_df), round(nrow(model_df) * 0.8))

traindf <- model_df[ trainInd, ]
validdf <- model_df[-trainInd, ]

test_rf <- ranger(
  boruta_formula,
  data=traindf,
  importance="permutation"
)

validdf$rf_pred_maxdepth <- predict(test_rf, validdf)$predictions

ggplot(validdf, aes(x=maxdepth, y=rf_pred_maxdepth)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  xlim(0, 50) +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")

# Calc R2 and RMSE
cor(validdf$rf_pred_maxdepth, validdf$maxdepth) ^ 2
sqrt(mean((validdf$rf_pred_maxdepth - validdf$maxdepth) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(maxdepth ~ rf_pred_maxdepth, data=validdf)
summary(valid_lm) # slope = 1.24, so prediction is slightly underestimating

ggplot(
  test_rf$variable.importance %>% data.frame(variable=names(.), importance=.),
  aes(x=reorder(variable,importance), 
      y=importance,fill=importance)
  ) + 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill="none")+
  scale_fill_gradient(low="red", high="blue")

# try re-running the model with only the top 20 predictors - how good is it then?
varImp <- test_rf$variable.importance %>%
  data.frame(variable=names(.), importance=.)

varImp_top <- varImp %>%
  arrange(desc(importance)) %>%
  head(20)

form <- str_c("maxdepth ~ ", str_c(varImp_top$variable, collapse="+")) %>%
  as.formula()

test_rf2 <- ranger(
  form,
  data=traindf,
  importance="permutation"
)

validdf$rf_pred_maxdepth2 <- predict(test_rf2, validdf)$predictions

# Calc R2 and RMSE
cor(validdf$rf_pred_maxdepth2, validdf$maxdepth) ^ 2
sqrt(mean((validdf$rf_pred_maxdepth2 - validdf$maxdepth) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(maxdepth ~ rf_pred_maxdepth2, data=validdf)
summary(valid_lm) # m = 1.12, so prediction is slightly underestimating

ggplot(validdf, aes(x=maxdepth, y=rf_pred_maxdepth2)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  xlim(0, 50) +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")


# run Sobek, Heathcote, and Hollister as comparison
model_df$linear_term <- model_df$dist_pole * model_df$slope_median
hollister <- lm(maxdepth ~ linear_term, 
                data=model_df %>% mutate(linear_term = dist_pole * slope_median))

sobek <- lm(maxdepth ~ logarea + slope_max,
            data = model_df %>% mutate(logarea = log(area)))

# Really if I'm doing this perfectly it should be lake elevation.
# BUT, min within the buffer is close enough
heathcote <- lm(maxdepth ~ log_elev_change,
                data=model_df %>%
                  filter(maxdepth > 0, elev_median > elev_min) %>%
                  mutate(log_maxdepth_m = log(maxdepth),
                         log_elev_change = log(elev_median - elev_min)))

# Oliver et al.'s model is a little more complex - uses the mixed effects syntax
oliver_df <- model_df %>%
  select(maxdepth, area, SDI, ws_lake_arearatio, slope_max, lake_huc04) %>%
  inner_join(
    model_df %>%
      group_by(lake_huc04) %>%
      summarize(mean_zmax = mean(maxdepth),
                mean_area = mean(area)) %>%
      mutate(rel_depth = (mean_zmax * sqrt(pi)) / (20 * sqrt(mean_area))),
    by="lake_huc04"
  ) %>%
  select(-mean_zmax, -mean_area) %>%
  # do the variable transformations
  mutate(
    across(
      .cols = c(maxdepth, area, SDI, ws_lake_arearatio, slope_max), log
    )
  ) %>%
  # roll all the n < 10 groups into one group
  group_by(lake_huc04) %>%
  mutate(n = n(),
         model_group = ifelse(n < 10, "default", lake_huc04))

# Our numbers are quite close to what they report, except for area,
# which is much larger.
oliver_df %>%
  ungroup() %>%
  select(-lake_huc04, -model_group, -maxdepth, -n) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value))

oliver_scale <- oliver_df %>%
  ungroup() %>%
    mutate(
      across(
        .cols = c(-lake_huc04, -model_group, -n), scale
      )
    ) 

oliver_train <- oliver_scale[ trainInd, ]
oliver_valid <- oliver_scale[-trainInd, ]

# Now run the model
# For info on the mixed modeling syntax, see:
# http://www.modernstatisticswithr.com/regression.html#mixedmodels
oliver_lmer <- oliver_train %>%
  # The regional covariates are only used to model the observation-level slopes.
  # We only care about predictive power, so we only use the observation-level
  # covariates.
  lmer(maxdepth ~ (1 + area + SDI + ws_lake_arearatio + slope_max|model_group),
       data=.)

zmax_mean <- mean(oliver_df$maxdepth)
zmax_sd   <- sd(oliver_df$maxdepth)

# back calculate the original depths out of scaled log space
oliver_fortify <- fortify.merMod(oliver_lmer) %>%
  mutate(
    across(
      .cols=c(maxdepth, .fitted),
      # this matches the original distribution exactly
      ~exp((.x * zmax_sd) + zmax_mean)
    )
  )

# training stats match the paper pretty closely
cor(oliver_fortify$maxdepth, oliver_fortify$.fitted) ^ 2
sqrt(mean((oliver_fortify$maxdepth - oliver_fortify$.fitted) ^ 2))

# What about validation stats? Note the validation data was transformed at
# the same time, so we just call predict and then back-calculate
oliver_valid$.fitted <- predict(oliver_lmer, newdata=oliver_valid)
oliver_valid <- oliver_valid %>%
  mutate(
    across(
      .cols=c(maxdepth, .fitted),
      # this matches the original distribution exactly
      ~exp((.x * zmax_sd) + zmax_mean)
    )
  )

# validation stats are a little worse - but not overtly terrible
cor(oliver_valid$maxdepth, oliver_valid$.fitted) ^ 2
sqrt(mean((oliver_valid$maxdepth - oliver_valid$.fitted) ^ 2))

# plot time - compare LME and RF
oliver_valid %>%
  select(.fitted, maxdepth) %>%
  rename(pred_maxdepth = .fitted) %>%
  mutate(model="Mixed Effects") %>%
  bind_rows(
    validdf %>%
      select(maxdepth, rf_pred_maxdepth2) %>%
      rename(pred_maxdepth = rf_pred_maxdepth2) %>%
      mutate(model = "20-Variable Random Forest")
  ) %>%
  ggplot(aes(x=maxdepth, y=pred_maxdepth)) +
  geom_point() + 
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed", size=2) +
  coord_equal() + xlim(0, 50) +
  facet_wrap(~ model) +
  labs(x="True Depth (m)", y="Predicted Depth (m)")

# Bootstrapping is necessary to determine p-values in the mixed/RF models. If we
# permute the dependent variable (thus destroying any of the relationship the
# model is ostensibly finding), how good is the model?
load("data_working/simulated_rf_performance")
# sim_form <- str_c("fake_zmax ~ ", str_c(varImp_top$variable, collapse="+")) %>%
#   as.formula()
# 
ncores <- detectCores()
cl <- makeCluster(2)
registerDoParallel(cl)

# this takes a loooooooooong time
sim_r2 <- foreach(i=1:100, .combine=c, .packages=c("ranger")) %dopar% {
  traindf$fake_zmax <- sample(traindf$zmaxobs)
  rf <- ranger(sim_form, data=traindf)
  c(rf$r.squared)
}

stopCluster(cl)

# Now do the mixed effects one
cl <- makeCluster(2)
registerDoParallel(cl)

sim_r2_lmer <- foreach(i=1:100, .combine=c, .packages=c("lme4")) %do% {
  oliver_train$fake_zmax <- sample(oliver_train$zmaxobs)
  oliver_lmer <- lmer(
    fake_zmax ~ (1 + area + SDI + ws_lake_arearatio + slope_max|model_group),
    data=oliver_train
  )
  # grab the conditional R2 since the whole model is rand effects
  c(MuMIn::r.squaredGLMM(oliver_lmer)[1, 2])
}

stopCluster(cl)

ggplot(NULL, aes(x=sim_r2_lmer)) + geom_histogram()

n <- 25
x <- runif(n, max=n)
y <- 2 * x + runif(n, max=150)

ggplot(NULL, aes(x=x, y=y)) + geom_point(size=2) +
  geom_smooth(method="lm", se=F, color="red", size=2)

s <- lm(y ~ x) %>% summary()
r2_cutoff <- s$adj.r.squared
pval <- s$coefficients[2, 4]

n_better <- 0
cumulative_prop_better <- c()
for (i in 1:10000) {
  if (i %% 1000 == 0) {print(i)}
  y_fake <- sample(y)
  new_r2 <- (lm(y_fake ~ x) %>% summary())$adj.r.squared
  if (new_r2 > r2_cutoff) {
    n_better <- n_better + 1
  }
  cumulative_prop_better <- c(cumulative_prop_better, n_better / i)
}

ggplot(NULL, aes(x=1:10000, y=cumulative_prop_better)) + 
  geom_hline(yintercept=pval, linetype="dashed", color="red", size=2) +
  geom_line(size=2) + 
  annotate("text", 8000, 0.32, label="Original p-value = 0.3", color="red",
           size=5) +
  labs(y="Estimated p-value", x="Number of Runs") +
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=15))

load("data_working/simulated_lme_performance")
load("data_working/simulated_rf_performance")

lme_r2 <- 0.28
rf_r2  <- 0.34

type <- c(rep("Random Forest", 1000), rep("Mixed Effects", 100))

ggplot(NULL, aes(x=c(sim_r2, sim_r2_lmer))) + 
  geom_density_ridges(aes(y=type)) + 
  geom_vline(xintercept=lme_r2, color="red", linetype="dashed") +
  geom_vline(xintercept=rf_r2, color="blue", linetype="dashed")

traindf$rf_pred_maxdepth <- predict(test_rf2, traindf)$predictions

traindf %>%
  select(NLAID, rf_pred_maxdepth) %>%
  write_csv("data_working/NLA_rf_predictions.csv")

lagos_depths <- read_csv("data_working/lagos_ne_depths.csv") %>%
  rename(maxdepth = zmaxobs) %>%
  select(-lagoslakei) %>%
  mutate(group="NLA") %>%
  bind_rows(
    model_df %>% select(maxdepth) %>% mutate(group="LAGOS NE")
  )

lagos_depths %>%
  group_by(group) %>%
  summarize(ssq = sum((maxdepth - mean(maxdepth))^2),
            sd  = sd(maxdepth))

lagos_depths %>%
  ggplot(aes(x=maxdepth, y=group)) + geom_density_ridges() +
  xlim(0, 50) +
  labs(x="True Depth (m)", y="") +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18)) 
