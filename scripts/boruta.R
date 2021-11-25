library(tidyverse)
library(Boruta)
library(ranger)
library(foreach)
library(doParallel)

model_df <- read_csv("data_out/lagos_ne_shape.csv") %>%
  inner_join(
    read_csv("data_out/lagos_ne_terrain.csv"),
    by="lagoslakei"
  ) %>%
  # paucity of data, leave it out
  # inner_join(
  #   read_csv("data_out/mglp_lagos_network.csv"),
  #   by="lake_nhdid"
  # ) %>%
  inner_join(
    read_csv("data_out/lagos_ne_geography.csv"),
    by=c("lagoslakei"="lagoslakeid")
  ) %>%
  inner_join(
    read_csv("data_working/lagos_ne_depths.csv") %>%
      select(lagoslakei, zmaxobs),
    by="lagoslakei"
  ) %>%
  select(
    -contains("lagoslakei"), -contains("dam"), #-OBJECTID 
     #-nhdplusv2_comid,
     #-lake_nets_upstreamlake_km, -lake_nets_downstreamlake_km,
     #net_dams_n, lake_nets_damonlake_flag
  ) %>%
  mutate(
    lake_huc02 = factor(str_sub(lake_huc12, 1, 2)),
    lake_huc04 = factor(str_sub(lake_huc12, 1, 4)),
    lake_huc06 = factor(str_sub(lake_huc12, 1 ,6)),
    #net_id = factor(net_id)
  ) %>%
  select(-lake_huc12)

apply(model_df, 2, function(x) sum(is.na(x)) / length(x))

# This is slow, run the formula below
# boruta <- Boruta(zmaxobs ~ ., data=model_df, doTrace=2)
# confirmed <- names(
#   boruta$finalDecision[boruta$finalDecision == "Confirmed"]
# )
# boruta_formula <- str_c("zmaxobs", "~", str_c(confirmed, collapse="+"), "- lagoslakei") %>%
#   as.formula()

trainInd <- sample(1:nrow(model_df), round(nrow(model_df) * 0.8))

traindf <- model_df[ trainInd, ]
validdf <- model_df[-trainInd, ]

test_rf <- ranger(
  zmaxobs ~ ., #boruta_formula,
  data=traindf,
  importance="permutation"
)

validdf$pred_maxdepth <- predict(test_rf, validdf)$predictions

ggplot(validdf, aes(x=zmaxobs, y=pred_maxdepth)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")

# Calc R2 and RMSE
cor(validdf$pred_maxdepth, validdf$zmaxobs) ^ 2
sqrt(mean((validdf$pred_maxdepth - validdf$zmaxobs) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(zmaxobs ~ pred_maxdepth, data=validdf)
summary(valid_lm) # slope = 1.12, so prediction is slightly underestimating

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

form <- str_c("zmaxobs ~ ", str_c(varImp_top$variable, collapse="+")) %>%
  as.formula()

test_rf2 <- ranger(
  form,
  data=traindf,
  importance="permutation"
)

validdf$pred_maxdepth2 <- predict(test_rf2, validdf)$predictions

# Calc R2 and RMSE
cor(validdf$pred_maxdepth2, validdf$zmaxobs) ^ 2
sqrt(mean((validdf$pred_maxdepth2 - validdf$zmaxobs) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(zmaxobs ~ pred_maxdepth2, data=validdf)
summary(valid_lm) # m = 1.12, so prediction is slightly underestimating

ggplot(validdf, aes(x=zmaxobs, y=pred_maxdepth2)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")


# run Sobek, Heathcote, and Hollister as comparison
model_df$linear_term <- model_df$dist_pole * model_df$slope_median
hollister <- lm(zmaxobs ~ linear_term, 
                data=model_df %>% mutate(linear_term = dist_pole * slope_median))

sobek <- lm(zmaxobs ~ logarea + slope_max,
            data = model_df %>% mutate(logarea = log(area)))

# Really if I'm doing this perfectly it should be lake elevation.
# BUT, min within the buffer is close enough
heathcote <- lm(zmaxobs ~ log_elev_change,
                data=model_df %>%
                  filter(zmaxobs > 0, elev_median > elev_min) %>%
                  mutate(log_maxdepth_m = log(zmaxobs),
                         log_elev_change = log(elev_median - elev_min)))

# Do a simulation analysis to determine the p-value of the RF model. If we
# permute the dependent variable (thus destroying any of the relationship the
# RF is ostensibly finding), how good is the model?
sim_form <- str_c("fake_zmax ~ ", str_c(varImp_top$variable, collapse="+")) %>%
  as.formula()

ncores <- detectCores()
cl <- makeCluster(2)
registerDoParallel(cl)

# this takes a loooooooooong time
sim_r2 <- foreach(i=1:10000, .combine=c, .packages=c("ranger")) %dopar% {
  traindf$fake_zmax <- sample(traindf$zmaxobs)
  rf <- ranger(sim_form, data=traindf)
  rf$r.squared
}

stopCluster(cl)
