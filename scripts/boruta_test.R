library(tidyverse)
library(Boruta)
library(ranger)

mglp_model_df <- read_csv("data_out/mglp_lagos_shape.csv") %>%
  inner_join(
    read_csv("data_out/mglp_lagos_terrain.csv"),
    by="lake_nhdid"
  ) %>%
  # inner_join(
  #   read_csv("data_out/mglp_lagos_network.csv"),
  #   by="lake_nhdid"
  # ) %>%
  inner_join(
    read_csv("data_out/mglp_lagos_geography.csv"),
    by="lake_nhdid"
  ) %>%
  inner_join(
    read_csv("data_in/mglp/MGLP_max_depth_subset_211008.csv") %>%
      mutate(site_id=str_sub(site_id, 7)),
    by=c("lake_nhdid"="site_id")
  ) %>%
  select(
    -contains("lagoslakeid"), -contains("dam"), 
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

apply(mglp_model_df, 2, function(x) sum(is.na(x)) / length(x))

# This is slow, run the formula below
mglp_boruta <- Boruta(lake_depth ~ ., data=mglp_model_df, doTrace=2)
confirmed <- names(
  mglp_boruta$finalDecision[mglp_boruta$finalDecision == "Confirmed"]
)
boruta_formula <- str_c("lake_depth", "~", str_c(confirmed, collapse="+")) %>%
  as.formula()

trainInd <- sample(1:nrow(mglp_model_df), round(nrow(mglp_model_df) * 0.8))

traindf <- mglp_model_df[ trainInd, ]
validdf <- mglp_model_df[-trainInd, ]

test_rf <- ranger(
  boruta_formula,
  data=traindf,
  importance="permutation"
)

validdf$pred_maxdepth <- predict(test_rf, validdf)$predictions

ggplot(validdf, aes(x=lake_depth, y=pred_maxdepth)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")

# Calc R2 and RMSE
cor(validdf$pred_maxdepth, validdf$lake_depth) ^ 2
sqrt(mean((validdf$pred_maxdepth - validdf$lake_depth) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(lake_depth ~ pred_maxdepth, data=validdf)
summary(valid_lm) # m = 1.12, so prediction is slightly underestimating

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

form <- str_c("lake_depth ~ ", str_c(varImp_top$variable, collapse="+")) %>%
  as.formula()

test_rf2 <- ranger(
  form,
  data=traindf,
  importance="permutation"
)

validdf$pred_maxdepth2 <- predict(test_rf2, validdf)$predictions

# Calc R2 and RMSE
cor(validdf$pred_maxdepth2, validdf$lake_depth) ^ 2
sqrt(mean((validdf$pred_maxdepth2 - validdf$lake_depth) ^ 2))

# Calc slope bt predicted and actual
valid_lm <- lm(lake_depth ~ pred_maxdepth2, data=validdf)
summary(valid_lm) # m = 1.12, so prediction is slightly underestimating

ggplot(validdf, aes(x=lake_depth, y=pred_maxdepth2)) + geom_point() +
  geom_abline(size=2, slope=1, intercept=0, color="red", linetype="dashed") +
  coord_equal() +
  labs(x="True Depth (m)", y="Predicted Depth (m)")
