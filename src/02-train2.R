##################
# Survey Motion
# Part I: Build Prediction Models
# Data: Field experiment
# Christoph Kern
# R 3.4.4
##################

## 01: Setup

set.seed(2748)

library(tidyverse)
library(matrixStats)
library(caret)
library(glmnet)
library(party)
library(partykit)
library(randomForest)
library(xgboost)

setwd("E:\\Uni\\Forschung\\Article\\2019 - MASS")
load("./data/Netquest_Train.RData")

# Wide to long

Out1 <- select(Out, 1:14, 15, 20:24, 16, 25:29, 17, 30:34, 18, 35:39, 19, 40:44)

SM_Final1 <- reshape(Out1, direction = 'long', 
                     varying = 15:44, 
                     timevar = 'var',
                     times = c('SE1', 'SE2', 'SE3', 'SE4', 'SE5'),
                     v.names = c('SM', 'SF_OFF', 'SF_OFF_Count', 'SF_OFF_Time', 'SF_ON_Time', 'Scroll_Time'),
                     idvar = 'ID')

names1 <- c("SM.SE1_SM_G.304", "SM.SE4_SM_G.483")
names2 <- paste0(rep("SM.SE1_SM.",178),305:483)
names3 <- paste0(rep("SM.SE1_SM_G.",178),305:483)
names4 <- paste0(rep("SM.SE2_SM.",300),183:483)
names5 <- paste0(rep("SM.SE2_SM_G.",300),183:483)
names6 <- paste0(rep("SM.SE3_SM.",77),406:483)
names7 <- paste0(rep("SM.SE3_SM_G.",77),406:483)
names8 <- paste0(rep("SM.SE5_SM.",194),289:483)
names9 <- paste0(rep("SM.SE5_SM_G.",194),289:483)

Out[, c(names1, names2, names3, names4, names5, names6, names7, names8, names9)] <- NA

Out2 <- select(Out, ID, 
               starts_with("SM.SE1_SM."), starts_with("SM.SE1_SM_G."),
               starts_with("SM.SE2_SM."), starts_with("SM.SE2_SM_G."),
               starts_with("SM.SE3_SM."), starts_with("SM.SE3_SM_G."),
               starts_with("SM.SE4_SM."), starts_with("SM.SE4_SM_G."),
               starts_with("SM.SE5_SM."), starts_with("SM.SE5_SM_G."))

names1 <- paste0(rep("SM", 483), 1:483, rep(".SE1", 483))
names2 <- paste0(rep("SM_G", 483), 1:483, rep(".SE1", 483))
names3 <- paste0(rep("SM", 483), 1:483, rep(".SE2", 483))
names4 <- paste0(rep("SM_G", 483), 1:483, rep(".SE2", 483))
names5 <- paste0(rep("SM", 483), 1:483, rep(".SE3", 483))
names6 <- paste0(rep("SM_G", 483), 1:483, rep(".SE3", 483))
names7 <- paste0(rep("SM", 483), 1:483, rep(".SE4", 483))
names8 <- paste0(rep("SM_G", 483), 1:483, rep(".SE4", 483))
names9 <- paste0(rep("SM", 483), 1:483, rep(".SE5", 483))
names10 <- paste0(rep("SM_G", 483), 1:483, rep(".SE5", 483))
colnames(Out2) <- c("ID", names1, names2, names3, names4, names5, names6, names7, names8, names9, names10)

SM_Final2 <- reshape(Out2, direction = 'long', 
                     varying = 2:4831, 
                     timevar = 'var',
                     sep = ".",
                     idvar = 'ID')

SM_Final <- merge(SM_Final1, SM_Final2, by = c("ID", "var"))

# y

table(SM_Final$SM_CONDITION)

SM_Final$D_group <- NA
SM_Final$D_group <- factor(SM_Final$D_group, levels = c("Not_Moving", "Moving"))
SM_Final$D_group[SM_Final$SM_CONDITION == 1] <- "Not_Moving"
SM_Final$D_group[SM_Final$SM_CONDITION == 2] <- "Moving"
table(SM_Final$D_group, useNA = "always")

# X

# nearZeroVar(SM_Final)

motionvars <- grep("SM[123456789]" , names(SM_Final), value = TRUE)

SM_Final <- 
  SM_Final %>%
  arrange(ID, var) %>%
  filter(!is.na(SM1))

sm <-
  SM_Final %>%
  select(motionvars) %>%
  mutate(SM_mean = rowMeans2(as.matrix(.), na.rm = T)) %>%
  mutate(SM_med = rowMedians(as.matrix(.), na.rm = T)) %>%
  mutate(SM_var = rowVars(as.matrix(.), na.rm = T)) %>%
  mutate(SM_mad = rowMads(as.matrix(.), na.rm = T)) %>%
  mutate(SM_iqr = rowIQRs(as.matrix(.), na.rm = T)) %>%
  mutate(SM_min = rowMins(as.matrix(.), na.rm = T)) %>%
  mutate(SM_max = rowMaxs(as.matrix(.), na.rm = T)) %>%
  mutate(SM_r = SM_max - SM_min) %>%
  mutate(SM_q5 = rowQuantiles(as.matrix(.), probs = 0.05, na.rm = T)) %>%
  mutate(SM_q10 = rowQuantiles(as.matrix(.), probs = 0.10, na.rm = T)) %>%
  mutate(SM_q25 = rowQuantiles(as.matrix(.), probs = 0.25, na.rm = T)) %>%
  mutate(SM_q75 = rowQuantiles(as.matrix(.), probs = 0.75, na.rm = T)) %>%
  mutate(SM_q9 = rowQuantiles(as.matrix(.), probs = 0.9, na.rm = T)) %>%
  mutate(SM_q95 = rowQuantiles(as.matrix(.), probs = 0.95, na.rm = T)) %>%
  select(SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95) %>%
  bind_cols(SM_Final, .)

sm1 <- select(sm, D_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)

## 02: Tuning Setup

# k = length(unique(sm$ID))

folds <- groupKFold(sm$ID, k = 10)

evalStats <- function(...) c(twoClassSummary(...), 
                             defaultSummary(...),
                             mnLogLoss(...))

ctrl  <- trainControl(method = "cv",
                      index = folds,
                      number = 10,
                      summaryFunction = evalStats,
                      classProbs = TRUE,
                      verboseIter = TRUE)

## 03: glmnet

grid <- expand.grid(alpha = c(0, 0.5, 1),
                    lambda = seq(0.1, 0, length = 30))

set.seed(83357)
glmnet_f <- train(D_group  ~ .,
                  data = sm1,
                  method = "glmnet",
                  family = "binomial",
                  trControl = ctrl,
                  tuneGrid = grid,
                  metric = "logLoss")

glmnet_f
plot(glmnet_f)
confusionMatrix(glmnet_f)
plot(varImp(glmnet_f))

## 04: CTREE

grid <- expand.grid(mincriterion = c(0.99, 0.95, 0.90, 0.85, 0.75))

set.seed(83357)
ctree_f <- train(D_group  ~ .,
                 data = sm1,
                 method = "ctree",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric = "logLoss")

ctree_f
plot(ctree_f)
confusionMatrix(ctree_f)
plot(ctree_f$finalModel)

## 05: Random Forest and Extra Trees

cols <- ncol(model.matrix(D_group  ~ ., data = sm1)[,-1])
grid <- expand.grid(mtry = c(round(sqrt(cols)), round(log(cols))),
                    splitrule = c("gini", "extratrees"),
                    min.node.size = 15)

set.seed(83357)
rf_f <- train(D_group  ~ .,
              data = sm1,
              method = "ranger",
              trControl = ctrl,
              tuneGrid = grid,
              metric = "logLoss",
              importance = "impurity")

rf_f
plot(rf_f)
confusionMatrix(rf_f)
plot(varImp(rf_f))

## 06: Boosting

grid <- expand.grid(max_depth = c(3, 5, 7, 9),
                    nrounds = c(500, 1000, 1500),
                    eta = c(0.01, 0.05),
                    min_child_weight = 5,
                    subsample = 0.7,
                    gamma = 0,
                    colsample_bytree = 1)

set.seed(83357)
xgb_f <- train(D_group  ~ .,
               data = sm1,
               method = "xgbTree",
               trControl = ctrl,
               tuneGrid = grid,
               metric = "logLoss")

xgb_f
plot(xgb_f)
confusionMatrix(xgb_f)
plot(varImp(xgb_f))

## 06: Comparison

## Prediction in training set (CV)

resamps_f <- resamples(list(GLMnet = glmnet_f,
                             CTREE = ctree_f,
                             RF = rf_f,
                             XGBoost = xgb_f))

resamps_f
summary(resamps_f)
bwplot(resamps_f)

resamp_f <- 
  reshape(resamps_f$values,
          direction = "long",
          varying = 2:ncol(resamps_f$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp_f <- 
  resamp_f %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "GLMnet" = "1",
                            "CTREE" = "2",
                            "RF" = "3",
                            "XGBoost" = "4"))

save(sm1, resamp_f,
     glmnet_f, 
     ctree_f,
     rf_f,
     xgb_f,
     file = "./src/output2.Rdata")
