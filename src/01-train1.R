##################
# Survey Motion
# Part I: Build Prediction Models
# Data: Lab experiment
# Christoph Kern
# R 3.4.4
##################

## 01: Setup

set.seed(9253)

library(tidyverse)
library(matrixStats)
library(caret)
library(glmnet)
library(party)
library(partykit)
library(ranger)
library(xgboost)

setwd("/home/ckern/Uni/Forschung/Article/2019 - MASS")
load("./data/G_Train.RData")

# y

table(SM_Final$Group)

SM_Final$G_group <- NA
SM_Final$G_group <- factor(SM_Final$G_group, levels = c("Sitting", "Standing", "Walking", "Climbing"))
SM_Final$G_group[SM_Final$Group == 1] <- "Sitting"
SM_Final$G_group[SM_Final$Group == 2] <- "Standing"
SM_Final$G_group[SM_Final$Group == 3] <- "Walking"
SM_Final$G_group[SM_Final$Group == 4] <- "Climbing"
table(SM_Final$G_group, useNA = "always")

SM_Final$D_group <- NA
SM_Final$D_group <- factor(SM_Final$D_group, levels = c("Not_Moving", "Moving"))
SM_Final$D_group[SM_Final$Group == 1 | SM_Final$Group == 2] <- "Not_Moving"
SM_Final$D_group[SM_Final$Group == 3 | SM_Final$Group == 4] <- "Moving"
table(SM_Final$D_group, useNA = "always")

# X

# nearZeroVar(SM_Final)

motionvars <- grep("SM_" , names(SM_Final), value = TRUE)

SM_Final <- arrange(SM_Final, ID, page)

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

sm1 <- select(sm, G_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)
sm2 <- select(sm, D_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)
sm3 <- select(sm, motionvars[5:50], G_group, D_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)

## 02: Tuning Setup

# k = length(unique(sm$ID))
folds <- groupKFold(sm$ID, k = 10)

multiStats <- function(...) c(multiClassSummary(...), 
                              mnLogLoss(...))

evalStats <- function(...) c(twoClassSummary(...), 
                             defaultSummary(...),
                             mnLogLoss(...))

ctrl1  <- trainControl(method = "cv",
                       index = folds,
                       number = 10,
                       summaryFunction = multiStats,
                       classProbs = TRUE,
                       verboseIter = TRUE)

ctrl2  <- trainControl(method = "cv",
                       index = folds,
                       number = 10,
                       summaryFunction = evalStats,
                       classProbs = TRUE,
                       verboseIter = TRUE)

## 03: glmnet

grid <- expand.grid(alpha = c(0, 0.5, 1),
                    lambda = seq(0.1, 0, length = 30))

set.seed(74684)
glmnet_l1 <- train(G_group  ~ .,
                   data = sm1,
                   method = "glmnet",
                   family = "multinomial",
                   #type.multinomial = "grouped",
                   trControl = ctrl1,
                   tuneGrid = grid,
                   metric = "logLoss")

glmnet_l1
plot(glmnet_l1)
confusionMatrix(glmnet_l1)
plot(varImp(glmnet_l1))

set.seed(74684)
glmnet_l2 <- train(D_group  ~ .,
                   data = sm2,
                   method = "glmnet",
                   family = "binomial",
                   trControl = ctrl2,
                   tuneGrid = grid,
                   metric = "logLoss")

glmnet_l2
plot(glmnet_l2)
confusionMatrix(glmnet_l2)
plot(varImp(glmnet_l2))

set.seed(74684)
glmnet_l3 <- train(D_group  ~ . - G_group,
                   data = sm3,
                   method = "glmnet",
                   family = "binomial",
                   trControl = ctrl2,
                   tuneGrid = grid,
                   metric = "logLoss")

plot(glmnet_l3)
plot(varImp(glmnet_l3), top = 20)

## 04: CTREE

grid <- expand.grid(mincriterion = c(0.99, 0.95, 0.90, 0.85, 0.75))

set.seed(74684)
ctree_l1 <- train(G_group  ~ .,
                  data = sm1,
                  method = "ctree",
                  trControl = ctrl1,
                  tuneGrid = grid,
                  metric = "logLoss")

ctree_l1
plot(ctree_l1)
confusionMatrix(ctree_l1)
plot(ctree_l1$finalModel)

set.seed(74684)
ctree_l2 <- train(D_group  ~ .,
                  data = sm2,
                  method = "ctree",
                  trControl = ctrl2,
                  tuneGrid = grid,
                  metric = "logLoss")

ctree_l2
plot(ctree_l2)
confusionMatrix(ctree_l2)
plot(ctree_l2$finalModel)

set.seed(74684)
ctree_l3 <- train(D_group  ~ . - G_group,
                  data = sm3,
                  method = "ctree",
                  trControl = ctrl2,
                  tuneGrid = grid,
                  metric = "logLoss")

plot(ctree_l3)
plot(ctree_l3$finalModel)

## 05: Random Forest and Extra Trees

cols <- ncol(model.matrix(G_group  ~ ., data = sm1)[,-1])
grid <- expand.grid(mtry = c(round(sqrt(cols)), round(log(cols))),
                    splitrule = c("gini", "extratrees"),
                    min.node.size = 15)

set.seed(74684)
rf_l1 <- train(G_group  ~ .,
               data = sm1,
               method = "ranger",
               trControl = ctrl1,
               tuneGrid = grid,
               metric = "logLoss",
               importance = "impurity")

rf_l1
plot(rf_l1)
confusionMatrix(rf_l1)
plot(varImp(rf_l1), top = 20)

set.seed(74684)
rf_l2 <- train(D_group  ~ .,
               data = sm2,
               method = "ranger",
               trControl = ctrl2,
               tuneGrid = grid,
               metric = "logLoss",
               importance = "impurity")

rf_l2
plot(rf_l2)
confusionMatrix(rf_l2)
plot(varImp(rf_l2), top = 20)

cols <- ncol(model.matrix(G_group  ~ . - D_group, data = sm3)[,-1])
grid <- expand.grid(mtry = c(round(sqrt(cols)), round(log(cols))),
                    splitrule = c("gini", "extratrees"),
                    min.node.size = 15)

set.seed(74684)
rf_l3 <- train(D_group  ~ . - G_group,
               data = sm3,
               method = "ranger",
               trControl = ctrl2,
               tuneGrid = grid,
               metric = "logLoss",
               importance = "impurity")

plot(rf_l3)
plot(varImp(rf_l3), top = 20)

## 06: Boosting

grid <- expand.grid(max_depth = c(3, 5, 7, 9),
                    nrounds = c(500, 1000, 1500),
                    eta = c(0.01, 0.05),
                    min_child_weight = 5,
                    subsample = 0.7,
                    gamma = 0,
                    colsample_bytree = 1)

set.seed(74684)
xgb_l1 <- train(G_group  ~ .,
                data = sm1,
                method = "xgbTree",
                trControl = ctrl1,
                tuneGrid = grid,
                metric = "logLoss")

xgb_l1
plot(xgb_l1)
confusionMatrix(xgb_l1)
plot(varImp(xgb_l1))

set.seed(74684)
xgb_l2 <- train(D_group  ~ .,
                data = sm2,
                method = "xgbTree",
                trControl = ctrl2,
                tuneGrid = grid,
                metric = "logLoss")

xgb_l2
plot(xgb_l2)
confusionMatrix(xgb_l2)
plot(varImp(xgb_l2))

set.seed(74684)
xgb_l3 <- train(D_group  ~ . - G_group,
                data = sm3,
                method = "xgbTree",
                trControl = ctrl2,
                tuneGrid = grid,
                metric = "logLoss")

plot(xgb_l3)
plot(varImp(xgb_l3), top = 20)

## 06: Comparison

## Prediction in training set (CV)

resamps_l1 <- resamples(list(GLMnet = glmnet_l1,
                             CTREE = ctree_l1,
                             RF = rf_l1,
                             XGBoost = xgb_l1))

resamps_l1
summary(resamps_l1)
bwplot(resamps_l1)

resamp_l1 <- 
  reshape(resamps_l1$values,
          direction = "long",
          varying = 2:ncol(resamps_l1$values),
          sep = "~",
          v.names = c("Accuracy", "AUC", "Kappa", "logLoss", "logLoss.1", "Mean_Balanced_Accuracy", 
                      "Mean_Detection_Rate", "Mean_F1", "Mean_Neg_Pred_Value", "Mean_Pos_Pred_Value", 
                      "Mean_Precision", "Mean_Recall", "Mean_Sensitivity", "Mean_Specificity", "prAUC"),
          timevar = "model")

resamp_l1 <- 
  resamp_l1 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "GLMnet" = "1",
                            "CTREE" = "2",
                            "RF" = "3",
                            "XGBoost" = "4"))

resamps_l2 <- resamples(list(GLMnet = glmnet_l2,
                             CTREE = ctree_l2,
                             RF = rf_l2,
                             XGBoost = xgb_l2))

resamps_l2
summary(resamps_l2)
bwplot(resamps_l2)

resamp_l2 <- 
  reshape(resamps_l2$values,
          direction = "long",
          varying = 2:ncol(resamps_l2$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp_l2 <- 
  resamp_l2 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "GLMnet" = "1",
                            "CTREE" = "2",
                            "RF" = "3",
                            "XGBoost" = "4"))

resamps_l3 <- resamples(list(GLMnet = glmnet_l3,
                             CTREE = ctree_l3,
                             RF = rf_l3,
                             XGBoost = xgb_l3))

resamps_l3
summary(resamps_l3)
bwplot(resamps_l3)

resamp_l3 <- 
  reshape(resamps_l3$values,
          direction = "long",
          varying = 2:ncol(resamps_l3$values),
          sep = "~",
          v.names = c("Accuracy", "Kappa", "logLoss", "ROC", "Sens", "Spec"),
          timevar = "model")

resamp_l3 <- 
  resamp_l3 %>%
  mutate(model = factor(model)) %>%
  mutate(model = fct_recode(model,
                            "GLMnet" = "1",
                            "CTREE" = "2",
                            "RF" = "3",
                            "XGBoost" = "4"))

save(sm, resamp_l1, resamp_l2, resamp_l3, 
     glmnet_l1, glmnet_l2, glmnet_l3, 
     ctree_l1, ctree_l2, ctree_l3,
     rf_l1, rf_l2, rf_l3,
     xgb_l1, xgb_l2, xgb_l3,
     file = "./src/output1.Rdata")
