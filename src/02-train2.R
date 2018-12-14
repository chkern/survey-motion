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

# setwd("/home/ckern/Uni/Forschung/Article/2019 - MASS")
load("../data/Netquest_Train.RData")

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

sm3 <- select(sm, motionvars[5:50], G_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)

sm4 <- select(sm, motionvars[5:50], D_group, SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_min, SM_max, SM_r, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95)

## 02: Tuning Setup

# k = length(unique(sm$ID))

folds <- groupKFold(sm$ID, k = 10)

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl1  <- trainControl(method = "cv",
                       index = folds,
                       number = 10,
                       summaryFunction = multiClassSummary,
                       classProbs = TRUE,
                       verboseIter = TRUE)

ctrl2  <- trainControl(method = "cv",
                       index = folds,
                       number = 10,
                       summaryFunction = fiveStats,
                       classProbs = TRUE,
                       verboseIter = TRUE)

## 03: glmnet

grid <- expand.grid(alpha = c(0, 0.5, 1),
                    lambda = seq(0.1, 0, length = 30))

set.seed(74684)
glmnet1 <- train(G_group  ~ .,
                data = sm1,
                method = "glmnet",
                family = "multinomial",
                #type.multinomial = "grouped",
                trControl = ctrl1,
                tuneGrid = grid,
                metric = "AUC")

glmnet1
plot(glmnet1)
confusionMatrix(glmnet1)
plot(varImp(glmnet1))

set.seed(74684)
glmnet2 <- train(D_group  ~ .,
                data = sm2,
                method = "glmnet",
                family = "binomial",
                trControl = ctrl2,
                tuneGrid = grid,
                metric = "ROC")

glmnet2
plot(glmnet2)
confusionMatrix(glmnet2)
plot(varImp(glmnet2))

set.seed(74684)
glmnet3 <- train(D_group  ~ .,
                 data = sm4,
                 method = "glmnet",
                 family = "binomial",
                 trControl = ctrl2,
                 tuneGrid = grid,
                 metric = "ROC")

plot(glmnet3)
plot(varImp(glmnet3), top = 20)

## 04: CTREE

grid <- expand.grid(mincriterion = c(0.99, 0.95, 0.90, 0.85, 0.75))

set.seed(74684)
ctree1 <- train(G_group  ~ .,
             data = sm1,
             method = "ctree",
             trControl = ctrl1,
             tuneGrid = grid,
             metric = "AUC")

ctree1
plot(ctree1)
confusionMatrix(ctree1)
plot(ctree1$finalModel)

set.seed(74684)
ctree2 <- train(D_group  ~ .,
             data = sm2,
             method = "ctree",
             trControl = ctrl2,
             tuneGrid = grid,
             metric = "ROC")

ctree2
plot(ctree2)
confusionMatrix(ctree2)
plot(ctree2$finalModel)

set.seed(74684)
ctree3 <- train(D_group  ~ .,
                data = sm4,
                method = "ctree",
                trControl = ctrl2,
                tuneGrid = grid,
                metric = "ROC")

plot(ctree3)
plot(ctree3$finalModel)

## 05: Random Forest

grid <- expand.grid(mtry = 1:14)

set.seed(74684)
rf1 <- train(G_group  ~ .,
             data = sm1,
             method = "rf",
             trControl = ctrl1,
             tuneGrid = grid,
             metric = "AUC")

rf1
plot(rf1)
confusionMatrix(rf1)
plot(varImp(rf1))

set.seed(74684)
rf2 <- train(D_group  ~ .,
             data = sm2,
             method = "rf",
             trControl = ctrl2,
             tuneGrid = grid,
             metric = "ROC")

rf2
plot(rf2)
confusionMatrix(rf2)
plot(varImp(rf2))

grid <- expand.grid(mtry = 10*1:6)

set.seed(74684)
rf3 <- train(D_group  ~ .,
             data = sm4,
             method = "rf",
             trControl = ctrl2,
             tuneGrid = grid,
             metric = "ROC")

plot(rf3)
plot(varImp(rf3), top = 20)

## 06: Boosting

grid <- expand.grid(max_depth = c(3, 5, 7, 9),
                    nrounds = c(500, 1000, 1500),
                    eta = c(0.01, 0.05),
                    min_child_weight = 5,
                    subsample = 0.7,
                    gamma = 0,
                    colsample_bytree = 1)

set.seed(74684)
xgb1 <- train(G_group  ~ .,
              data = sm1,
              method = "xgbTree",
              trControl = ctrl1,
              tuneGrid = grid,
              metric = "AUC")

xgb1
plot(xgb1)
confusionMatrix(xgb1)
plot(varImp(xgb1))

set.seed(74684)
xgb2 <- train(D_group  ~ .,
              data = sm2,
              method = "xgbTree",
              trControl = ctrl2,
              tuneGrid = grid,
              metric = "ROC")

xgb2
plot(xgb2)
confusionMatrix(xgb2)
plot(varImp(xgb2))

set.seed(74684)
xgb3 <- train(D_group  ~ .,
              data = sm4,
              method = "xgbTree",
              trControl = ctrl2,
              tuneGrid = grid,
              metric = "ROC")

plot(xgb3)
plot(varImp(xgb3), top = 20)

## 06: Comparison

## Prediction in training set (CV)

resamps1 <- resamples(list(glmnet = glmnet1,
                           ctree = ctree1,
                           rf = rf1,
                           xgb = xgb1))

resamps1
summary(resamps1)
bwplot(resamps1)

resamps2 <- resamples(list(glmnet = glmnet2,
                           ctree = ctree2,
                           rf = rf2,
                           xgb = xgb2))

resamps2
summary(resamps2)
bwplot(resamps2)

resamps3 <- resamples(list(glmnet = glmnet3,
                           ctree = ctree3,
                           rf = rf3,
                           xgb = xgb3))

resamps3
summary(resamps3)
bwplot(resamps3)

# save(glmnet1, glmnet2, ctree1, ctree2, rf1, rf2, xgb1, xgb2, file = "output2.Rdata")
