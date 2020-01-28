##################
# Survey Motion
# Part I: Build Prediction Models (Lab experiment)
# Christoph Kern
# R 3.5.1
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
library(gridExtra)
library(xtable)
library(stargazer)

setwd("E:\\Uni\\Forschung\\Article\\2019 - MASS")
load("./data/Goe_Train.RData")

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

# Examples of TA profiles

rawmotion <- grep("SM_[0123456789]", names(sm), value = TRUE)
row.names(sm) <- paste0("SM_", row.names(sm))

set.seed(25986)

p1 <- sm %>%
  filter(Group == 1 & page == "Matrix") %>%
  sample_n(1) %>%
  select(rawmotion) %>%
  rownames_to_column %>%
  gather(var, value, -rowname) %>%
  mutate(var = as.numeric(gsub("SM_", "", var))) %>%
  filter(var < 2000) %>%
  ggplot() + 
  geom_line(aes(x = var, y = value, group = rowname), alpha = 0.75, size = 0.2) + 
  #  geom_smooth(aes(x = var, y = value), se = FALSE, color = "red", size = 0.25) +
  ylim(0, 5) +
  labs(x = "Time", y = "TA", title = "Sitting") +
  theme_light(base_size = 10) +
  theme(legend.position = "none")

p2 <- sm %>%
  filter(Group == 2 & page == "Matrix") %>%
  sample_n(1) %>%
  select(rawmotion) %>%
  rownames_to_column %>%
  gather(var, value, -rowname) %>%
  mutate(var = as.numeric(gsub("SM_", "", var))) %>%
  filter(var < 2000) %>%
  ggplot() + 
  geom_line(aes(x = var, y = value, group = rowname), alpha = 0.75, size = 0.2) + 
  #  geom_smooth(aes(x = var, y = value), se = FALSE, color = "red", size = 0.25) +
  ylim(0, 5) +
  labs(x = "Time", y = "TA", title = "Standing") +
  theme_light(base_size = 10) +
  theme(legend.position = "none")

p3 <- sm %>%
  filter(Group == 3 & page == "Matrix") %>%
  sample_n(1) %>%
  select(rawmotion) %>%
  rownames_to_column %>%
  gather(var, value, -rowname) %>%
  mutate(var = as.numeric(gsub("SM_", "", var))) %>%
  filter(var < 2000) %>%
  ggplot() + 
  geom_line(aes(x = var, y = value, group = rowname), alpha = 0.75, size = 0.2) + 
  #  geom_smooth(aes(x = var, y = value), se = FALSE, color = "red", size = 0.25) +
  ylim(0, 15) +
  labs(x = "Time", y = "TA", title = "Walking") +
  theme_light(base_size = 10) +
  theme(legend.position = "none")

p4 <- sm %>%
  filter(Group == 4 & page == "Matrix") %>%
  sample_n(1) %>%
  select(rawmotion) %>%
  rownames_to_column %>%
  gather(var, value, -rowname) %>%
  mutate(var = as.numeric(gsub("SM_", "", var))) %>%
  filter(var < 2000) %>%
  ggplot() + 
  geom_line(aes(x = var, y = value, group = rowname), alpha = 0.75, size = 0.2) + 
  #  geom_smooth(aes(x = var, y = value), se = FALSE, color = "red", size = 0.25) +
  ylim(0, 15) +
  labs(x = "Time", y = "TA", title = "Climbing") +
  theme_light(base_size = 10) +
  theme(legend.position = "none")

plots <- arrangeGrob(p1, p2, p3, p4, nrow = 2)
ggsave("p1_TA_examples.pdf", plots, width = 7.5, height = 6)

## 02: Tuning Setup

multiStats <- function(...) c(multiClassSummary(...), 
                              mnLogLoss(...))

evalStats <- function(...) c(twoClassSummary(...), 
                             defaultSummary(...),
                             mnLogLoss(...))

folds <- groupKFold(sm$ID, k = 10) # 10 fold CV

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

ntrain <- nrow(sm) # nested CV 
outer_train <- folds
outer_test <- lapply(outer_train, function(x) (1:ntrain)[-x])

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
plot(varImp(glmnet_l1))
tab <- xtable(confusionMatrix(glmnet_l1)$table, digits = 3)
print(tab, type = "latex", file = "t1_glmnet1.tex")

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
plot(varImp(glmnet_l2))
tab <- xtable(confusionMatrix(glmnet_l2)$table, digits = 3)
print(tab, type = "latex", file = "t1_glmnet2.tex")

glmnet_m <- list()
glmnet_p <- list()

set.seed(74684)
for (i in 1:10){ # nested CV
  glmnet_m[[i]] <- train(D_group ~ ., 
               data = sm2[outer_train[[i]],],
               method = "glmnet",
               family = "binomial",
               tuneGrid = grid,
               metric = "logLoss",
               trControl = trainControl(method = "cv", 
                                        number = 10,
                                        summaryFunction = evalStats,
                                        classProbs = TRUE))
  glmnet_p[[i]] <- cbind(predict(glmnet_m[[i]], newdata = sm2[outer_test[[i]],], type = "prob"),
                         pred = predict(glmnet_m[[i]], newdata = sm2[outer_test[[i]],]),
                         obs = sm2[outer_test[[i]], 1])
}

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
plot(varImp(ctree_l1))
tab <- xtable(confusionMatrix(ctree_l1)$table, digits = 3)
print(tab, type = "latex", file = "t1_ctree_1.tex")

set.seed(74684)
ctree_l2 <- train(D_group  ~ .,
                  data = sm2,
                  method = "ctree",
                  trControl = ctrl2,
                  tuneGrid = grid,
                  metric = "logLoss")

ctree_l2
plot(ctree_l2)
plot(varImp(ctree_l2))
tab <- xtable(confusionMatrix(ctree_l2)$table, digits = 3)
print(tab, type = "latex", file = "t1_ctree_2.tex")

ctree_m <- list()
ctree_p <- list()

set.seed(74684)
for (i in 1:10){ # nested CV
  ctree_m[[i]] <- train(D_group ~ ., 
                         data = sm2[outer_train[[i]],],
                         method = "ctree",
                         tuneGrid = grid,
                         metric = "logLoss",
                         trControl = trainControl(method = "cv", 
                                                  number = 10,
                                                  summaryFunction = evalStats,
                                                  classProbs = TRUE))
  ctree_p[[i]] <- cbind(predict(ctree_m[[i]], newdata = sm2[outer_test[[i]],], type = "prob"),
                        pred = predict(ctree_m[[i]], newdata = sm2[outer_test[[i]],]),
                        obs = sm2[outer_test[[i]], 1])
}

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
plot(varImp(rf_l1), top = 20)
tab <- xtable(confusionMatrix(rf_l1)$table, digits = 3)
print(tab, type = "latex", file = "t1_rf_1.tex")

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
plot(varImp(rf_l2), top = 20)
tab <- xtable(confusionMatrix(rf_l2)$table, digits = 3)
print(tab, type = "latex", file = "t1_rf_2.tex")

rf_m <- list()
rf_p <- list()

set.seed(74684)
for (i in 1:10){ # nested CV
  rf_m[[i]] <- train(D_group ~ ., 
                     data = sm2[outer_train[[i]],],
                     method = "ranger",
                     tuneGrid = grid,
                     metric = "logLoss",
                     trControl = trainControl(method = "cv", 
                                              number = 10,
                                              summaryFunction = evalStats,
                                              classProbs = TRUE))
  rf_p[[i]] <- cbind(predict(rf_m[[i]], newdata = sm2[outer_test[[i]],], type = "prob"),
                     pred = predict(rf_m[[i]], newdata = sm2[outer_test[[i]],]),
                     obs = sm2[outer_test[[i]], 1])
}

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
plot(varImp(xgb_l1))
tab <- xtable(confusionMatrix(xgb_l1)$table, digits = 3)
print(tab, type = "latex", file = "t1_xgb_1.tex")

set.seed(74684)
xgb_l2 <- train(D_group  ~ .,
                data = sm2,
                method = "xgbTree",
                trControl = ctrl2,
                tuneGrid = grid,
                metric = "logLoss")

xgb_l2
plot(xgb_l2)
plot(varImp(xgb_l2))
tab <- xtable(confusionMatrix(xgb_l2)$table, digits = 3)
print(tab, type = "latex", file = "t1_xgb_2.tex")

xgb_m <- list()
xgb_p <- list()

set.seed(74684)
for (i in 1:10){ # nested CV
  xgb_m[[i]] <- train(D_group ~ ., 
                      data = sm2[outer_train[[i]],],
                      method = "xgbTree",
                      tuneGrid = grid,
                      metric = "logLoss",
                      trControl = trainControl(method = "cv", 
                                               number = 10,
                                               summaryFunction = evalStats,
                                               classProbs = TRUE))
  xgb_p[[i]] <- cbind(predict(xgb_m[[i]], newdata = sm2[outer_test[[i]],], type = "prob"),
                      pred = predict(xgb_m[[i]], newdata = sm2[outer_test[[i]],]),
                      obs = sm2[outer_test[[i]], 1])
}

## 06: Comparison

## Prediction in training set (CV)
# G_group

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

p1 <- ggplot(resamp_l1) +
  geom_boxplot(aes(y = AUC, x = fct_rev(model), fill = model)) +
  ylim(0.5, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  theme_light(base_size = 17) +
  theme(legend.position = "none")

p2 <- ggplot(resamp_l1) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  theme_light(base_size = 17) +
  theme(legend.position = "none")

plots <- arrangeGrob(p1, p2, nrow = 1)
ggsave("p1_CV_perf1.pdf", plots, width = 8.25, height = 6)

# D_group

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

p3 <- ggplot(resamp_l2) +
  geom_boxplot(aes(y = ROC, x = fct_rev(model), fill = model)) +
  ylim(0.9, 1) +
  labs(x = "") +
  labs(y = "ROC-AUC") +
  coord_flip() + 
  theme_light(base_size = 17) +
  theme(legend.position = "none")

p4 <- ggplot(resamp_l2) +
  geom_boxplot(aes(y = logLoss, x = fct_rev(model), fill = model)) +
  labs(x = "") +
  labs(y = "logLoss") +
  coord_flip() + 
  theme_light(base_size = 17) +
  theme(legend.position = "none")

plots <- arrangeGrob(p3, p4, nrow = 1)
ggsave("p1_CV_perf2.pdf", plots, width = 8.25, height = 6)

# Nested CV results

glmnet_sum <- matrix(nrow = 10, ncol = 3)
for (i in 1:10){
glmnet_sum[i, 1] <- defaultSummary(glmnet_p[[i]], lev = levels(glmnet_p[[i]]$obs))[[1]]
glmnet_sum[i, 2] <- twoClassSummary(glmnet_p[[i]], lev = levels(glmnet_p[[i]]$obs))[[1]]
glmnet_sum[i, 3] <- mnLogLoss(glmnet_p[[i]], lev = levels(glmnet_p[[i]]$obs))[[1]]
}

ctree_sum <- matrix(nrow = 10, ncol = 3)
for (i in 1:10){
  ctree_sum[i, 1] <- defaultSummary(ctree_p[[i]], lev = levels(ctree_p[[i]]$obs))[[1]]
  ctree_sum[i, 2] <- twoClassSummary(ctree_p[[i]], lev = levels(ctree_p[[i]]$obs))[[1]]
  ctree_sum[i, 3] <- mnLogLoss(ctree_p[[i]], lev = levels(ctree_p[[i]]$obs))[[1]]
}

rf_sum <- matrix(nrow = 10, ncol = 3)
for (i in 1:10){
  rf_sum[i, 1] <- defaultSummary(rf_p[[i]], lev = levels(rf_p[[i]]$obs))[[1]]
  rf_sum[i, 2] <- twoClassSummary(rf_p[[i]], lev = levels(rf_p[[i]]$obs))[[1]]
  rf_sum[i, 3] <- mnLogLoss(rf_p[[i]], lev = levels(rf_p[[i]]$obs))[[1]]
}

xgb_sum <- matrix(nrow = 10, ncol = 3)
for (i in 1:10){
  xgb_sum[i, 1] <- defaultSummary(xgb_p[[i]], lev = levels(xgb_p[[i]]$obs))[[1]]
  xgb_sum[i, 2] <- twoClassSummary(xgb_p[[i]], lev = levels(xgb_p[[i]]$obs))[[1]]
  xgb_sum[i, 3] <- mnLogLoss(xgb_p[[i]], lev = levels(xgb_p[[i]]$obs))[[1]]
}

sum_stats <- matrix(nrow = 12, ncol = 6)
num <- c(1, 5, 9)

for (i in 1:3){
j <- num[i]
sum_stats[j, ] <- rbind(min(glmnet_sum[, i]), quantile(glmnet_sum[, i], probs = 0.25), median(glmnet_sum[, i]), mean(glmnet_sum[, i]), 
                      quantile(glmnet_sum[, i], probs = 0.75), max(glmnet_sum[, i]))
sum_stats[j+1, ] <- rbind(min(ctree_sum[, i]), quantile(ctree_sum[, i], probs = 0.25), median(ctree_sum[, i]), mean(ctree_sum[, i]), 
                      quantile(ctree_sum[, i], probs = 0.75), max(ctree_sum[, i]))
sum_stats[j+2, ] <- rbind(min(rf_sum[, i]), quantile(rf_sum[, i], probs = 0.25), median(rf_sum[, i]), mean(rf_sum[, i]), 
                      quantile(rf_sum[, i], probs = 0.75), max(rf_sum[, i]))
sum_stats[j+3, ] <- rbind(min(xgb_sum[, i]), quantile(xgb_sum[, i], probs = 0.25), median(xgb_sum[, i]), mean(xgb_sum[, i]), 
                      quantile(xgb_sum[, i], probs = 0.75), max(xgb_sum[, i]))
}

stargazer(round(sum_stats, 3), summary = FALSE, out = "t1_perf.html")

# Save all results

save(sm, resamp_l1, resamp_l2, 
     glmnet_l1, glmnet_l2, 
     ctree_l1, ctree_l2,
     rf_l1, rf_l2,
     xgb_l1, xgb_l2,
     file = "./src/output1.Rdata")

# Save random forest

saveRDS(rf_l2, file = "./src/rf.rds")
