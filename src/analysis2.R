##################
# Survey Motion
# Part II: Apply Prediction Models on Test Set
# Christoph Kern
# R 3.4.4
##################

## 01: Setup

library(tidyverse)
library(matrixStats)
library(caret)

load("~/Uni/Forschung/Article/2019 - MASS/data/Test_Data_1_0.RData")
load("~/Uni/Forschung/Article/2019 - MASS/src/output1.Rdata")

# X

motionvars <- grep("SM_" , names(SM_Final), value = TRUE)

sm <-
  SM_Final %>%
  select(motionvars) %>%
  mutate(SM_mean = rowMeans2(as.matrix(.), na.rm = T)) %>%
  mutate(SM_med = rowMedians(as.matrix(.), na.rm = T)) %>%
  mutate(SM_var = rowVars(as.matrix(.), na.rm = T)) %>%
  mutate(SM_mad = rowMads(as.matrix(.), na.rm = T)) %>%
  mutate(SM_iqr = rowIQRs(as.matrix(.), na.rm = T)) %>%
  mutate(SM_q5 = rowQuantiles(as.matrix(.), probs = 0.05, na.rm = T)) %>%
  mutate(SM_q10 = rowQuantiles(as.matrix(.), probs = 0.10, na.rm = T)) %>%
  mutate(SM_q25 = rowQuantiles(as.matrix(.), probs = 0.25, na.rm = T)) %>%
  mutate(SM_q75 = rowQuantiles(as.matrix(.), probs = 0.75, na.rm = T)) %>%
  mutate(SM_q9 = rowQuantiles(as.matrix(.), probs = 0.9, na.rm = T)) %>%
  mutate(SM_q95 = rowQuantiles(as.matrix(.), probs = 0.95, na.rm = T)) %>%
  select(SM_mean, SM_med, SM_var, SM_mad, SM_iqr, SM_q5, SM_q10, SM_q25, SM_q75, SM_q9, SM_q95) %>%
  bind_cols(SM_Final, .)

## 02: Predict

pr_glmnet1 <- predict(glmnet1, newdata = sm, type = "prob")
pr_ctree1 <- predict(ctree1, newdata = sm, type = "prob")
pr_rf1 <- predict(rf1, newdata = sm, type = "prob")
pr_xgb1 <- predict(xgb1, newdata = sm, type = "prob")

p_glmnet1 <- predict(glmnet1, newdata = sm)
p_ctree1 <- predict(ctree1, newdata = sm)
p_rf1 <- predict(rf1, newdata = sm)
p_xgb1 <- predict(xgb1, newdata = sm)

pr_glmnet2 <- predict(glmnet2, newdata = sm, type = "prob")
pr_ctree2 <- predict(ctree2, newdata = sm, type = "prob")
pr_rf2 <- predict(rf2, newdata = sm, type = "prob")
pr_xgb2 <- predict(xgb2, newdata = sm, type = "prob")

p_glmnet2 <- predict(glmnet2, newdata = sm)
p_ctree2 <- predict(ctree2, newdata = sm)
p_rf2 <- predict(rf2, newdata = sm)
p_xgb2 <- predict(xgb2, newdata = sm)

## 03: Plot predicted propensities

ggplot(pr_glmnet1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_ctree1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_rf1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_xgb1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_glmnet2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_ctree2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_rf2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_xgb2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

## 04: Summarize predicted classes

prop.table(table(p_glmnet1))
prop.table(table(p_ctree1))
prop.table(table(p_rf1))
prop.table(table(p_xgb1))

prop.table(table(p_glmnet2))
prop.table(table(p_ctree2))
prop.table(table(p_rf2))
prop.table(table(p_xgb2))

# 05: Agreement across classifiers

table(p_glmnet1, p_ctree1)
table(p_glmnet1, p_rf1)
table(p_glmnet1, p_xgb1)
table(p_ctree1, p_rf1)
table(p_ctree1, p_xgb1)
table(p_rf1, p_xgb1)

table(p_glmnet2, p_ctree2)
table(p_glmnet2, p_rf2)
table(p_glmnet2, p_xgb2)
table(p_ctree2, p_rf2)
table(p_ctree2, p_xgb2)
table(p_rf2, p_xgb2)
