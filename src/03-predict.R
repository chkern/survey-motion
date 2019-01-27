##################
# Survey Motion
# Part II: Apply Prediction Models on Test Set
# Christoph Kern
# R 3.4.4
##################

## 01: Setup

library(tidyverse)
library(matrixStats)
library(party)
library(partykit)
library(caret)

setwd("/home/ckern/Uni/Forschung/Article/2019 - MASS")
load("./src/output1.Rdata")
load("./data/G_Test_SM.RData")
load("./data/G_Test_full.RData")

# X

motionvars <- grep("SM_" , names(SM_Final), value = TRUE)

G_test_SM <-
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
  bind_cols(SM_Final[, 1:2], .)

## 02: Predict

pr_glmnet_l1 <- predict(glmnet_l1, newdata = G_test_SM, type = "prob")
pr_ctree_l1 <- predict(ctree_l1, newdata = G_test_SM, type = "prob")
pr_rf_l1 <- predict(rf_l1, newdata = G_test_SM, type = "prob")
pr_xgb_l1 <- predict(xgb_l1, newdata = G_test_SM, type = "prob")

p_glmnet_l1 <- predict(glmnet_l1, newdata = G_test_SM)
p_ctree_l1 <- predict(ctree_l1, newdata = G_test_SM)
p_rf_l1 <- predict(rf_l1, newdata = G_test_SM)
p_xgb_l1 <- predict(xgb_l1, newdata = G_test_SM)

pr_glmnet_l2 <- predict(glmnet_l2, newdata = G_test_SM, type = "prob")
pr_ctree_l2 <- predict(ctree_l2, newdata = G_test_SM, type = "prob")
pr_rf_l2 <- predict(rf_l2, newdata = G_test_SM, type = "prob")
pr_xgb_l2 <- predict(xgb_l2, newdata = G_test_SM, type = "prob")

p_glmnet_l2 <- predict(glmnet_l2, newdata = G_test_SM)
p_ctree_l2 <- predict(ctree_l2, newdata = G_test_SM)
p_rf_l2 <- predict(rf_l2, newdata = G_test_SM)
p_xgb_l2 <- predict(xgb_l2, newdata = G_test_SM)

## 03: Plot predicted propensities

ggplot(pr_glmnet_l1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_ctree_l1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_rf_l1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_xgb_l1) +
  geom_freqpoly(aes(x = Sitting), color = "green") +
  geom_freqpoly(aes(x = Standing), color = "black") +
  geom_freqpoly(aes(x = Walking), color = "red") +
  geom_freqpoly(aes(x = Climbing), color = "orange")

ggplot(pr_glmnet_l2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_ctree_l2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_rf_l2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

ggplot(pr_xgb_l2) +
  geom_freqpoly(aes(x = Not_Moving), color = "green") +
  geom_freqpoly(aes(x = Moving), color = "red")

## 04: Summarize predicted classes

prop.table(table(p_glmnet_l1))
prop.table(table(p_ctree_l1))
prop.table(table(p_rf_l1))
prop.table(table(p_xgb_l1))

prop.table(table(p_glmnet_l2))
prop.table(table(p_ctree_l2))
prop.table(table(p_rf_l2))
prop.table(table(p_xgb_l2))

# 05: Agreement across classifiers

table(p_glmnet_l1, p_ctree_l1)
table(p_glmnet_l1, p_rf_l1)
table(p_glmnet_l1, p_xgb_l1)
table(p_ctree_l1, p_rf_l1)
table(p_ctree_l1, p_xgb_l1)
table(p_rf_l1, p_xgb_l1)

table(p_glmnet_l2, p_ctree_l2)
table(p_glmnet_l2, p_rf_l2)
table(p_glmnet_l2, p_xgb_l2)
table(p_ctree_l2, p_rf_l2)
table(p_ctree_l2, p_xgb_l2)
table(p_rf_l2, p_xgb_l2)

# 06: Join with full data (long and wide)

G_test_SM <- 
  cbind(G_test_SM, 
        p_glmnet_l1, p_ctree_l1, p_rf_l1, p_xgb_l1,
        p_glmnet_l2, p_ctree_l2, p_rf_l2, p_xgb_l2)

ML <- ML %>% select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
                    E1_Answ_1, E1_Completion_Time, E1_SF_OFF,
                    E2_Answ_1, E2_Completion_Time, E2_SF_OFF,
                    E3_Answ_1, E3_Completion_Time, E3_SF_OFF,
                    E4_Answ_1, E4_Completion_Time, E4_SF_OFF,
                    E5_Answ_1, E5_Completion_Time, E5_SF_OFF,
                    E6_Answ_1, E6_Completion_Time, E6_SF_OFF,
                    E7_Answ_1, E7_Completion_Time, E7_SF_OFF,
                    E8_Answ_1, E8_Completion_Time, E8_SF_OFF,
                    M_1_Answ_1, M_1_Completion_Time, M_1_SF_OFF,
                    M_2_Answ_1, M_2_Completion_Time, M_2_SF_OFF)

ML_long <- reshape(ML, direction = "long", 
        varying = c("E1_Answ_1", "E1_Completion_Time", "E1_SF_OFF",
                  "E2_Answ_1", "E2_Completion_Time", "E2_SF_OFF",
                  "E3_Answ_1", "E3_Completion_Time", "E3_SF_OFF",
                  "E4_Answ_1", "E4_Completion_Time", "E4_SF_OFF",
                  "E5_Answ_1", "E5_Completion_Time", "E5_SF_OFF",
                  "E6_Answ_1", "E6_Completion_Time", "E6_SF_OFF",
                  "E7_Answ_1", "E7_Completion_Time", "E7_SF_OFF",
                  "E8_Answ_1", "E8_Completion_Time", "E8_SF_OFF",
                  "M_1_Answ_1", "M_1_Completion_Time", "M_1_SF_OFF",
                  "M_2_Answ_1", "M_2_Completion_Time", "M_2_SF_OFF"), 
        timevar = "page",
        times = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "M_1", "M_2"),
        v.names = c("Answ_1", "Completion_Time", "SF_OFF"),
        idvar = "ID")

G_test_long <-
  ML_long %>%
  mutate(page = fct_recode(page, "Matrix_1" = "M_1", 
                           "Matrix_2" = "M_2",
                           "Single_E1" = "E1",
                           "Single_E2" = "E2",
                           "Single_E3" = "E3",
                           "Single_E4" = "E4",
                           "Single_E5" = "E5",
                           "Single_E6" = "E6",
                           "Single_E7" = "E7",
                           "Single_E8" = "E8")) %>%
  left_join(G_test_SM, by = c("ID" = "ID", "page" = "page")) %>%
  arrange(ID, page)

# G_test_s <- # Spread predicted classes into wide format
#  G_test_SM %>%
#  select(ID, page, p_glmnet_l1, p_ctree_l1, p_rf_l1, p_xgb_l1, p_glmnet_l2, p_ctree_l2, p_rf_l2, p_xgb_l2) %>%
#  gather(variable, value, -(ID:page)) %>%
#  unite(temp, page, variable) %>%
#  spread(temp, value)

# G_test_a <- # Aggregate by mode of predicted classes over pages
#  G_test_SM %>%
#  arrange(ID) %>%
#  group_by(ID) %>%
#  summarise(p_rf_l1m = names(which.max(table(p_rf_l1))),
#            p_rf_l2m = names(which.max(table(p_rf_l2))),
#            p_xgb_l1m = names(which.max(table(p_xgb_l1))),
#            p_xgb_l2m = names(which.max(table(p_xgb_l2))))

# G_test_wide <-
#  ML %>%
#  left_join(G_test_s, by = "ID") %>%
#  left_join(G_test_a, by = "ID")

save(G_test_long, file = "./src/output3.Rdata")
