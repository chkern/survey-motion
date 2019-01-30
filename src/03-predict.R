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

ML_sub <- ML %>% 
  select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
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

ML_long <- reshape(ML_sub, direction = "long", 
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

# Set completion time outliers to NA

G_test_long$Completion_Time_s <- G_test_long$Completion_Time * 0.001
G_test_long$Completion_Time_sc <- G_test_long$Completion_Time_s

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E1"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E1"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E1"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E1"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E2"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E2"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E2"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E2"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E3"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E3"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E3"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E3"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E4"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E4"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E4"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E4"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E5"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E5"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E5"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E5"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E6"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E6"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E6"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E6"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E7"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E7"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E7"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E7"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E8"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Single_E8"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Single_E8"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Single_E8"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Matrix_1"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Matrix_1"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Matrix_1"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Matrix_1"] <- NA

lower <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Matrix_2"], probs = 0.05, na.rm = T)
upper <- quantile(G_test_long$Completion_Time_s[G_test_long$page == "Matrix_2"], probs = 0.95, na.rm = T)
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s < lower & G_test_long$page == "Matrix_2"] <- NA
G_test_long$Completion_Time_sc[G_test_long$Completion_Time_s > upper & G_test_long$page == "Matrix_2"] <- NA

G_test_a <- # Aggregate by mode of predicted classes over pages
  G_test_SM %>%
  arrange(ID) %>%
  group_by(ID) %>%
  summarise(p_rf_l1m = names(which.max(table(p_rf_l1))),
            p_rf_l2m = names(which.max(table(p_rf_l2))),
            p_xgb_l1m = names(which.max(table(p_xgb_l1))),
            p_xgb_l2m = names(which.max(table(p_xgb_l2))))

G_test_s <- # Aggregate by "ever moving" over pages
  G_test_SM %>%
  select(ID, page, p_rf_l2, p_xgb_l2) %>%
  gather(variable, value, -(ID:page)) %>%
  unite(temp, page, variable) %>%
  spread(temp, value)  %>%
  mutate(p_rf_l2e = ifelse(Matrix_1_p_rf_l2 == "Moving" | 
                           Matrix_2_p_rf_l2 == "Moving" |
                           Single_E1_p_rf_l2 == "Moving" |
                           Single_E2_p_rf_l2 == "Moving" |
                           Single_E3_p_rf_l2 == "Moving" |
                           Single_E4_p_rf_l2 == "Moving" |
                           Single_E5_p_rf_l2 == "Moving" |
                           Single_E6_p_rf_l2 == "Moving" |
                           Single_E7_p_rf_l2 == "Moving" |
                           Single_E8_p_rf_l2 == "Moving", "Moving", "Not_Moving")) %>%
  mutate(p_xgb_l2e = ifelse(Matrix_1_p_xgb_l2 == "Moving" | 
                             Matrix_2_p_xgb_l2 == "Moving" |
                             Single_E1_p_xgb_l2 == "Moving" |
                             Single_E2_p_xgb_l2 == "Moving" |
                             Single_E3_p_xgb_l2 == "Moving" |
                             Single_E4_p_xgb_l2 == "Moving" |
                             Single_E5_p_xgb_l2 == "Moving" |
                             Single_E6_p_xgb_l2 == "Moving" |
                             Single_E7_p_xgb_l2 == "Moving" |
                             Single_E8_p_xgb_l2 == "Moving", "Moving", "Not_Moving")) %>%
  select(ID, p_rf_l2e, p_xgb_l2e)

ML_sub <-
  ML %>%
  select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
         E1_Answ_1, E1_Completion_Time, E1_SF_OFF,
         E2_Answ_1, E2_Completion_Time, E2_SF_OFF,
         E3_Answ_1, E3_Completion_Time, E3_SF_OFF,
         E4_Answ_1, E4_Completion_Time, E4_SF_OFF,
         E5_Answ_1, E5_Completion_Time, E5_SF_OFF,
         E6_Answ_1, E6_Completion_Time, E6_SF_OFF,
         E7_Answ_1, E7_Completion_Time, E7_SF_OFF,
         E8_Answ_1, E8_Completion_Time, E8_SF_OFF,
         M_1_Answ_1, M_1_Answ_2, M_1_Answ_3, M_1_Answ_4, M_1_Answ_5, M_1_Answ_6, M_1_Answ_7, M_1_Answ_8, 
         M_1_Completion_Time, M_1_SF_OFF,
         M_2_Answ_1, M_2_Answ_2, M_2_Answ_3, M_2_Answ_4, M_2_Answ_5, M_2_Answ_6, M_2_Answ_7, M_2_Answ_8,
         M_2_Completion_Time, M_2_SF_OFF,
         AC_Answ_1, AC_Answ_2, AC_Answ_3, AC_Answ_4, AC_Answ_5, AC_Answ_6, AC_Answ_7, AC_Answ_8,
         AC_Answ_9, AC_Answ_10, AC_Answ_11, AC_Answ_12, AC_Answ_13, AC_Answ_14, AC_Answ_15,
         AC_Completion_Time, AC_SF_OFF,
         SemDiff_Answ_1, SemDiff_Answ_2, SemDiff_Answ_3, SemDiff_Answ_4, SemDiff_Answ_5,
         SemDiff_Completion_Time, SemDiff_SF_OFF,
         Motivation_Answ_1, Motivation_Completion_Time, Motivation_SF_OFF,
         Particip_Answ_1, Particip_Completion_Time, Particip_SF_OFF,
         Multitasking_1_Answ_1, Multitasking_1_Completion_Time, Multitasking_1_SF_OFF,
         Multitasking_2_Answ_1, Multitasking_2_Answ_2, Multitasking_2_Answ_3, Multitasking_2_Answ_4, 
         Multitasking_2_Answ_5, Multitasking_2_Answ_6, Multitasking_2_Answ_7, Multitasking_2_Answ_8,
         Multitasking_2_Completion_Time, Multitasking_2_SF_OFF,
         Demo_Answ_1, Demo_Answ_2, Demo_Answ_3, Demo_Answ_4, Demo_Answ_5,
         Demo_Completion_Time, Demo_SF_OFF)

G_test_wide <-
  ML_sub %>%
  mutate_at(c("SemDiff_Answ_1", "SemDiff_Answ_2", "SemDiff_Answ_3", "SemDiff_Answ_4", "SemDiff_Answ_5",
              "Motivation_Answ_1", "Particip_Answ_1", "Multitasking_1_Answ_1"), list(~na_if(., 0))) %>%
  mutate_at(c("E1_SF_OFF", "E2_SF_OFF", "E3_SF_OFF", "E4_SF_OFF", "E5_SF_OFF", "E6_SF_OFF", "E7_SF_OFF", 
              "E8_SF_OFF", "M_1_SF_OFF", "M_2_SF_OFF"), list(~replace_na(., 0))) %>%
  mutate(AC_Answ = ifelse(AC_Answ_10 == 1 & AC_Answ_15 == 1, 1, 0)) %>%
  mutate(Multitasking_1_Answ = ifelse(Multitasking_1_Answ_1 == 1, 1, 0)) %>%
  mutate(Multitasking_2_Answ = rowSums(ML_sub[, 83:88])) %>%
  left_join(G_test_a, by = "ID") %>%
  left_join(G_test_s, by = "ID")

save(G_test_long, G_test_wide, file = "./src/output3.Rdata")
