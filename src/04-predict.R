##################
# Survey Motion
# Part IIIa: Apply Prediction Models on Test Set (Respondi data)
# Christoph Kern
# R 3.5.1
##################

## 01: Setup

library(tidyverse)
library(data.table)
library(matrixStats)
library(party)
library(partykit)
library(caret)

setwd("/home/ckern/Uni/Forschung/Article/2019 - MASS")
load("./src/output1.Rdata")
load("./data/SD-KOR_Test.RData")

# Pre-processing

resp_p <- 
  Dataset2 %>% select(ID, MA_1_SM_No_G, MA_2_SM_No_G, MA_3_SM_No_G, MA_4_SM_No_G, MA_5_SM_No_G, MJ_1_SM_No_G, MJ_2_SM_No_G)

resp_long <- reshape(resp_p, direction = "long",
                     varying = c("MA_1_SM_No_G","MA_2_SM_No_G", "MA_3_SM_No_G", "MA_4_SM_No_G", "MA_5_SM_No_G", 
                                 "MJ_1_SM_No_G", "MJ_2_SM_No_G"),
                     timevar = "page",
                     times = c("MA_1", "MA_2", "MA_3", "MA_4", "MA_5", "MJ_1", "MJ_2"),
                     v.names = c("SM"),
                     idvar = "ID")

resp_long <- arrange(resp_long, ID, as.numeric(as.character(page)))

resp_long <- filter(resp_long, SM != "")
names <- paste0("SM_", 1:15000)

datalist <- list()
for (i in resp_long$ID) {
  datalist[[i]] <- separate(resp_long[resp_long$ID == i,], 
                            col = SM, 
                            sep = ",",
                            into = names,
                            fill = "right",
                            convert = TRUE,
                            remove = FALSE)
  datalist[[i]] <- datalist[[i]][, colSums(is.na(datalist[[i]])) != nrow(datalist[[i]])]
  print(i)
}
resp_long <- rbindlist(datalist, fill = TRUE)

saveRDS(resp_long, file = "./data/Respondi_Test.RDS")

# X

motionvars <- grep("SM_" , names(resp_long), value = TRUE)

resp_SM <-
  resp_long %>%
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
  bind_cols(resp_long[, 1:3], .)

resp_SM[which(resp_SM$SM_max == 0), 4:17] <- NA
resp_SM_drop <- drop_na(resp_SM, 4:17)

## 02: Predict

pr_glmnet_l1 <- predict(glmnet_l1, newdata = resp_SM_drop, type = "prob")
pr_ctree_l1 <- predict(ctree_l1, newdata = resp_SM_drop, type = "prob")
pr_rf_l1 <- predict(rf_l1, newdata = resp_SM_drop, type = "prob")
pr_xgb_l1 <- predict(xgb_l1, newdata = resp_SM_drop, type = "prob")

p_glmnet_l1 <- predict(glmnet_l1, newdata = resp_SM_drop)
p_ctree_l1 <- predict(ctree_l1, newdata = resp_SM_drop)
p_rf_l1 <- predict(rf_l1, newdata = resp_SM_drop)
p_xgb_l1 <- predict(xgb_l1, newdata = resp_SM_drop)

pr_glmnet_l2 <- predict(glmnet_l2, newdata = resp_SM_drop, type = "prob")
pr_ctree_l2 <- predict(ctree_l2, newdata = resp_SM_drop, type = "prob")
pr_rf_l2 <- predict(rf_l2, newdata = resp_SM_drop, type = "prob")
pr_xgb_l2 <- predict(xgb_l2, newdata = resp_SM_drop, type = "prob")

p_glmnet_l2 <- predict(glmnet_l2, newdata = resp_SM_drop)
p_ctree_l2 <- predict(ctree_l2, newdata = resp_SM_drop)
p_rf_l2 <- predict(rf_l2, newdata = resp_SM_drop)
p_xgb_l2 <- predict(xgb_l2, newdata = resp_SM_drop)

resp_SM_drop <- 
  resp_SM_drop %>%
  add_column(., 
             p_glmnet_l1, p_ctree_l1, p_rf_l1, p_xgb_l1,
             p_glmnet_l2, p_ctree_l2, p_rf_l2, p_xgb_l2) %>%
  select(ID, page, 18:25)

resp_SM <- left_join(resp_SM, resp_SM_drop, by = c("ID", "page"))

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

resp_SM <- mutate(resp_SM, page = fct_recode(page, 
                                           "E1" = "MA_1",
                                           "E2" = "MA_2",
                                           "E3" = "MA_3",
                                           "E4" = "MA_4",
                                           "E5" = "MA_5",
                                           "M_1" = "MJ_1", 
                                           "M_2" = "MJ_2")) 

save(resp_SM, file = "./src/output4.Rdata")

# 06: Join with full data (long_1: one row per page)

ML <- Dataset2 %>%
  rename("E1_Answ_1" = "MA_1_Answ_1",
         "E2_Answ_1" = "MA_2_Answ_1",
         "E3_Answ_1" = "MA_3_Answ_1",
         "E4_Answ_1" = "MA_4_Answ_1",
         "E5_Answ_1" = "MA_5_Answ_1",
         "M_1_Answ_1" = "MJ_1_Answ_1", "M_1_Answ_2" = "MJ_1_Answ_2",
         "M_1_Answ_3" = "MJ_1_Answ_3", "M_1_Answ_4" = "MJ_1_Answ_4",
         "M_1_Answ_5" = "MJ_1_Answ_5", "M_1_Answ_6" = "MJ_1_Answ_6",
         "M_1_Answ_7" = "MJ_1_Answ_7", "M_1_Answ_8" = "MJ_1_Answ_8",
         "M_2_Answ_1" = "MJ_2_Answ_1", "M_2_Answ_2" = "MJ_2_Answ_2",
         "M_2_Answ_3" = "MJ_2_Answ_3", "M_2_Answ_4" = "MJ_2_Answ_4",
         "M_2_Answ_5" = "MJ_2_Answ_5", "M_2_Answ_6" = "MJ_2_Answ_6",
         "M_2_Answ_7" = "MJ_2_Answ_7", "M_2_Answ_8" = "MJ_2_Answ_8")

# Completion time

ML <-
  ML %>% # Completion time in seconds
  mutate(E1_Completion_Time_s = MA_1_Completion_Time * 0.001,
         E2_Completion_Time_s = MA_2_Completion_Time * 0.001,
         E3_Completion_Time_s = MA_3_Completion_Time * 0.001,
         E4_Completion_Time_s = MA_4_Completion_Time * 0.001,
         E5_Completion_Time_s = MA_5_Completion_Time * 0.001,
         M_1_Completion_Time_s = MJ_1_Completion_Time * 0.001,
         M_2_Completion_Time_s = MJ_2_Completion_Time * 0.001)

list_in <- c("E1_Completion_Time_s", "E2_Completion_Time_s", "E3_Completion_Time_s",
             "E4_Completion_Time_s", "E5_Completion_Time_s", "M_1_Completion_Time_s", "M_2_Completion_Time_s")

list_out <- c("E1_Completion_Time_sc", "E2_Completion_Time_sc", "E3_Completion_Time_sc",
              "E4_Completion_Time_sc", "E5_Completion_Time_sc", "M_1_Completion_Time_sc", "M_2_Completion_Time_sc")

cln_outliers <- function(x){
  lower <- quantile(x, probs = 0.05, na.rm = T)
  upper <- quantile(x, probs = 0.95, na.rm = T)
  x[x < lower] <- NA
  x[x > upper] <- NA
  return(x)
}

ML[, list_out] <- lapply(ML[, list_in], cln_outliers)

# IRV

ML$M_1_irv <- rowSds(as.matrix(ML[, 36:43]), na.rm = T) # Row SD for M_1
ML$M_2_irv <- rowSds(as.matrix(ML[, 49:56]), na.rm = T) # Row SD for M_2

ML$E1_irv <- NA
ML$E2_irv <- NA
ML$E3_irv <- NA
ML$E4_irv <- NA
ML$E5_irv <- NA
ML$E6_irv <- NA
ML$E7_irv <- NA
ML$E8_irv <- NA

# Join

ML_sub <- ML %>%
  select(ID, Birthyear, sex, PC_SD, datetime, 
         E1_Answ_1, E1_Completion_Time_s, E1_Completion_Time_sc, E1_irv,
         E2_Answ_1, E2_Completion_Time_s, E2_Completion_Time_sc, E2_irv,
         E3_Answ_1, E3_Completion_Time_s, E3_Completion_Time_sc, E3_irv,
         E4_Answ_1, E4_Completion_Time_s, E4_Completion_Time_sc, E4_irv,
         E5_Answ_1, E5_Completion_Time_s, E5_Completion_Time_sc, E5_irv,
         M_1_Answ_1, M_1_Completion_Time_s, M_1_Completion_Time_sc, M_1_irv,
         M_2_Answ_1, M_2_Completion_Time_s, M_2_Completion_Time_sc, M_2_irv)

ML_long1 <- reshape(ML_sub, direction = "long", 
                   varying = c("E1_Answ_1", "E1_Completion_Time_s", "E1_Completion_Time_sc", "E1_irv",
                               "E2_Answ_1", "E2_Completion_Time_s", "E2_Completion_Time_sc", "E2_irv",
                               "E3_Answ_1", "E3_Completion_Time_s", "E3_Completion_Time_sc", "E3_irv",
                               "E4_Answ_1", "E4_Completion_Time_s", "E4_Completion_Time_sc", "E4_irv",
                               "E5_Answ_1", "E5_Completion_Time_s", "E5_Completion_Time_sc", "E5_irv",
                               "M_1_Answ_1", "M_1_Completion_Time_s", "M_1_Completion_Time_sc", "M_1_irv",
                               "M_2_Answ_1", "M_2_Completion_Time_s", "M_2_Completion_Time_sc", "M_2_irv"), 
                   timevar = "page",
                   times = c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2"),
                   v.names = c("Answ_1", "Completion_Time_s", "Completion_Time_sc", "irv"),
                   idvar = "ID")

qs <- quantile(ML_long1$irv, probs = c(0, 0.1, 0.2, 0.5, 0.8, 0.9, 1), na.rm = T)
ML_long1$irv_p10 <- ifelse(ML_long1$irv <= qs[[2]], 1, 0)
ML_long1$irv_p20 <- ifelse(ML_long1$irv <= qs[[3]], 1, 0)
ML_long1$irv_p80 <- ifelse(ML_long1$irv >= qs[[5]], 1, 0)
ML_long1$irv_p90 <- ifelse(ML_long1$irv >= qs[[6]], 1, 0)

ML_long1 <-
  ML_long1 %>%
  mutate(pages = fct_collapse(page, 
                              Matrix = c("M_1", "M_2"),
                              Single = c("E1", "E2", "E3", "E4", "E5"))) %>%
  mutate(sex = fct_recode(as.factor(sex),
                             "male" = "1",
                             "female" = "2")) %>%
  mutate(age = 2019 - Birthyear) %>%
  mutate(age_s = scale(age)[,1])
  
resp_long1 <-
  ML_long1 %>%
  left_join(resp_SM, by = c("ID", "page")) %>%
  arrange(ID, page)

# 07: Join with full data (long_2: one row per item)

# Primacy

list_in <- c("E1_Answ_1", "E2_Answ_1", "E3_Answ_1", "E4_Answ_1", "E5_Answ_1",
             "M_1_Answ_1", "M_1_Answ_2", "M_1_Answ_3", "M_1_Answ_4", "M_1_Answ_5", "M_1_Answ_6", "M_1_Answ_7", "M_1_Answ_8",
             "M_2_Answ_1", "M_2_Answ_2", "M_2_Answ_3", "M_2_Answ_4", "M_2_Answ_5", "M_2_Answ_6", "M_2_Answ_7", "M_2_Answ_8")
  
list_out <- c("E1_primacy", "E2_primacy", "E3_primacy", "E4_primacy", "E5_primacy",
             "M_1_1_primacy", "M_1_2_primacy", "M_1_3_primacy", "M_1_4_primacy", "M_1_5_primacy", "M_1_6_primacy", "M_1_7_primacy", "M_1_8_primacy",
             "M_2_1_primacy", "M_2_2_primacy", "M_2_3_primacy", "M_2_4_primacy", "M_2_5_primacy", "M_2_6_primacy", "M_2_7_primacy", "M_2_8_primacy")

ML[,list_out] <- lapply(ML[,list_in], function(x) {ifelse(x == 1, 1, 0)})

# Join

ML_sub <- ML %>% 
  select(ID, Birthyear, sex, PC_SD, datetime, 
         E1_primacy, E2_primacy, E3_primacy, E4_primacy, E5_primacy,
         M_1_1_primacy, M_1_2_primacy, M_1_3_primacy, M_1_4_primacy, M_1_5_primacy, M_1_6_primacy, M_1_7_primacy, M_1_8_primacy,
         M_2_1_primacy, M_2_2_primacy, M_2_3_primacy, M_2_4_primacy, M_2_5_primacy, M_2_6_primacy, M_2_7_primacy, M_2_8_primacy)

ML_long2 <- reshape(ML_sub, direction = "long", 
                    varying = c("E1_primacy", "E2_primacy", "E3_primacy", "E4_primacy", "E5_primacy",
                                "M_1_1_primacy", "M_1_2_primacy", "M_1_3_primacy", "M_1_4_primacy", "M_1_5_primacy", "M_1_6_primacy", "M_1_7_primacy", "M_1_8_primacy",
                                "M_2_1_primacy", "M_2_2_primacy", "M_2_3_primacy", "M_2_4_primacy", "M_2_5_primacy", "M_2_6_primacy", "M_2_7_primacy", "M_2_8_primacy"), 
                    timevar = "item",
                    times = c("E1", "E2", "E3", "E4", "E5", 
                              "M_1_1", "M_1_2", "M_1_3", "M_1_4", "M_1_5", "M_1_6", "M_1_7", "M_1_8",
                              "M_2_1", "M_2_2", "M_2_3", "M_2_4", "M_2_5", "M_2_6", "M_2_7", "M_2_8"),
                    v.names = c("primacy"),
                    idvar = "ID")

ML_long2 <-
  ML_long2 %>%
  mutate(pages = fct_collapse(item, 
                              Matrix = c("M_1_1", "M_1_2", "M_1_3", "M_1_4", "M_1_5", "M_1_6", "M_1_7", "M_1_8",
                                         "M_2_1", "M_2_2", "M_2_3", "M_2_4", "M_2_5", "M_2_6", "M_2_7", "M_2_8"),
                              Single = c("E1", "E2", "E3","E4","E5"))) %>%
  mutate(page = fct_recode(item, 
                           "M_1" = "M_1_1", "M_1" = "M_1_2", "M_1" = "M_1_3", "M_1" = "M_1_4", 
                           "M_1" = "M_1_5", "M_1" = "M_1_6", "M_1" = "M_1_7", "M_1" = "M_1_8",
                           "M_2" = "M_2_1", "M_2" = "M_2_2", "M_2" = "M_2_3", "M_2" = "M_2_4",
                           "M_2" = "M_2_5", "M_2" = "M_2_6", "M_2" = "M_2_7", "M_2" = "M_2_8",
                           "E1" = "E1", "E2" = "E2", "E3" = "E3", "E4" = "E4", "E5" = "E5")) %>%
  mutate(sex = fct_recode(as.factor(sex),
                             "male" = "1",
                             "female" = "2"))  %>%
  mutate(age = 2019 - Birthyear) %>%
  mutate(age_s = scale(age)[,1])

resp_long2 <- 
  ML_long2 %>%
  left_join(resp_SM, by = c("ID", "page")) %>%
  arrange(ID, page, item)

# 08: Join with full data (ac: only attention check)

# Attention check

# Save

save(resp_SM, resp_long1, resp_long2, file = "./src/output4.Rdata")
