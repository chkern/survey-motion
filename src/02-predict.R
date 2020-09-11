##################
# Survey Motion
# Part IIa: Apply Prediction Models on Test Set (Goettingen data)
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
Goe <- read.csv("./data/Goe_with_out_G.csv", sep = "|", header = T) # nur SM vars

# Pre-processing

Goe_long <- 
  Goe %>% 
  gather(key = "page", value = "SM", 8:38) %>%
  arrange(lfdn, as.numeric(as.character(page)))

Goe_long <- filter(Goe_long, SM != "")
names <- paste0("SM_", 1:15000)

datalist <- list()
for (i in Goe$lfdn) {
  datalist[[i]] <- separate(Goe_long[Goe_long$lfdn == i,], 
                            col = SM, 
                            sep = ",",
                            into = names,
                            fill = "right",
                            convert = TRUE,
                            remove = FALSE)
  datalist[[i]] <- datalist[[i]][, colSums(is.na(datalist[[i]])) != nrow(datalist[[i]])]
  print(i)
}
Goe_long <- rbindlist(datalist, fill = TRUE)

saveRDS(Goe_long, file = "./data/Goe_Test.RDS")

# X

motionvars <- grep("SM_" , names(Goe_long), value = TRUE)

Goe_SM <-
  Goe_long %>%
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
  bind_cols(Goe_long[, 1:9], .)

Goe_SM[which(Goe_SM$SM_max == 0), 10:23] <- NA
Goe_SM_drop <- drop_na(Goe_SM, 10:23)

## 02: Predict

pr_glmnet_l1 <- predict(glmnet_l1, newdata = Goe_SM_drop, type = "prob")
pr_ctree_l1 <- predict(ctree_l1, newdata = Goe_SM_drop, type = "prob")
pr_rf_l1 <- predict(rf_l1, newdata = Goe_SM_drop, type = "prob")
pr_xgb_l1 <- predict(xgb_l1, newdata = Goe_SM_drop, type = "prob")

p_glmnet_l1 <- predict(glmnet_l1, newdata = Goe_SM_drop)
p_ctree_l1 <- predict(ctree_l1, newdata = Goe_SM_drop)
p_rf_l1 <- predict(rf_l1, newdata = Goe_SM_drop)
p_xgb_l1 <- predict(xgb_l1, newdata = Goe_SM_drop)

pr_glmnet_l2 <- predict(glmnet_l2, newdata = Goe_SM_drop, type = "prob")
pr_ctree_l2 <- predict(ctree_l2, newdata = Goe_SM_drop, type = "prob")
pr_rf_l2 <- predict(rf_l2, newdata = Goe_SM_drop, type = "prob")
pr_xgb_l2 <- predict(xgb_l2, newdata = Goe_SM_drop, type = "prob")

p_glmnet_l2 <- predict(glmnet_l2, newdata = Goe_SM_drop)
p_ctree_l2 <- predict(ctree_l2, newdata = Goe_SM_drop)
p_rf_l2 <- predict(rf_l2, newdata = Goe_SM_drop)
p_xgb_l2 <- predict(xgb_l2, newdata = Goe_SM_drop)

Goe_SM_drop <- 
  Goe_SM_drop %>%
  add_column(., 
             p_glmnet_l1, p_ctree_l1, p_rf_l1, p_xgb_l1,
             p_glmnet_l2, p_ctree_l2, p_rf_l2, p_xgb_l2) %>%
  select(lfdn, page, 24:31)

Goe_SM <- left_join(Goe_SM, Goe_SM_drop, by = c("lfdn", "page"))

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

save(Goe_SM, file = "./src/output2.Rdata")

# 06: Join with full data (long_1: one row per page)

Goe_SM <- mutate(Goe_SM, page = fct_recode(page, 
                                           "M_1" = "v_579", 
                                           "M_2" = "v_609",
                                           "E1" = "v_399",
                                           "E2" = "v_429",
                                           "E3" = "v_459",
                                           "E4" = "v_489",
                                           "E5" = "v_519",
                                           "E6" = "v_309",
                                           "E7" = "v_339",
                                           "E8" = "v_369")) 

load("./data/Goe_Test_full.RData") # inhaltliche vars

ML <- filter(ML, Mobile_Device == 1)

# Completion time

ML <-
  ML %>% # Completion time in seconds
  mutate(E1_Completion_Time_s = E1_Completion_Time * 0.001,
         E2_Completion_Time_s = E2_Completion_Time * 0.001,
         E3_Completion_Time_s = E3_Completion_Time * 0.001,
         E4_Completion_Time_s = E4_Completion_Time * 0.001,
         E5_Completion_Time_s = E5_Completion_Time * 0.001,
         E6_Completion_Time_s = E6_Completion_Time * 0.001,
         E7_Completion_Time_s = E7_Completion_Time * 0.001,
         E8_Completion_Time_s = E8_Completion_Time * 0.001,
         M_1_Completion_Time_s = M_1_Completion_Time * 0.001,
         M_2_Completion_Time_s = M_2_Completion_Time * 0.001)

list_in <- c("E1_Completion_Time_s", "E2_Completion_Time_s", "E3_Completion_Time_s", "E4_Completion_Time_s",
             "E5_Completion_Time_s", "E6_Completion_Time_s", "E7_Completion_Time_s", "E8_Completion_Time_s",
             "M_1_Completion_Time_s", "M_2_Completion_Time_s")

list_out <- c("E1_Completion_Time_sc", "E2_Completion_Time_sc", "E3_Completion_Time_sc", "E4_Completion_Time_sc",
             "E5_Completion_Time_sc", "E6_Completion_Time_sc", "E7_Completion_Time_sc", "E8_Completion_Time_sc",
             "M_1_Completion_Time_sc", "M_2_Completion_Time_sc")

cln_outliers <- function(x){
  lower <- quantile(x, probs = 0.05, na.rm = T)
  upper <- quantile(x, probs = 0.95, na.rm = T)
  x[x < lower] <- NA
  x[x > upper] <- NA
  return(x)
}

ML[, list_out] <- lapply(ML[, list_in], cln_outliers)

# IRV

ML$M_1_irv <- rowSds(as.matrix(ML[, 468:475]), na.rm = T) # Row SD for M_1
ML$M_2_irv <- rowSds(as.matrix(ML[, 506:513]), na.rm = T) # Row SD for M_2

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
  select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
         Demo_Answ_1, Demo_Answ_2, Demo_Answ_3, Demo_Answ_4, Demo_Answ_5,
         E1_Answ_1, E1_Completion_Time_s, E1_Completion_Time_sc, E1_SF_OFF, E1_irv,
         E2_Answ_1, E2_Completion_Time_s, E2_Completion_Time_sc, E2_SF_OFF, E2_irv,
         E3_Answ_1, E3_Completion_Time_s, E3_Completion_Time_sc, E3_SF_OFF, E3_irv,
         E4_Answ_1, E4_Completion_Time_s, E4_Completion_Time_sc, E4_SF_OFF, E4_irv,
         E5_Answ_1, E5_Completion_Time_s, E5_Completion_Time_sc, E5_SF_OFF, E5_irv,
         E6_Answ_1, E6_Completion_Time_s, E6_Completion_Time_sc, E6_SF_OFF, E6_irv,
         E7_Answ_1, E7_Completion_Time_s, E7_Completion_Time_sc, E7_SF_OFF, E7_irv,
         E8_Answ_1, E8_Completion_Time_s, E8_Completion_Time_sc, E8_SF_OFF, E8_irv,
         M_1_Answ_1, M_1_Completion_Time_s, M_1_Completion_Time_sc, M_1_SF_OFF, M_1_irv,
         M_2_Answ_1, M_2_Completion_Time_s, M_2_Completion_Time_sc, M_2_SF_OFF, M_2_irv)

ML_long1 <- reshape(ML_sub, direction = "long", 
                   varying = c("E1_Answ_1", "E1_Completion_Time_s", "E1_Completion_Time_sc", "E1_SF_OFF", "E1_irv",
                               "E2_Answ_1", "E2_Completion_Time_s", "E2_Completion_Time_sc", "E2_SF_OFF", "E2_irv",
                               "E3_Answ_1", "E3_Completion_Time_s", "E3_Completion_Time_sc", "E3_SF_OFF", "E3_irv",
                               "E4_Answ_1", "E4_Completion_Time_s", "E4_Completion_Time_sc", "E4_SF_OFF", "E4_irv",
                               "E5_Answ_1", "E5_Completion_Time_s", "E5_Completion_Time_sc", "E5_SF_OFF", "E5_irv",
                               "E6_Answ_1", "E6_Completion_Time_s", "E6_Completion_Time_sc", "E6_SF_OFF", "E6_irv",
                               "E7_Answ_1", "E7_Completion_Time_s", "E7_Completion_Time_sc", "E7_SF_OFF", "E7_irv",
                               "E8_Answ_1", "E8_Completion_Time_s", "E8_Completion_Time_sc", "E8_SF_OFF", "E8_irv",
                               "M_1_Answ_1", "M_1_Completion_Time_s", "M_1_Completion_Time_sc", "M_1_SF_OFF", "M_1_irv",
                               "M_2_Answ_1", "M_2_Completion_Time_s", "M_2_Completion_Time_sc", "M_2_SF_OFF", "M_2_irv"), 
                   timevar = "page",
                   times = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "M_1", "M_2"),
                   v.names = c("Answ_1", "Completion_Time_s", "Completion_Time_sc", "SF_OFF", "irv"),
                   idvar = "ID")

names(ML_long1) <- c("ID", "Completed", "Lurker", "No_JavaScript", "Mobile_Device",
                     "Demo_Answ_1", "Demo_Answ_2", "Demo_Answ_3", "Demo_Answ_4", "Demo_Answ_5",
                     "page", "Answ_1", "Completion_Time_s", "Completion_Time_sc", "irv", "SF_OFF")

qs <- quantile(ML_long1$irv, probs = c(0, 0.1, 0.2, 0.5, 0.8, 0.9, 1), na.rm = T)
ML_long1$irv_p10 <- ifelse(ML_long1$irv <= qs[[2]], 1, 0)
ML_long1$irv_p20 <- ifelse(ML_long1$irv <= qs[[3]], 1, 0)
ML_long1$irv_p80 <- ifelse(ML_long1$irv >= qs[[5]], 1, 0)
ML_long1$irv_p90 <- ifelse(ML_long1$irv >= qs[[6]], 1, 0)

ML_long1 <-
  ML_long1 %>%
  mutate(pages = fct_collapse(page, 
                              Matrix = c("M_1", "M_2"),
                              Single = c("E1","E2","E3","E4","E5","E6","E7","E8"))) %>%
  mutate(sex = fct_recode(as.factor(Demo_Answ_1),
                             "male" = "1",
                             "female" = "2",
                             NULL = "0")) %>%
  mutate(Birthyear = ifelse(Demo_Answ_2 >= 1900, Demo_Answ_2, NA)) %>%
  mutate(age = 2017 - Birthyear) %>%
  mutate(age_s = scale(age)[,1]) %>%
  mutate(german = fct_recode(as.factor(Demo_Answ_5),
                             "german" = "1",
                             "not_german" = "2",
                             NULL = "0"))
  
Goe_long1 <-
  ML_long1 %>%
  left_join(Goe_SM, by = c("ID" = "lfdn", "page" = "page")) %>%
  arrange(ID, page)

# 07: Join with full data (long_2: one row per item)

# Primacy

list_in <- c("E1_Answ_1", "E2_Answ_1", "E3_Answ_1", "E4_Answ_1", "E5_Answ_1", "E6_Answ_1", "E7_Answ_1", "E8_Answ_1",
             "M_1_Answ_1", "M_1_Answ_2", "M_1_Answ_3", "M_1_Answ_4", "M_1_Answ_5", "M_1_Answ_6", "M_1_Answ_7", "M_1_Answ_8",
             "M_2_Answ_1", "M_2_Answ_2", "M_2_Answ_3", "M_2_Answ_4", "M_2_Answ_5", "M_2_Answ_6", "M_2_Answ_7", "M_2_Answ_8")
  
list_out <- c("E1_primacy", "E2_primacy", "E3_primacy", "E4_primacy", "E5_primacy", "E6_primacy", "E7_primacy", "E8_primacy",
             "M_1_1_primacy", "M_1_2_primacy", "M_1_3_primacy", "M_1_4_primacy", "M_1_5_primacy", "M_1_6_primacy", "M_1_7_primacy", "M_1_8_primacy",
             "M_2_1_primacy", "M_2_2_primacy", "M_2_3_primacy", "M_2_4_primacy", "M_2_5_primacy", "M_2_6_primacy", "M_2_7_primacy", "M_2_8_primacy")

ML[,list_out] <- lapply(ML[,list_in], function(x) {ifelse(x == 1, 1, 0)})

# Item nonresponse

list_out <- c("E1_NR", "E2_NR", "E3_NR", "E4_NR", "E5_NR", "E6_NR", "E7_NR", "E8_NR",
              "M_1_1_NR", "M_1_2_NR", "M_1_3_NR", "M_1_4_NR", "M_1_5_NR", "M_1_6_NR", "M_1_7_NR", "M_1_8_NR",
              "M_2_1_NR", "M_2_2_NR", "M_2_3_NR", "M_2_4_NR", "M_2_5_NR", "M_2_6_NR", "M_2_7_NR", "M_2_8_NR")

ML[,list_out] <- lapply(ML[,list_in], function(x) {ifelse(x == 0, 1, 0)})

# Join

ML_sub <- ML %>% 
  select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
         Demo_Answ_1, Demo_Answ_2, Demo_Answ_3, Demo_Answ_4, Demo_Answ_5,
         E1_primacy, E1_NR, 
         E2_primacy, E2_NR, 
         E3_primacy, E3_NR, 
         E4_primacy, E4_NR, 
         E5_primacy, E5_NR, 
         E6_primacy, E6_NR, 
         E7_primacy, E7_NR, 
         E8_primacy, E8_NR,
         M_1_1_primacy, M_1_1_NR, 
         M_1_2_primacy, M_1_2_NR, 
         M_1_3_primacy, M_1_3_NR, 
         M_1_4_primacy, M_1_4_NR, 
         M_1_5_primacy, M_1_5_NR, 
         M_1_6_primacy, M_1_6_NR, 
         M_1_7_primacy, M_1_7_NR, 
         M_1_8_primacy, M_1_8_NR,
         M_2_1_primacy, M_2_1_NR,
         M_2_2_primacy, M_2_2_NR, 
         M_2_3_primacy, M_2_3_NR, 
         M_2_4_primacy, M_2_4_NR, 
         M_2_5_primacy, M_2_5_NR, 
         M_2_6_primacy, M_2_6_NR, 
         M_2_7_primacy, M_2_7_NR, 
         M_2_8_primacy, M_2_8_NR)

ML_long2 <- reshape(ML_sub, direction = "long", 
                    varying = c("E1_primacy", "E1_NR", 
                                "E2_primacy", "E2_NR", 
                                "E3_primacy", "E3_NR", 
                                "E4_primacy", "E4_NR", 
                                "E5_primacy", "E5_NR", 
                                "E6_primacy", "E6_NR", 
                                "E7_primacy", "E7_NR", 
                                "E8_primacy", "E8_NR",
                                "M_1_1_primacy", "M_1_1_NR", 
                                "M_1_2_primacy", "M_1_2_NR", 
                                "M_1_3_primacy", "M_1_3_NR", 
                                "M_1_4_primacy", "M_1_4_NR", 
                                "M_1_5_primacy", "M_1_5_NR", 
                                "M_1_6_primacy", "M_1_6_NR", 
                                "M_1_7_primacy", "M_1_7_NR", 
                                "M_1_8_primacy", "M_1_8_NR",
                                "M_2_1_primacy", "M_2_1_NR",
                                "M_2_2_primacy", "M_2_2_NR", 
                                "M_2_3_primacy", "M_2_3_NR", 
                                "M_2_4_primacy", "M_2_4_NR", 
                                "M_2_5_primacy", "M_2_5_NR", 
                                "M_2_6_primacy", "M_2_6_NR", 
                                "M_2_7_primacy", "M_2_7_NR", 
                                "M_2_8_primacy", "M_2_8_NR"), 
                    timevar = "item",
                    times = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", 
                              "M_1_1", "M_1_2", "M_1_3", "M_1_4", "M_1_5", "M_1_6", "M_1_7", "M_1_8",
                              "M_2_1", "M_2_2", "M_2_3", "M_2_4", "M_2_5", "M_2_6", "M_2_7", "M_2_8"),
                    v.names = c("var1", "var2"),
                    idvar = "ID")

ML_long2 <-
  ML_long2 %>%
  rename(primacy = var1, NR = var2) %>%
  mutate(pages = fct_collapse(item, 
                              Matrix = c("M_1_1", "M_1_2", "M_1_3", "M_1_4", "M_1_5", "M_1_6", "M_1_7", "M_1_8",
                                         "M_2_1", "M_2_2", "M_2_3", "M_2_4", "M_2_5", "M_2_6", "M_2_7", "M_2_8"),
                              Single = c("E1", "E2", "E3","E4","E5","E6","E7","E8"))) %>%
  mutate(page = fct_recode(item, 
                           "M_1" = "M_1_1", "M_1" = "M_1_2", "M_1" = "M_1_3", "M_1" = "M_1_4", 
                           "M_1" = "M_1_5", "M_1" = "M_1_6", "M_1" = "M_1_7", "M_1" = "M_1_8",
                           "M_2" = "M_2_1", "M_2" = "M_2_2", "M_2" = "M_2_3", "M_2" = "M_2_4",
                           "M_2" = "M_2_5", "M_2" = "M_2_6", "M_2" = "M_2_7", "M_2" = "M_2_8",
                           "E1" = "E1", "E2" = "E2", "E3" = "E3", "E4" = "E4",
                           "E5" = "E5", "E6" = "E6", "E7" = "E7", "E8" = "E8")) %>%
  mutate(sex = fct_recode(as.factor(Demo_Answ_1),
                             "male" = "1",
                             "female" = "2",
                             NULL = "0")) %>%
  mutate(Birthyear = ifelse(Demo_Answ_2 >= 1900, Demo_Answ_2, NA)) %>%
  mutate(age = 2017 - Birthyear) %>%
  mutate(age_s = scale(age)[,1]) %>%
  mutate(german = fct_recode(as.factor(Demo_Answ_5),
                             "german" = "1",
                             "not_german" = "2",
                             NULL = "0")) 

Goe_long2 <- 
  ML_long2 %>%
  left_join(Goe_SM, by = c("ID" = "lfdn", "page" = "page")) %>%
  arrange(ID, page, item)

# 08: Join with full data (ac: only attention check)

# Attention check

ML_sub <- ML %>%
  mutate(AC_Answ = ifelse(AC_Answ_10 == 1 & AC_Answ_15 == 1, 1, 0)) %>%
  select(ID, Completed, Lurker, No_JavaScript, Mobile_Device, 
         Demo_Answ_1, Demo_Answ_2, Demo_Answ_3, Demo_Answ_4, Demo_Answ_5,
         AC_Answ, AC_Completion_Time, AC_SF_OFF)

ML_ac <-
  ML_sub %>%
  mutate(page = "AC") %>%
  mutate(sex = fct_recode(as.factor(Demo_Answ_1),
                             "male" = "1",
                             "female" = "2",
                             NULL = "0")) %>%
  mutate(Birthyear = ifelse(Demo_Answ_2 >= 1900, Demo_Answ_2, NA)) %>%
  mutate(age = 2017 - Birthyear) %>%
  mutate(age_s = scale(age)[,1]) %>%
  mutate(german = fct_recode(as.factor(Demo_Answ_5),
                             "german" = "1",
                             "not_german" = "2",
                             NULL = "0"))

Goe_SM <- mutate(Goe_SM, page = fct_recode(page, 
                                           "AC" = "v_639"))

# Join

Goe_ac <- 
  ML_ac %>%
  left_join(Goe_SM, by = c("ID" = "lfdn", "page" = "page")) %>%
  arrange(ID)

save(Goe_SM, Goe_long1, Goe_long2, Goe_ac, file = "./src/output2.Rdata")
