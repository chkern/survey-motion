##################
# Survey Motion
# Part II: Apply Prediction Models on Test Set (ESDQP)
# Christoph Kern
# R 3.4.4
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
ESDQP <- read.csv("./data/ESDQP_with_out_G.csv", sep = "|", header = T)

# Pre-processing

ESDQP_long <- 
  ESDQP %>% 
  gather(key = "page", value = "SM", 15:54) %>%
  arrange(lfdn, as.numeric(as.character(page)))

ESDQP_long <- filter(ESDQP_long, SM != "")
names <- paste0("SM_", 1:15000)

datalist <- list()
for (i in ESDQP$lfdn) {
  datalist[[i]] <- separate(ESDQP_long[ESDQP_long$lfdn == i,], 
                            col = SM, 
                            sep = ",",
                            into = names,
                            fill = "right",
                            convert = TRUE,
                            remove = FALSE)
  datalist[[i]] <- datalist[[i]][, colSums(is.na(datalist[[i]])) != nrow(datalist[[i]])]
  print(i)
}
ESDQP_long <- rbindlist(datalist, fill = TRUE)

ESDQP_long$SM_1 <- as.numeric(ESDQP_long$SM_1)
saveRDS(ESDQP_long, file = "./data/ESDQP_Test.RDS")

# X

motionvars <- grep("SM_" , names(ESDQP_long), value = TRUE)

ESDQP_SM <-
  ESDQP_long %>%
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
  bind_cols(ESDQP_long[, 1:28], .)

ESDQP_SM[which(ESDQP_SM$SM_max == 0), 29:42] <- NA
ESDQP_SM_drop <- drop_na(ESDQP_SM, 29:42)

## 02: Predict

pr_glmnet_l1 <- predict(glmnet_l1, newdata = ESDQP_SM_drop, type = "prob")
pr_ctree_l1 <- predict(ctree_l1, newdata = ESDQP_SM_drop, type = "prob")
pr_rf_l1 <- predict(rf_l1, newdata = ESDQP_SM_drop, type = "prob")
pr_xgb_l1 <- predict(xgb_l1, newdata = ESDQP_SM_drop, type = "prob")

p_glmnet_l1 <- predict(glmnet_l1, newdata = ESDQP_SM_drop)
p_ctree_l1 <- predict(ctree_l1, newdata = ESDQP_SM_drop)
p_rf_l1 <- predict(rf_l1, newdata = ESDQP_SM_drop)
p_xgb_l1 <- predict(xgb_l1, newdata = ESDQP_SM_drop)

pr_glmnet_l2 <- predict(glmnet_l2, newdata = ESDQP_SM_drop, type = "prob")
pr_ctree_l2 <- predict(ctree_l2, newdata = ESDQP_SM_drop, type = "prob")
pr_rf_l2 <- predict(rf_l2, newdata = ESDQP_SM_drop, type = "prob")
pr_xgb_l2 <- predict(xgb_l2, newdata = ESDQP_SM_drop, type = "prob")

p_glmnet_l2 <- predict(glmnet_l2, newdata = ESDQP_SM_drop)
p_ctree_l2 <- predict(ctree_l2, newdata = ESDQP_SM_drop)
p_rf_l2 <- predict(rf_l2, newdata = ESDQP_SM_drop)
p_xgb_l2 <- predict(xgb_l2, newdata = ESDQP_SM_drop)

ESDQP_SM_drop <- 
  ESDQP_SM_drop %>%
  add_column(., 
             p_glmnet_l1, p_ctree_l1, p_rf_l1, p_xgb_l1,
             p_glmnet_l2, p_ctree_l2, p_rf_l2, p_xgb_l2) %>%
  select(lfdn, page, 43:50)

ESDQP_SM <- left_join(ESDQP_SM, ESDQP_SM_drop, by = c("lfdn", "page"))

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

save(ESDQP_SM, file = "./src/output5.Rdata")
