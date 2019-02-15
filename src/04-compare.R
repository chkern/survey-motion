##################
# Survey Motion
# Part III: Compare Motion Groups in Test Set
# Christoph Kern
# R 3.5.1
##################

## 01: Setup

library(tidyverse)
library(broom)
library(caret)
library(gridExtra)
library(ggmosaic)
library(xtable)

setwd("E:\\Uni\\Forschung\\Article\\2019 - MASS")
load("./src/output1.Rdata")
load("./src/output3.Rdata")

## 02: Data check

gt1 <- ggplot(sm) +
  geom_density(aes(x = SM_mean, color = D_group)) +
  xlim(0, 4) +
  labs(title = "Training data") +
  theme_light(base_size = 9)  +
  theme(legend.position = "none")

gt2 <- ggplot(sm) +
  geom_density(aes(x = SM_var, color = D_group)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)  +
  theme(legend.position = "none")

gt3 <- ggplot(sm) +
  geom_density(aes(x = SM_max, color = D_group)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)  +
  theme(legend.position = c(0.75, 0.8),
        legend.text = element_text(size = 6),
        legend.title = element_blank())

gt4 <- ggplot(G_test_long) +
  geom_density(aes(x = SM_mean)) +
  xlim(0, 4) +
  labs(title = "Test data") +
  theme_light(base_size = 9)

gt5 <- ggplot(G_test_long) +
  geom_density(aes(x = SM_var)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)

gt6 <- ggplot(G_test_long) +
  geom_density(aes(x = SM_max)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p4_TA_distributions.pdf", plots, width = 8, height = 6)

## 03: Class prediction

g1 <- ggplot(G_test_long) +
  geom_mosaic(aes(x = product(p_rf_l1, page), fill = p_rf_l1), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light()  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds1.pdf", width = 4, height = 5.5)

g2 <- ggplot(G_test_long) +
  geom_mosaic(aes(x = product(p_rf_l2, page), fill = p_rf_l2), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light()  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds2.pdf", width = 4, height = 5.5)

## 04: Compare groups (page level)
# Completion Times - plots

g3 <- G_test_long %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("Single_E1", "Single_E2", "Single_E3", "Single_E4", "Single_E5", "Single_E6", "Single_E7", "Single_E8")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_s, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "Completion Time", x = "") +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

g4 <- G_test_long %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("Matrix_1", "Matrix_2")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_s, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "", x = "") +
  guides(color = guide_legend(title = "")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

plots <- arrangeGrob(g3, g4, nrow = 1)
ggsave("p4_resp_times.pdf", plots, width = 7, height = 5.5)

# Completion Times - table

t1 <- G_test_long %>%
  filter(page %in% c("Single_E1", "Single_E2", "Single_E3", "Single_E4", "Single_E5", "Single_E6", "Single_E7", "Single_E8")) %>%
  t.test(Completion_Time_s ~ p_rf_l2, data = .) %>%
  tidy(.)

t2 <- G_test_long %>%
  filter(page %in% c("Matrix_1", "Matrix_2")) %>%
  t.test(Completion_Time_s ~ p_rf_l2, data = .) %>%
  tidy(.)

dat <- rbind(t1[, 2:5], t2[, 2:5])
dat <- add_column(dat, var = c("Single", "Grid"), .before = 1)
colnames(dat) <- c("", "m(moving)", "m(not moving)", "statistic", "p.value")

tab <- xtable(dat, digits = 3)
print(tab, type = "latex", file = "t4_resp_times.tex")

## 05: Compare groups (respondent level)
# Attention check, Multitasking 1 + 2

d <- G_test_wide %>%
  drop_na(p_rf_l2m) %>%
  group_by(p_rf_l2m) %>%
  summarise(AC = mean(AC_Answ, na.rm = T), Multitasking_1 = mean(Multitasking_1_Answ, na.rm = T), Multitasking_2 = mean(Multitasking_2_Answ, na.rm = T))

t1 <- tidy(chisq.test(G_test_wide$p_rf_l2m, G_test_wide$AC_Answ))
t2 <- tidy(chisq.test(G_test_wide$p_rf_l2m, G_test_wide$Multitasking_1_Answ, simulate.p.value = T))
t3 <- tidy(t.test(Multitasking_2_Answ ~ p_rf_l2m, data = G_test_wide))

tab1 <- t(d[,2:4])
tab2 <- rbind(t1[1:2], t2[1:2], t3[4:5])
dat <- cbind(tab1, tab2)
colnames(dat) <- c("m(moving)", "m(not moving)", "statistic", "p.value")

tab <- xtable(dat, digits = 3)
print(tab, type = "latex", file = "t4_AC_multitask.tex")

# Motivation, Survey Evaluation

t1 <- tidy(t.test(Motivation_Answ_1 ~ p_rf_l2m, data = G_test_wide))
t2 <- tidy(t.test(SemDiff_Answ_1 ~ p_rf_l2m, data = G_test_wide))
t3 <- tidy(t.test(SemDiff_Answ_2 ~ p_rf_l2m, data = G_test_wide))
t4 <- tidy(t.test(SemDiff_Answ_3 ~ p_rf_l2m, data = G_test_wide))
t5 <- tidy(t.test(SemDiff_Answ_4 ~ p_rf_l2m, data = G_test_wide))
t6 <- tidy(t.test(SemDiff_Answ_5 ~ p_rf_l2m, data = G_test_wide))

dat <- rbind(t1[, 2:5], t2[, 2:5], t3[, 2:5], t4[, 2:5], t5[, 2:5], t6[, 2:5])
dat <- add_column(dat, var = c("Motivation", "Boring", "Difficult", "Exhausting", "Complicated", "Monotonous"), .before = 1)
colnames(dat) <- c("", "m(moving)", "m(not moving)", "statistic", "p.value")

tab <- xtable(dat, digits = 3)
print(tab, type = "latex", file = "t4_svy_eval.tex")

# Item Nonresponse, Break-Off

# Response styles
