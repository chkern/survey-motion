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
library(car)
library(lme4)
library(lmerTest)
library(xtable)
library(stargazer)

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

gt4 <- ggplot(G_test_long1) +
  geom_density(aes(x = SM_mean)) +
  xlim(0, 4) +
  labs(title = "Test data") +
  theme_light(base_size = 9)

gt5 <- ggplot(G_test_long1) +
  geom_density(aes(x = SM_var)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)

gt6 <- ggplot(G_test_long1) +
  geom_density(aes(x = SM_max)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p4_TA_distributions.pdf", plots, width = 8, height = 6)

## 03: Class prediction

g1 <- ggplot(G_test_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l1), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds1.pdf", width = 4, height = 5.5)

g2 <- G_test_long1 %>%
  drop_na(p_rf_l1) %>%
  ggplot() + 
  geom_bar(aes(x = page, fill = p_rf_l1), position = "fill", alpha = 0.85)  +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_light(base_size = 12)  +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds1_2.pdf", width = 5, height = 5.5)

g3 <- ggplot(G_test_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l2), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds2.pdf", width = 4, height = 5.5)

g4 <- G_test_long1 %>%
  drop_na(p_rf_l2) %>%
  ggplot() + 
  geom_bar(aes(x = page, fill = p_rf_l2), position = "fill", alpha = 0.85)  +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_light(base_size = 12)  +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds2_2.pdf", width = 5.1, height = 5.5)

## 04: Compare groups (page level)
# Completion Times - plots

g3 <- G_test_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("Single_E1", "Single_E2", "Single_E3", "Single_E4", "Single_E5", "Single_E6", "Single_E7", "Single_E8")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "Completion Time", x = "") +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

g4 <- G_test_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("Matrix_1", "Matrix_2")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "", x = "") +
  guides(color = guide_legend(title = "")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

plots <- arrangeGrob(g3, g4, nrow = 1)
ggsave("p4_resp_times.pdf", plots, width = 7, height = 5.5)

# Completion Times - tests

G_test_lf1 <- filter(G_test_long1, page %in% c("Single_E1", "Single_E2", "Single_E3", "Single_E4", "Single_E5", "Single_E6", "Single_E7", "Single_E8"))

t1a <- tidy(leveneTest(Completion_Time_sc ~ p_rf_l2, data = G_test_lf1))
t1b <- tidy(t.test(Completion_Time_sc ~ p_rf_l2, data = G_test_lf1))
t1c <- tidy(wilcox.test(Completion_Time_sc ~ p_rf_l2, data = G_test_lf1))
t1d <- by(G_test_lf1$Completion_Time_sc, G_test_lf1$p_rf_l2, median, na.rm = T)

G_test_lf2 <- filter(G_test_long1, page %in% c("Matrix_1", "Matrix_2"))

t2a <- tidy(leveneTest(Completion_Time_sc ~ p_rf_l2, data = G_test_lf2))
t2b <- tidy(t.test(Completion_Time_sc ~ p_rf_l2, data = G_test_lf2))
t2c <- tidy(wilcox.test(Completion_Time_sc ~ p_rf_l2, data = G_test_lf2))
t2d <- by(G_test_lf2$Completion_Time_sc, G_test_lf2$p_rf_l2, median, na.rm = T)

dat <- rbind(t1b[, 2:5], t2b[, 2:5])
dat <- add_column(dat, var = c("Single", "Grid"), .before = 1)
colnames(dat) <- c("", "m(moving)", "m(not moving)", "statistic", "p.value")

tab <- xtable(dat, digits = 3)
print(tab, type = "latex", file = "t4_resp_times.tex")

# Completion Times - models

G_test_long1$age_s <- scale(G_test_long1$age)

m0 <- lmer(Completion_Time_sc ~ (1 | ID) + (1 | page), data = G_test_long1)
m1 <- lmer(Completion_Time_sc ~ p_rf_l2 + (1 | ID) + (1 | page), data = G_test_long1)
m2 <- lmer(Completion_Time_sc ~ p_rf_l2 + gender + age_s + german + (1 | ID) + (1 | page), data = G_test_long1)
m3 <- lmer(Completion_Time_sc ~ p_rf_l2 + pages + gender + age_s + german + (1 | ID) + (1 | page), data = G_test_long1)
m4 <- lmer(Completion_Time_sc ~ p_rf_l2*pages + gender + age_s + german + (1 | ID) + (1 | page), data = G_test_long1)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"

stargazer(m1, m2, m3, m4, keep = c("Constant", "p_rf_l2", "pages", "p_rf_l2Moving:pagesMatrix"),
          add.lines = list(c("Demographic controls", "", "X", "X", "X")), title = "Mixed effects regressions", 
          omit.stat = c("ll", "aic"), align = TRUE, no.space = TRUE, out.header = T, out = "t4_resp_times_m.tex")

# Intra-individual response variability - tests

G_test_long1$irv[G_test_long1$irv == 0 & G_test_long1$Answ_1 == 0] <- NA

t1 <- tidy(leveneTest(irv ~ p_rf_l2, data = G_test_long1))
t2 <- tidy(t.test(irv ~ p_rf_l2, data = G_test_long1))

qs <- quantile(G_test_long1$irv, probs = c(0, 0.1, 0.2, 0.5, 0.8, 0.9, 1), na.rm = T)
G_test_long1$irv_p20 <- ifelse(G_test_long1$irv <= qs[[3]], 1, 0)
G_test_long1$irv_p10 <- ifelse(G_test_long1$irv <= qs[[2]], 1, 0)
G_test_long1$irv_p90 <- ifelse(G_test_long1$irv >= qs[[6]], 1, 0)
G_test_long1$irv_p80 <- ifelse(G_test_long1$irv >= qs[[5]], 1, 0)

t3 <- tidy(chisq.test(G_test_long1$p_rf_l2, G_test_long1$irv_p20))
t4 <- tidy(chisq.test(G_test_long1$p_rf_l2, G_test_long1$irv_p80))
t5 <- by(G_test_long1$irv_p20, G_test_long1$p_rf_l2, table)
t6 <- by(G_test_long1$irv_p80, G_test_long1$p_rf_l2, table)

# Intra-individual response variability - models

m0a <- glmer(irv_p20 ~ (1 | ID), family = binomial, data = G_test_long1)
m1a <- glmer(irv_p20 ~ p_rf_l2 + (1 | ID), family = binomial, data = G_test_long1)
m2a <- glmer(irv_p20 ~ p_rf_l2 + gender + age_s + german + (1 | ID), family = binomial, data = G_test_long1)

m0b <- glmer(irv_p80 ~ (1 | ID), family = binomial, data = G_test_long1)
m1b <- glmer(irv_p80 ~ p_rf_l2 + (1 | ID), family = binomial, data = G_test_long1)
m2b <- glmer(irv_p80 ~ p_rf_l2 + gender + age_s + german + (1 | ID), family = binomial, data = G_test_long1)

summary(m1a)
summary(m2a)
summary(m1b)
summary(m2b)

class(m2a) <- "lmerMod"
class(m2b) <- "lmerMod"

stargazer(m2a, m2b, keep = c("Constant", "p_rf_l2"),
          add.lines = list(c("Demographic controls", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), align = TRUE, no.space = TRUE, out.header = T, out = "t4_irv_m.tex")

## 05: Compare groups (item level)

# Primacy effects - tests

t1 <- tidy(chisq.test(G_test_long2$p_rf_l2, G_test_long2$primacy))
t2 <- by(G_test_long2$primacy, G_test_long2$p_rf_l2, table)

# Primacy effects - models

G_test_long2$age_s <- scale(G_test_long2$age)

m0 <- glmer(primacy ~ (1 | ID) + (1 | page), family = binomial, data = G_test_long2)
m1 <- glmer(primacy ~ p_rf_l2 + (1 | ID) + (1 | page), family = binomial, data = G_test_long2)
m2 <- glmer(primacy ~ p_rf_l2 + gender + age_s + german + (1 | ID) + (1 | page), family = binomial, data = G_test_long2)
m3 <- glmer(primacy ~ p_rf_l2 + pages + gender + age_s + german + (1 | ID) + (1 | page), family = binomial, data = G_test_long2)
m4 <- glmer(primacy ~ p_rf_l2*pages + gender + age_s + german + (1 | ID) + (1 | page), family = binomial, data = G_test_long2)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"

stargazer(m2, m3, m4, keep = c("Constant", "p_rf_l2", "pages", "p_rf_l2Moving:pagesMatrix"),
          add.lines = list(c("Demographic controls", "X", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), align = TRUE, no.space = TRUE, out.header = T, out = "t4_primacy_m.tex")

## 06: Compare groups (respondent level)
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

# Survey Focus, Item Nonresponse

G_test_wide %>%
  drop_na(p_rf_l2m) %>%
  group_by(p_rf_l2m) %>%
  rename(p_rf = p_rf_l2m) %>%
  summarise(E1_SF_OFF = mean(E1_SF_OFF), E2_SF_OFF = mean(E2_SF_OFF), 
            E1_SF_OFF = mean(E3_SF_OFF), E4_SF_OFF = mean(E4_SF_OFF), 
            E5_SF_OFF = mean(E5_SF_OFF), E6_SF_OFF = mean(E6_SF_OFF), 
            E7_SF_OFF = mean(E7_SF_OFF), E8_SF_OFF = mean(E8_SF_OFF), 
            M_1_SF_OFF = mean(M_1_SF_OFF), M_2_SF_OFF = mean(M_2_SF_OFF),
            Missings = mean(E1_M2_Missing))
