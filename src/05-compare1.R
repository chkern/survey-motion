##################
# Survey Motion
# Part IIIb: Compare Motion Groups in Test Set (Respondi data)
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
load("./src/output4.Rdata")

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

gt4 <- ggplot(resp_long1) +
  geom_density(aes(x = SM_mean)) +
  xlim(0, 4) +
  labs(title = "Test data") +
  theme_light(base_size = 9)

gt5 <- ggplot(resp_long1) +
  geom_density(aes(x = SM_var)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)

gt6 <- ggplot(resp_long1) +
  geom_density(aes(x = SM_max)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p5_TA_distributions.pdf", plots, width = 8, height = 6)

## 03: Class prediction

ggplot(resp_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l1), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p5_class_preds1.pdf", width = 4, height = 5.5)

resp_long1 %>%
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

ggsave("p5_class_preds1_2.pdf", width = 5, height = 5.5)

ggplot(resp_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l2), na.rm = TRUE) +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p5_class_preds2.pdf", width = 4, height = 5.5)

resp_long1 %>%
  drop_na(p_rf_l2) %>%
  ggplot() + 
  geom_bar(aes(x = page, fill = p_rf_l2), position = "fill", alpha = 0.85)  +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_light(base_size = 12)  +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p5_class_preds2_2.pdf", width = 5.1, height = 5.5)

## 03b: Sequence plots

## 04: Compare groups (page level)
# Completion Times - plots

g3 <- resp_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "Completion Time", x = "") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

g4 <- resp_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("M_1", "M_2")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "", x = "") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  guides(color = guide_legend(title = "")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

plots <- arrangeGrob(g3, g4, nrow = 1)
ggsave("p5_resp_times.pdf", plots, width = 7, height = 5.5)

# Completion Times - models

resp_long1$age_s <- scale(2019 - resp_long1$Birthyear)

m0 <- lmer(Completion_Time_sc ~ (1 | ID) + (1 | page), data = resp_long1)
m1 <- lmer(Completion_Time_sc ~ p_rf_l2 + (1 | ID) + (1 | page), data = resp_long1)
m2 <- lmer(Completion_Time_sc ~ p_rf_l2 + sex + age_s + (1 | ID) + (1 | page), data = resp_long1)
m3 <- lmer(Completion_Time_sc ~ p_rf_l2 + pages + sex + age_s + (1 | ID) + (1 | page), data = resp_long1)
m4 <- lmer(Completion_Time_sc ~ p_rf_l2*pages + sex + age_s + (1 | ID) + (1 | page), data = resp_long1)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"

stargazer(m1, m2, m3, m4, keep = c("Constant", "p_rf_l2", "pages", "p_rf_l2Moving:pagesMatrix"), report = ('vcsp'),
          add.lines = list(c("Demographic controls", "", "X", "X", "X")), title = "Mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5_resp_times_m.tex")

# Intra-individual response variability - models

m0a <- glmer(irv_p20 ~ (1 | ID), family = binomial, data = resp_long1)
m1a <- glmer(irv_p20 ~ p_rf_l2 + (1 | ID), family = binomial, data = resp_long1)
m2a <- glmer(irv_p20 ~ p_rf_l2 + sex + age_s + (1 | ID), family = binomial, data = resp_long1)

m0b <- glmer(irv_p80 ~ (1 | ID), family = binomial, data = resp_long1)
m1b <- glmer(irv_p80 ~ p_rf_l2 + (1 | ID), family = binomial, data = resp_long1)
m2b <- glmer(irv_p80 ~ p_rf_l2 + sex + age_s + (1 | ID), family = binomial, data = resp_long1)

summary(m1a)
summary(m2a)
summary(m1b)
summary(m2b)

class(m2a) <- "lmerMod"
class(m2b) <- "lmerMod"

stargazer(m2a, m2b, keep = c("Constant", "p_rf_l2"), report = ('vcsp'),
          add.lines = list(c("Demographic controls", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5_irv_m.tex")

## 05: Compare groups (item level)
# Primacy effects - models

resp_long2$age_s <- scale(2019 - resp_long2$Birthyear)

m0 <- glmer(primacy ~ (1 | ID) + (1 | page), family = binomial, data = resp_long2)
m1 <- glmer(primacy ~ p_rf_l2 + (1 | ID) + (1 | page), family = binomial, data = resp_long2)
m2 <- glmer(primacy ~ p_rf_l2 + sex + age_s + (1 | ID) + (1 | page), family = binomial, data = resp_long2)
m3 <- glmer(primacy ~ p_rf_l2 + pages + sex + age_s + (1 | ID) + (1 | page), family = binomial, data = resp_long2)
m4 <- glmer(primacy ~ p_rf_l2*pages + sex + age_s + (1 | ID) + (1 | page), family = binomial, data = resp_long2)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"

stargazer(m2, m3, m4, keep = c("Constant", "p_rf_l2", "pages", "p_rf_l2Moving:pagesMatrix"), report = ('vcsp'),
          add.lines = list(c("Demographic controls", "X", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5_primacy_m.tex")

## 06: Compare groups (respondent level)
# Attention check - models
