##################
# Survey Motion
# Part IIIc: Compare Motion Groups in Test Set (Respondi and Goettingen data)
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
load("./src/output2.Rdata")
load("./src/output4.Rdata")

# Combine surveys

Goe_long1 <- Goe_long1 %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2")) %>%
  select(ID, page, pages, sex, Birthyear, age, age_s, datetime,
         Completion_Time_s, Completion_Time_sc, irv, irv_p10, irv_p20, irv_p80, irv_p90,
         SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

resp_long1 <- select(resp_long1, ID, page, pages, sex, Birthyear, age, age_s, datetime,
                    Completion_Time_s, Completion_Time_sc, irv, irv_p10, irv_p20, irv_p80, irv_p90,
                    SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

com_long1 <- bind_rows("Survey one" = Goe_long1, "Survey two" = resp_long1, .id = "survey")
com_long1$age_s <- scale(com_long1$age)[,1] # Re-scale age

Goe_long2 <- Goe_long2 %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2")) %>%
  select(ID, item, page, pages, sex, Birthyear, age, age_s, datetime,
         primacy, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

resp_long2 <- select(resp_long2, ID, item, page, pages, sex, Birthyear, age, age_s, datetime,
                     primacy, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

com_long2 <- bind_rows("Survey one" = Goe_long2, "Survey two" = resp_long2, .id = "survey")
com_long2$age_s <- scale(com_long2$age)[,1] # Re-scale age

## 02: Data check

gt1 <- ggplot(sm) +
  geom_density(aes(x = SM_mean, color = D_group)) +
  xlim(0, 4) +
  labs(title = "Training data") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gt2 <- ggplot(sm) +
  geom_density(aes(x = SM_var, color = D_group)) +
  xlim(0, 2) +
  labs(title = "") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gt3 <- ggplot(sm) +
  geom_density(aes(x = SM_max, color = D_group)) +
  xlim(0, 20) +
  labs(title = "") +
  theme(legend.position = c(0.75, 0.8),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        text = element_text(size = 10))

gt4 <- ggplot(com_long1) +
  geom_density(aes(x = SM_mean, color = survey)) +
  xlim(0, 4) +
  labs(title = "Survey data") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gt5 <- ggplot(com_long1) +
  geom_density(aes(x = SM_var, color = survey)) +
  xlim(0, 2) +
  labs(title = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gt6 <- ggplot(com_long1) +
  geom_density(aes(x = SM_max, color = survey)) +
  xlim(0, 20) +
  labs(title = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = c(0.75, 0.8),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        text = element_text(size = 10))

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p5b_TA_distributions.pdf", plots, width = 9, height = 6)

## 03: Class prediction

com_long1 %>%
  drop_na(p_rf_l1) %>%
  ggplot() + 
  geom_bar(aes(x = page, fill = p_rf_l1), position = "fill", alpha = 0.85)  +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  facet_grid(. ~ survey)

ggsave("p5b_class_preds1.pdf", width = 8, height = 6)

com_long1 %>%
  drop_na(p_rf_l2) %>%
  ggplot() + 
  geom_bar(aes(x = page, fill = p_rf_l2), position = "fill", alpha = 0.85)  +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  facet_grid(. ~ survey)

ggsave("p5b_class_preds2.pdf", width = 8, height = 6)

## 03b: Sequence plots

## 04: Compare groups (page level)
# Completion Times - plots

g3 <- com_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "Completion Time", x = "") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  facet_grid(. ~ survey)

g4 <- com_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("M_1", "M_2")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "", x = "") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  guides(color = guide_legend(title = "")) +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1)) +
  facet_grid(. ~ survey)

plots <- arrangeGrob(g3, g4, nrow = 1)
ggsave("p5b_resp_times.pdf", plots, width = 9, height = 6)

# Completion Times - models

m0 <- lmer(Completion_Time_sc ~ (1 | ID) + (1 | page), data = com_long1)
m1 <- lmer(Completion_Time_sc ~ p_rf_l2 + survey + (1 | ID) + (1 | page), data = com_long1)
m2 <- lmer(Completion_Time_sc ~ p_rf_l2 + sex + age_s + survey + (1 | ID) + (1 | page), data = com_long1)
m3 <- lmer(Completion_Time_sc ~ p_rf_l2 + pages + sex + age_s + survey + (1 | ID) + (1 | page), data = com_long1)
m4 <- lmer(Completion_Time_sc ~ p_rf_l2*pages + sex + age_s + survey + (1 | ID) + (1 | page), data = com_long1)
m5 <- lmer(Completion_Time_sc ~ p_rf_l2*survey + pages + sex + age_s + (1 | ID) + (1 | page), data = com_long1)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"
class(m5) <- "lmerMod"

stargazer(m1, m2, m3, m4, m5, 
          keep = c("Constant", "p_rf_l2", "pages", "survey"),
          report = ('vcsp'), add.lines = list(c("Demographic controls", "", "X", "X", "X", "X")), title = "Mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_resp_times_m.html")

# Intra-individual response variability - models

m0a <- glmer(irv_p20 ~ (1 | ID), family = binomial, data = com_long1)
m1a <- glmer(irv_p20 ~ p_rf_l2 + survey + (1 | ID), family = binomial, data = com_long1)
m2a <- glmer(irv_p20 ~ p_rf_l2 + sex + age_s + survey + (1 | ID), family = binomial, data = com_long1)
m3a <- glmer(irv_p20 ~ p_rf_l2*survey + sex + age_s + (1 | ID), family = binomial, data = com_long1)

m0b <- glmer(irv_p80 ~ (1 | ID), family = binomial, data = com_long1)
m1b <- glmer(irv_p80 ~ p_rf_l2 + survey + (1 | ID), family = binomial, data = com_long1)
m2b <- glmer(irv_p80 ~ p_rf_l2 + sex + age_s + survey + (1 | ID), family = binomial, data = com_long1)
m3b <- glmer(irv_p80 ~ p_rf_l2*survey + sex + age_s + (1 | ID), family = binomial, data = com_long1)

summary(m1a)
summary(m2a)
summary(m3a)
summary(m1b)
summary(m2b)
summary(m3b)

class(m1a) <- "lmerMod"
class(m2a) <- "lmerMod"
class(m3a) <- "lmerMod"
class(m1b) <- "lmerMod"
class(m2b) <- "lmerMod"
class(m3b) <- "lmerMod"

stargazer(m1a, m2a, m3a, m1b, m2b, m3b, 
          keep = c("Constant", "p_rf_l2", "survey"), 
          report = ('vcsp'), add.lines = list(c("Demographic controls", "", "X", "X", "", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_irv_m.html")

## 05: Compare groups (item level)
# Primacy effects - models

m0 <- glmer(primacy ~ (1 | ID) + (1 | page), family = binomial, data = com_long2)
m1 <- glmer(primacy ~ p_rf_l2 + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2)
m2 <- glmer(primacy ~ p_rf_l2 + sex + age_s + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2)
m3 <- glmer(primacy ~ p_rf_l2 + pages + sex + age_s + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2)
m4 <- glmer(primacy ~ p_rf_l2*pages + sex + age_s + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2)
m5 <- glmer(primacy ~ p_rf_l2*survey + sex + age_s + pages + (1 | ID) + (1 | page), family = binomial, data = com_long2)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"
class(m5) <- "lmerMod"

stargazer(m1, m2, m3, m4, m5,
          keep = c("Constant", "p_rf_l2", "pages", "survey"), 
          report = ('vcsp'), add.lines = list(c("Demographic controls", "", "X", "X", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_primacy_m.html")

## 06: Compare groups (respondent level)
# Attention check - models
