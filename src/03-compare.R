##################
# Survey Motion
# Part IIIa: Compare Motion Groups in Test Set (Goettingen data)
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

gt4 <- ggplot(Goe_long1) +
  geom_density(aes(x = SM_mean)) +
  xlim(0, 4) +
  labs(title = "Test data") +
  theme_light(base_size = 9)

gt5 <- ggplot(Goe_long1) +
  geom_density(aes(x = SM_var)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)

gt6 <- ggplot(Goe_long1) +
  geom_density(aes(x = SM_max)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p3_TA_distributions.pdf", plots, width = 8, height = 6)

## 03: Class prediction

ggplot(Goe_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l1), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p4_class_preds1.pdf", width = 4, height = 5.5)

Goe_long1 %>%
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

ggsave("p3_class_preds1_2.pdf", width = 5, height = 5.5)

ggplot(Goe_long1) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l2), na.rm = TRUE) +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p3_class_preds2.pdf", width = 4, height = 5.5)

Goe_long1 %>%
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

ggsave("p3_class_preds2_2.pdf", width = 5.1, height = 5.5)

## 03b: Sequence plots

fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

Goe_SM$page_o <- fct_reorg(Goe_SM$page,
                            "E_Intro" = "v_279", "Single_E6" = "E6",
                            "Single_E7" = "E7", "Single_E8" = "E8",
                            "Single_E1" = "E1", "Single_E2" = "E2",
                            "Single_E3" = "E3", "Single_E4" = "E4",
                            "Single_E5" = "E5", "M_Intro" = "v_549",
                            "Matrix_1" = "M_1", "Matrix_2" = "M_2",
                            "AC" = "AC", "Pers_Intro" = "v_669",
                            "SemDiff" = "v_699", "Particip" = "v_759",
                            "Multi_1" = "v_789", "Multi_2" = "v_819",
                            "Demo" = "v_849", "Device" = "v_879",
                            "Device_Tablet" = "v_909", "Device_iPad" = "v_939",
                            "Device_Smart" = "v_1179", "Device_iPhone" = "v_1209",
                            "Connection" = "v_969", "Con_Mobile" = "v_999",
                            "Place" = "v_1029", "Big5_Intro" = "v_1059",
                            "NfC" = "v_1089", "Big5" = "v_1119",
                            "Final" = "v_1149")

Goe_SM <-
  Goe_SM %>%
  group_by(lfdn) %>%
  mutate(mover = ifelse(mean(as.numeric(p_rf_l2)) > 1, "mover", "non_mover")) %>%
  mutate(sum_moves = sum(as.numeric(p_rf_l2[p_rf_l2 == "Moving"]))/2) %>%
  ungroup

Goe_SM$lfdn_o <- reorder(Goe_SM$lfdn, -Goe_SM$sum_moves)

Goe_SM %>%
  filter(mover == "mover") %>%
  ggplot() +
  geom_tile(aes(x = page_o, y = lfdn_o, fill = p_rf_l2)) +
  labs(x = "Page", y = "ID") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light(base_size = 10) +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p3_sequences_1.pdf", width = 9, height = 6)

Goe_SM %>%
  filter(mover == "mover") %>%
  ggplot() +
  geom_tile(aes(x = page_o, y = lfdn_o, fill = p_rf_l1)) +
  labs(x = "Page", y = "ID") +
  theme_light(base_size = 10) +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p3_sequences_2.pdf", width = 9, height = 6)

## 04: Compare groups (page level)
# Completion Times - plots

g3 <- Goe_long1 %>%
  drop_na(p_rf_l2) %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")) %>%
  ggplot() +
  geom_boxplot(aes(y = Completion_Time_sc, x = page, color = p_rf_l2), outlier.size = 0.1) +
  labs(y = "Completion Time", x = "") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

g4 <- Goe_long1 %>%
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
ggsave("p3_resp_times.pdf", plots, width = 7, height = 5.5)

# Completion Times - models

m0 <- lmer(Completion_Time_sc ~ (1 | ID) + (1 | page), data = Goe_long1)
m1 <- lmer(Completion_Time_sc ~ p_rf_l2 + (1 | ID) + (1 | page), data = Goe_long1)
m2 <- lmer(Completion_Time_sc ~ p_rf_l2 + sex + age_s + german + (1 | ID) + (1 | page), data = Goe_long1)
m3 <- lmer(Completion_Time_sc ~ p_rf_l2 + pages + sex + age_s + german + (1 | ID) + (1 | page), data = Goe_long1)
m4 <- lmer(Completion_Time_sc ~ p_rf_l2*pages + sex + age_s + german + (1 | ID) + (1 | page), data = Goe_long1)

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
          out = "t3_resp_times_m.tex")

# Intra-individual response variability - models

m0a <- glmer(irv_p20 ~ (1 | ID), family = binomial, data = Goe_long1)
m1a <- glmer(irv_p20 ~ p_rf_l2 + (1 | ID), family = binomial, data = Goe_long1)
m2a <- glmer(irv_p20 ~ p_rf_l2 + sex + age_s + german + (1 | ID), family = binomial, data = Goe_long1)

m0b <- glmer(irv_p80 ~ (1 | ID), family = binomial, data = Goe_long1)
m1b <- glmer(irv_p80 ~ p_rf_l2 + (1 | ID), family = binomial, data = Goe_long1)
m2b <- glmer(irv_p80 ~ p_rf_l2 + sex + age_s + german + (1 | ID), family = binomial, data = Goe_long1)

summary(m1a)
summary(m2a)
summary(m1b)
summary(m2b)

class(m2a) <- "lmerMod"
class(m2b) <- "lmerMod"

stargazer(m2a, m2b, keep = c("Constant", "p_rf_l2"), report = ('vcsp'),
          add.lines = list(c("Demographic controls", "X", "X")), title = "Generalized mixed effects regressions", 
          omit.stat = c("ll", "aic"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t3_irv_m.tex")

## 05: Compare groups (item level)
# Primacy effects - models

m0 <- glmer(primacy ~ (1 | ID) + (1 | page), family = binomial, data = Goe_long2)
m1 <- glmer(primacy ~ p_rf_l2 + (1 | ID) + (1 | page), family = binomial, data = Goe_long2)
m2 <- glmer(primacy ~ p_rf_l2 + sex + age_s + german + (1 | ID) + (1 | page), family = binomial, data = Goe_long2)
m3 <- glmer(primacy ~ p_rf_l2 + pages + sex + age_s + german + (1 | ID) + (1 | page), family = binomial, data = Goe_long2)
m4 <- glmer(primacy ~ p_rf_l2*pages + sex + age_s + german + (1 | ID) + (1 | page), family = binomial, data = Goe_long2)

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
          out = "t3_primacy_m.tex")

## 06: Compare groups (respondent level)
# Attention check - models

m1 <- glm(AC_Answ ~ p_rf_l2, family = binomial, data = Goe_ac)
m2 <- glm(AC_Answ ~ p_rf_l2 + sex + age_s + german, family = binomial, data = Goe_ac)

summary(m1)
summary(m2)

stargazer(m1, m2, keep = c("Constant", "p_rf_l2"), report = ('vcsp'),
          add.lines = list(c("Demographic controls", "", "X")), title = "Logistic regressions", 
          omit.stat = c("ll"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t3_ac_m.tex")
