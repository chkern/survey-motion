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
load("./src/output5.Rdata")

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

gt4 <- ggplot(ESDQP_SM) +
  geom_density(aes(x = SM_mean)) +
  xlim(0, 4) +
  labs(title = "Test data") +
  theme_light(base_size = 9)

gt5 <- ggplot(ESDQP_SM) +
  geom_density(aes(x = SM_var)) +
  xlim(0, 2) +
  labs(title = "") +
  theme_light(base_size = 9)

gt6 <- ggplot(ESDQP_SM) +
  geom_density(aes(x = SM_max)) +
  xlim(0, 20) +
  labs(title = "") +
  theme_light(base_size = 9)

plots <- arrangeGrob(gt1, gt2, gt3, gt4, gt5, gt6, nrow = 2)
ggsave("p6_TA_distributions.pdf", plots, width = 8, height = 6)

## 03: Class prediction

ggplot(ESDQP_SM) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l1), na.rm = TRUE) +
  labs(y = "", x = "") +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p6_class_preds1.pdf", width = 4, height = 5.5)

ESDQP_SM %>%
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

ggsave("p6_class_preds1_2.pdf", width = 5, height = 5.5)

ggplot(ESDQP_SM) +
  geom_mosaic(aes(x = product(page), fill = p_rf_l2), na.rm = TRUE) +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_light(base_size = 12)  +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

ggsave("p6_class_preds2.pdf", width = 4, height = 5.5)

ESDQP_SM %>%
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

ggsave("p6_class_preds2_2.pdf", width = 5.1, height = 5.5)

## 03b: Sequence plots

fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

ESDQP_SM$page_o <- fct_reorg(ESDQP_SM$page,
                            "Welcome" = "v_250", "Demo" = "v_430",
                            "Heaping_I1" = "v_500", "Heaping_II2" = "v_538",
                            "Reliability_Intro" = "v_574", "Reliability_1" = "v_610",
                            "Reliability_2" = "v_646", "Knowledge_I1" = "v_682",
                            "Knowledge_II1" = "v_718", "Knowledge_III1" = "v_754",
                            "Knowledge_I2" = "v_790", "Knowledge_II2" = "v_826",
                            "Knowledge_III3" = "v_862", "Knowledge_VI" = "v_898",
                            "Knowledge_V" = "v_934", "Scale_Intro" = "v_970",
                            "Scale_I1" = "v_1006", "Scale_II1" = "v_1042",
                            "Scale_III1" = "v_1078", "Scale_I2" = "v_1114",
                            "Scale_II2" = "v_1150", "Scale_III2" = "v_1186",
                            "Big5_Intro" = "v_1222", "Big5" = "v_1258",
                            "NfC" = "v_1294", "Intruction_1" = "v_1330",
                            "Reliability_3" = "v_1366", "Multi_Intro" = "v_1402",
                            "Multi_1" = "v_1438", "Multi_2" = "v_1474",
                            "Multi_3" = "v_1510", "Multi_4" = "v_1546",
                            "Multi_5" = "v_1582", "Multi_6" = "v_1618",
                            "Multi_7" = "v_1654", "Tech_1" = "v_1690",
                            "Tech_2" = "v_1726", "Tech_3" = "v_1762",
                            "Tech_4" = "v_1798", "Tech_5" = "v_1834")

ESDQP_SM <-
  ESDQP_SM %>%
  group_by(lfdn) %>%
  mutate(mover = ifelse(mean(as.numeric(p_rf_l2)) > 1, "mover", "non_mover")) %>%
  mutate(sum_moves = sum(as.numeric(p_rf_l2[p_rf_l2 == "Moving"]))/2) %>%
  ungroup

ESDQP_SM$lfdn_o <- reorder(ESDQP_SM$lfdn, -ESDQP_SM$sum_moves)

ESDQP_SM %>%
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

ggsave("p6_sequences_1.pdf", width = 9, height = 6)

ESDQP_SM %>%
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

ggsave("p6_sequences_2.pdf", width = 9, height = 6)
