##################
# Survey Motion
# Part IIIc: Compare Motion Groups in Test Set (Respondi and Goettingen data)
# Christoph Kern
# R 3.5.1
##################

## 01: Setup

library(tidyverse)
library(broom)
library(sjmisc)
library(caret)
library(gridExtra)
library(ggmosaic)
library(car)
library(DescTools)
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
  select(ID, page, pages, sex, age, age_s, german,
         Completion_Time_s, Completion_Time_sc, irv, irv_p10, irv_p20, irv_p80, irv_p90,
         SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

resp_long1 <- select(resp_long1, ID, page, pages, sex, age, age_s, german,
                    Completion_Time_s, Completion_Time_sc, irv, irv_p10, irv_p20, irv_p80, irv_p90,
                    SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

com_long1 <- bind_rows("Survey one" = Goe_long1, "Survey two" = resp_long1, .id = "survey")
com_long1$age_s <- scale(com_long1$age)[,1] # Re-scale age

Goe_long2 <- Goe_long2 %>%
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2")) %>%
  select(ID, item, page, pages, sex, age, age_s, german,
         primacy, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

resp_long2 <- select(resp_long2, ID, item, page, pages, sex, age, age_s, german,
                     primacy, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

com_long2 <- bind_rows("Survey one" = Goe_long2, "Survey two" = resp_long2, .id = "survey")
com_long2$age_s <- scale(com_long2$age)[,1] # Re-scale age

Goe_ac <- select(Goe_ac, ID, page, sex, age, age_s, german,
                 AC_Answ, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

resp_ac <- select(resp_ac, ID, page, sex, age, age_s, german,
                  AC_Answ, SM, SM_mean:SM_q95, p_glmnet_l1:p_xgb_l2)

com_ac <- bind_rows("Survey one" = Goe_ac, "Survey two" = resp_ac, .id = "survey")
com_ac$age_s <- scale(com_ac$age)[,1] # Re-scale age

## 02a: Data check - Descriptive Statistics

CT <- com_long1 %>% filter(!is.na(p_rf_l2)) %>% group_by(survey) %>%
  summarise(min = min(Completion_Time_sc, na.rm = T), m = mean(Completion_Time_sc, na.rm = T), max = max(Completion_Time_sc, na.rm = T), n())

irv20 <- com_long1 %>% filter(!is.na(p_rf_l2)) %>% filter(pages == "Matrix") %>% group_by(survey) %>%
  summarise(min = min(irv_p20, na.rm = T), m = mean(irv_p20, na.rm = T), max = max(irv_p20, na.rm = T), n())

irv80 <- com_long1 %>% filter(!is.na(p_rf_l2)) %>% filter(pages == "Matrix") %>% group_by(survey) %>%
  summarise(min = min(irv_p80, na.rm = T), m = mean(irv_p80, na.rm = T), max = max(irv_p80, na.rm = T), n())

prim <- com_long2 %>% filter(!is.na(p_rf_l2)) %>% group_by(survey) %>%
  summarise(min = min(primacy, na.rm = T), m = mean(primacy, na.rm = T), max = max(primacy, na.rm = T), n())

AC <- com_ac %>% filter(!is.na(p_rf_l2)) %>% group_by(survey) %>%
  summarise(min = min(AC_Answ, na.rm = T), m = mean(AC_Answ, na.rm = T), max = max(AC_Answ, na.rm = T), n())

move <- com_long1 %>% to_dummy(p_rf_l2) %>% bind_cols(com_long1) %>% group_by(survey) %>%
  summarise(min = min(p_rf_l2_2, na.rm = T), m = mean(p_rf_l2_2, na.rm = T), max = max(p_rf_l2_2, na.rm = T), n())

stats_s1 <- bind_rows(CT = CT, irv20 = irv20, irv80 = irv80, prim = prim, AC = AC, move = move, .id = "var") %>% 
  filter(survey == "Survey one") %>% select(-survey) %>% mutate(m = round(m, 3))
stats_s2 <- bind_rows(CT = CT, irv20 = irv20, irv80 = irv80, prim = prim, AC = AC, move = move, .id = "var") %>%
  filter(survey == "Survey two") %>% select(-survey) %>% mutate(m = round(m, 3))

stargazer(stats_s1, summary = FALSE, out = "t5b_stats_1.html")
stargazer(stats_s2, summary = FALSE, out = "t5b_stats_2.html")

## 02b: Data check - Survey Motion

sm_stats1 <- sm %>% filter(D_group == "Moving") %>% select(SM_mean:SM_q95) %>%
  summarise_all(list(~min(.), ~mean(.), ~max(.), ~n())) %>% mutate_all(~round(., 3))
sm_stats1 <- as.data.frame(matrix(sm_stats1, ncol = 4)) 

sm_stats2 <- sm %>% filter(D_group == "Not_Moving") %>% select(SM_mean:SM_q95) %>%
  summarise_all(list(~min(.), ~mean(.), ~max(.), ~n())) %>% mutate_all(~round(., 3))
sm_stats2 <- as.data.frame(matrix(sm_stats2, ncol = 4))

Goe_stats <- Goe_SM %>% 
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2", "AC")) %>% 
  select(SM_mean:SM_q95) %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  summarise_all(list(~min(., na.rm = T), ~mean(., na.rm = T), ~max(., na.rm = T), ~n())) %>% mutate_all(~round(., 3))
Goe_stats <- as.data.frame(matrix(Goe_stats, ncol = 4))

resp_stats <- resp_SM %>% 
  filter(page %in% c("E1", "E2", "E3", "E4", "E5", "M_1", "M_2", "AC")) %>% 
  select(SM_mean:SM_q95) %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  summarise_all(list(~min(., na.rm = T), ~mean(., na.rm = T), ~max(., na.rm = T), ~n())) %>% mutate_all(~round(., 3))
resp_stats <- as.data.frame(matrix(resp_stats, ncol = 4))

stargazer(sm_stats1, summary = FALSE, out = "t5b_SM_stats_1.html")
stargazer(sm_stats2, summary = FALSE, out = "t5b_SM_stats_2.html")
stargazer(Goe_stats, summary = FALSE, out = "t5b_SM_stats_3.html")
stargazer(resp_stats, summary = FALSE, out = "t5b_SM_stats_4.html")

# Density plots

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

# Violin plots

gv1 <- ggplot(sm, aes(y = SM_mean, x = D_group, fill = D_group)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 1) +
  labs(title = "Training data", x = "") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gv2 <- ggplot(sm, aes(y = SM_var, x = D_group, fill = D_group)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 1) +
  labs(title = "", x = "") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gv3 <- ggplot(sm, aes(y = SM_max, x = D_group, fill = D_group)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 1) +
  labs(title = "", x = "") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gv4 <- ggplot(com_long1, aes(y = SM_mean, x = survey, fill = survey)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 0.5) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(title = "Survey data", x = "") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gv5 <- ggplot(com_long1, aes(y = SM_var, x = survey, fill = survey)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 0.5) +
  coord_cartesian(ylim = c(0, 3.65)) +
  labs(title = "", x = "") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        text = element_text(size = 10))

gv6 <- ggplot(com_long1, aes(y = SM_max, x = survey, fill = survey)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5, outlier.alpha = 0.1, outlier.size = 0.5) +
  coord_cartesian(ylim = c(0, 18.5)) +
  labs(title = "", x = "") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        text = element_text(size = 10))

plots <- arrangeGrob(gv1, gv2, gv3, gv4, gv5, gv6, nrow = 2)
ggsave("p5b_TA_distributions_2.pdf", plots, width = 9, height = 6)

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

plot_dat1 <- com_long1 %>%
  select(p_rf_l2, page, survey) 

plot_dat2 <- com_ac %>%
  select(p_rf_l2, page, survey)

plot_data <- rbind(plot_dat1, plot_dat2)

plot_data <- plot_data %>%
  mutate(p_rf_l2 = fct_recode(p_rf_l2, "Non-Moving" = "Not_Moving", "Moving" = "Moving")) %>%
  mutate(survey = fct_recode(survey, "Cross-sectional Survey 1" = "Survey one", "Cross-sectional Survey 2" = "Survey two")) %>%
  mutate(page = fct_recode(page, 
                           "S1" = "E1",
                           "S2" = "E2",
                           "S3" = "E3",
                           "S4" = "E4",
                           "S5" = "E5",
                           "I-b-I1" = "M_1",
                           "I-b-I2" = "M_2",
                           "IMC" = "AC")) %>%
  mutate(page = fct_relevel(page, "IMC", after = Inf))

p <- ggplot(plot_data) +
  geom_mosaic(aes(x = product(p_rf_l2, page), fill=p_rf_l2), na.rm=TRUE) +
  facet_grid(survey~.)

sum <- plot_data %>%
  group_by(survey, page) %>%
  summarise(sum = sum(!is.na(p_rf_l2))) %>%
  slice(rep(1:n(), each = 2))

temp <- ggplot_build(p)$data[[1]] %>% 
  mutate(survey = ifelse(PANEL == 1, "Cross-sectional Survey 1", "Cross-sectional Survey 2")) %>%
  add_column(sum$sum) %>%
  mutate(prop = .wt/sum$sum) %>%
  mutate(prop = sprintf("%0.2f", prop))

p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label= prop), size = 4) +
  labs(y = "", x = "Web Survey Page") +
  scale_fill_grey(start = 0.6, end = 0.8) +
  theme(text = element_text(size = 14),
        legend.position = "none")

ggsave("p5b_class_preds2_2.png", width = 7, height = 6)

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

m0 <- lmer(Completion_Time_sc ~ (1 | ID) + (1 | page), data = com_long1, 
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m1 <- lmer(Completion_Time_sc ~ p_rf_l2 + survey + (1 | ID) + (1 | page), data = com_long1,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m2 <- lmer(Completion_Time_sc ~ p_rf_l2 + sex + age_s + german + survey + (1 | ID) + (1 | page), data = com_long1,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m3 <- lmer(Completion_Time_sc ~ p_rf_l2 + pages + sex + age_s + german + survey + (1 | ID) + (1 | page), data = com_long1,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m4 <- lmer(Completion_Time_sc ~ p_rf_l2*pages + sex + age_s + german + survey + (1 | ID) + (1 | page), data = com_long1,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m5 <- lmer(Completion_Time_sc ~ p_rf_l2*survey + pages + sex + age_s + german + (1 | ID) + (1 | page), data = com_long1,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"
class(m5) <- "lmerMod"

ngrps <- rbind(summary(m1)$ngrps, summary(m2)$ngrps, summary(m3)$ngrps, summary(m4)$ngrps, summary(m5)$ngrps)

stargazer(m1, m2, m3, m4, m5, 
          keep = c("Constant", "p_rf_l2", "pages", "survey"), order = c(1, 7, 2, 8), report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes", "Yes", "Yes", "Yes"), c("Respondents", ngrps[,1]), c("Pages", ngrps[,2])), 
          title = "Mixed effects regressions", omit.stat = c("ll"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_resp_times_m.html")

# Intra-individual response variability - models

m0a <- glmer(irv_p20 ~ (1 | ID), family = binomial, data = com_long1)
m1a <- glmer(irv_p20 ~ p_rf_l2 + survey + (1 | ID), family = binomial, data = com_long1)
m2a <- glmer(irv_p20 ~ p_rf_l2 + sex + age_s + german + survey + (1 | ID), family = binomial, data = com_long1)
m3a <- glmer(irv_p20 ~ p_rf_l2*survey + sex + age_s + german + (1 | ID), family = binomial, data = com_long1)

m0b <- glmer(irv_p80 ~ (1 | ID), family = binomial, data = com_long1)
m1b <- glmer(irv_p80 ~ p_rf_l2 + survey + (1 | ID), family = binomial, data = com_long1)
m2b <- glmer(irv_p80 ~ p_rf_l2 + sex + age_s + german + survey + (1 | ID), family = binomial, data = com_long1)
m3b <- glmer(irv_p80 ~ p_rf_l2*survey + sex + age_s + german + (1 | ID), family = binomial, data = com_long1)

class(m1a) <- "lmerMod"
class(m2a) <- "lmerMod"
class(m3a) <- "lmerMod"
class(m1b) <- "lmerMod"
class(m2b) <- "lmerMod"
class(m3b) <- "lmerMod"

ngrps <- rbind(summary(m1a)$ngrps, summary(m2a)$ngrps, summary(m3a)$ngrps, 
               summary(m1b)$ngrps, summary(m2b)$ngrps, summary(m3b)$ngrps)

stargazer(m1a, m2a, m3a, m1b, m2b, m3b, 
          keep = c("Constant", "p_rf_l2", "survey"), order = c(1, 6), report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes", "Yes", "", "Yes", "Yes"), c("Respondents", ngrps)), 
          title = "Generalized mixed effects regressions", omit.stat = c("ll"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_irv_m.html")

## 05: Compare groups (item level)
# Primacy effects - models

m0 <- glmer(primacy ~ (1 | ID) + (1 | page), family = binomial, data = com_long2, 
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m1 <- glmer(primacy ~ p_rf_l2 + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m2 <- glmer(primacy ~ p_rf_l2 + sex + age_s + german + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m3 <- glmer(primacy ~ p_rf_l2 + pages + sex + age_s + german + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m4 <- glmer(primacy ~ p_rf_l2*pages + sex + age_s + german + survey + (1 | ID) + (1 | page), family = binomial, data = com_long2,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m5 <- glmer(primacy ~ p_rf_l2*survey + sex + age_s + german + pages + (1 | ID) + (1 | page), family = binomial, data = com_long2,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
class(m4) <- "lmerMod"
class(m5) <- "lmerMod"

ngrps <- rbind(summary(m1)$ngrps, summary(m2)$ngrps, summary(m3)$ngrps, summary(m4)$ngrps, summary(m5)$ngrps)

stargazer(m1, m2, m3, m4, m5,
          keep = c("Constant", "p_rf_l2", "pages", "survey"), order = c(1, 7, 2, 8), report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes", "Yes", "Yes", "Yes"), c("Respondents", ngrps[,1]), c("Pages", ngrps[,2])), 
          title = "Generalized mixed effects regressions", omit.stat = c("ll"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_primacy_m.html")

## 06: Compare groups (respondent level)
# Attention check - models

m0 <- glm(AC_Answ ~ p_rf_l2, family = binomial, data = com_ac)
m1 <- glm(AC_Answ ~ p_rf_l2 + survey, family = binomial, data = com_ac)
m2 <- glm(AC_Answ ~ p_rf_l2 + survey + sex + age_s + german, family = binomial, data = com_ac)
m3 <- glm(AC_Answ ~ p_rf_l2*survey + sex + age_s + german, family = binomial, data = com_ac)

r2s <- round(rbind(PseudoR2(m1, "McKelveyZavoina"), PseudoR2(m2, "McKelveyZavoina"), PseudoR2(m3, "McKelveyZavoina")), 3)
BICs <- round(rbind(BIC(m1), BIC(m2), BIC(m3)), 3)
  
stargazer(m1, m2, m3, 
          keep = c("Constant", "p_rf_l2", "survey"), order = c(1, 3), report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes", "Yes"), c("Pseudo r2", r2s[,1]), c("Bayesian Inf. Crit.", BICs[,1])), 
          title = "Logistic regressions", omit.stat = c("ll"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "t5b_ac_m.html")
