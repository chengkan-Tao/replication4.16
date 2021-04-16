library(tidyverse)
library(haven)
library(dplyr)


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}
nsw_dw_cpscontrol = read_data("cps_mixtape.dta")

nsw_mixtape = read_data("nsw_mixtape.dta")

nsw_dw = bind_rows(nsw_dw_cpscontrol,nsw_mixtape)


nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         educcube = educ*educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re74cube = re74^3,
         re75sq = re75^2,
         re75cube = re75^3,
         interaction2 = u74*hisp)

# logit estimating
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

linear_nsw <- lm(treat ~ age + agesq + educ + educsq +
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction2, 
                 data = nsw_dw_cpscontrol)

summary(linear_nsw)
summary(logit_nsw)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(logit_pscore = logit_nsw$fitted.values) %>%
  mutate(linear_pscore = linear_nsw$fitted.value)


# mean pscore 
#pscore_control <- nsw_dw_cpscontrol %>% 
#  filter(treat == 0) %>% 
#  pull(pscore) %>% 
#  mean()

#pscore_treated <- nsw_dw_cpscontrol %>% 
#  filter(treat == 1) %>% 
#  pull(pscore) %>% 
#  mean()

# histogram(logit)
nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = logit_pscore))

nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = logit_pscore))

nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = linear_pscore))

nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = linear_pscore))
## min and max
max_pscore_logit = nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(logit_pscore) %>% 
  max()
max_pscore_logit

min_pscore_logit = nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(logit_pscore) %>% 
  min()
min_pscore_logit

max_pscore_treat_logit = nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(logit_pscore) %>% 
  max()
max_pscore_treat_logit

min_pscore_treat_logit = nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(logit_pscore) %>% 
  min()
min_pscore_treat_logit

max_pscore_linear = nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(linear_pscore) %>% 
  max()
max_pscore_linear

min_pscore_linear = nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(linear_pscore) %>% 
  min()
min_pscore_linear

max_pscore_treat_linear = nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(linear_pscore) %>% 
  max()
max_pscore_treat_linear

min_pscore_treat_linear = nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(linear_pscore) %>% 
  min()
min_pscore_treat_linear


#drop
nsw_cpscontrol_logit_drop = nsw_dw_cpscontrol%>%
  filter(logit_pscore>0.1)

nsw_cpscontrol_logit_drop = nsw_cpscontrol_logit_drop%>%
  filter(logit_pscore<0.9)

nsw_cpscontrol_linear_drop = nsw_dw_cpscontrol%>%
  filter(logit_pscore>0.1)

nsw_cpscontrol_linear_drop = nsw_cpscontrol_linear_drop%>%
  filter(logit_pscore<0.9)

#histogram
nsw_cpscontrol_logit_drop %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = logit_pscore))

nsw_cpscontrol_logit_drop %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = logit_pscore))

nsw_cpscontrol_linear_drop %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = linear_pscore))

nsw_cpscontrol_linear_drop %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = linear_pscore))
## min and max
max_pscore_logit = nsw_cpscontrol_logit_drop %>% 
  filter(treat == 0) %>% 
  pull(logit_pscore) %>% 
  max()
max_pscore_logit

min_pscore_logit = nsw_cpscontrol_logit_drop %>% 
  filter(treat == 0) %>% 
  pull(logit_pscore) %>% 
  min()
min_pscore_logit

max_pscore_treat_logit = nsw_cpscontrol_logit_drop %>% 
  filter(treat == 1) %>% 
  pull(logit_pscore) %>% 
  max()
max_pscore_treat_logit

min_pscore_treat_logit = nsw_cpscontrol_logit_drop %>% 
  filter(treat == 1) %>% 
  pull(logit_pscore) %>% 
  min()
min_pscore_treat_logit

max_pscore_linear = nsw_cpscontrol_linear_drop %>% 
  filter(treat == 0) %>% 
  pull(linear_pscore) %>% 
  max()
max_pscore_linear

min_pscore_linear = nsw_cpscontrol_linear_drop %>% 
  filter(treat == 0) %>% 
  pull(linear_pscore) %>% 
  min()
min_pscore_linear

max_pscore_treat_linear = nsw_cpscontrol_linear_drop %>% 
  filter(treat == 1) %>% 
  pull(linear_pscore) %>% 
  max()
max_pscore_treat_linear

min_pscore_treat_linear = nsw_cpscontrol_linear_drop %>% 
  filter(treat == 1) %>% 
  pull(linear_pscore) %>% 
  min()
min_pscore_treat_linear

#first difference
x1_78 = nsw_mixtape %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

x0_78 = nsw_mixtape %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

average_treatment_effect_78 = unique(x1_78 - x0_78)

x1_75 = nsw_mixtape %>% 
  filter(treat == 1) %>% 
  pull(re75) %>% 
  mean()

x0_75 = nsw_mixtape %>% 
  filter(treat == 0) %>% 
  pull(re75) %>% 
  mean()

average_treatment_effect_75 = unique(x1_75 - x0_75)

x1_74 = nsw_mixtape %>% 
  filter(treat == 1) %>% 
  pull(re74) %>% 
  mean()

x0_74 = nsw_mixtape %>% 
  filter(treat == 0) %>% 
  pull(re74) %>% 
  mean()

average_treatment_effect_74 = unique(x1_74 - x0_74)

average_treatment_effect_74
average_treatment_effect_75
average_treatment_effect_78

##
N <- nrow(nsw_dw_cpscontrol)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/logit_pscore,
         d0 = (1-treat)/(1-logit_pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)


nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re75/logit_pscore,
         y0 = (1-treat) * re75/(1-logit_pscore),
         ht = y1 - y0)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1_weighted = (treat*re75/logit_pscore)/(s1/N),
         y0_weughted = ((1-treat)*re75/(1-logit_pscore))/(s0/N),
         norm = y1_weighted - y0_weughted)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/logit_pscore,
         d0 = (1-treat)/(1-logit_pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)


nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1_74 = treat * re74/logit_pscore,
         y0_74 = (1-treat) * re74/(1-logit_pscore),
         ht_74 = y1_74 - y0_74)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1_weighted74 = (treat*re74/logit_pscore)/(s1/N),
         y0_weighted74 = ((1-treat)*re74/(1-logit_pscore))/(s0/N),
         norm_74 = y1_weighted74 - y0_weighted74)

nsw_dw_cpscontrol %>% 
  pull(ht_74) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm_74) %>% 
  mean()
