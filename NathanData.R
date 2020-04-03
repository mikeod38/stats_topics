library(tidyverse)
library(magrittr)
ggplot2::theme_set(theme_classic())
#read csv file in:
Tstar <- readr::read_csv(file.choose()) %>% mutate(group = interaction(genotype, video)) 
Tstar %>%
  ggplot(aes(x = genotype, y = `T*`)) + geom_point(aes(colour = group))


Tstar %>% lm(data = ., `T*` ~ group) %>% 
  emmeans::emmeans("group") %>% 
  emmeans::contrast(method = "pairwise")

lm.anova <- Tstar %>% lm(data = ., 0 +`T*` ~ genotype) 
lm.anova %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")

lm.group <- Tstar %>% lme4::lmer(data = ., `T*` ~ 0 + genotype + (1|group)) 
lm.group %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
  
predict.anova <- predict(lm.anova, 
                         newdata = tibble(genotype = c("nhr-52", "WT")), 
                         interval = "confidence")

confint.group <- confint(lm.group, method = "boot")
predict.group <- tibble(fit = predict(lm.group, 
                         newdata = tibble(genotype = c("nhr-52", "WT")),
                         re.form = NA))

intervals <- tibble(
  genotype = rep(c("nhr-52", "WT"), 2),
  data_type = rep(c("anova", "mixed model"), each = 2),
  `T*` = c(predict.anova[,'fit'], predict.group[[1]]),
  lwr  = c(predict.anova[,'lwr'], confint.group[3:4,1]),
  upr  = c(predict.anova[,'upr'], confint.group[3:4,2])
)

Tstar %>% mutate(data_type = "raw") %>%
  ggplot(aes(x = data_type, y = `T*`)) +
  geom_point(aes(colour = group)) +
  geom_point(data = intervals) +
  geom_errorbar(data = intervals, aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_grid(~genotype)
