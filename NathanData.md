Data independence - Nathan
================
Mike O’Donnell
4/3/2020

  - [Analysis of Nathan’s grouped
    data:](#analysis-of-nathans-grouped-data)
      - [First plotting and colour T\* values according to
        group.](#first-plotting-and-colour-t-values-according-to-group.)
      - [Running a simple ANOVA vs a mixed effects
        model](#running-a-simple-anova-vs-a-mixed-effects-model)

<!-- output: -->

<!--   html_document: -->

<!--     keep_md: true -->

<!--     df_print: paged -->

<details>

<summary>Nathan’s Calcium Data</summary>

<p>

# Analysis of Nathan’s grouped data:

## First plotting and colour T\* values according to group.

In this case the group is the slide/video in which the imaging data were
acquired

``` r

#read csv file in:
Tstar <- readr::read_csv("data/WTvsNHR52_Tstar.csv") %>% mutate(group = interaction(genotype, video)) 
Tstar %>%
  ggplot(aes(x = genotype, y = `T*`)) + geom_point(aes(colour = group))
```

<img src="man/figures/README-Tstar grouped-1.png" width="100%" />

When we analyze the effect of the grouping variable, there is some
evidence that the recording session might have an effect on the T\*
value, see the `WT.Video 2 - WT.Video1` comparison.

``` r
Tstar %>% lm(data = ., `T*` ~ group) %>% 
  emmeans::emmeans("group") %>% 
  emmeans::contrast(method = "pairwise")
#>  contrast                      estimate    SE df t.ratio p.value
#>  WT.Video 2 - nhr-52.Video1     -0.7613 0.102 30 -7.440  <.0001 
#>  WT.Video 2 - WT.Video1         -0.2994 0.110 30 -2.728  0.0490 
#>  WT.Video 2 - nhr-52.Video2     -0.7893 0.106 30 -7.471  <.0001 
#>  nhr-52.Video1 - WT.Video1       0.4619 0.112 30  4.116  0.0015 
#>  nhr-52.Video1 - nhr-52.Video2  -0.0279 0.108 30 -0.258  0.9939 
#>  WT.Video1 - nhr-52.Video2      -0.4898 0.115 30 -4.250  0.0010 
#> 
#> P value adjustment: tukey method for comparing a family of 4 estimates
```

Don’t mistake this for a method to determine whether you should consider
the grouping variable in your analysis - this is for a demonstration
only. Even if there were no evidence of an effect of the grouping
variable, as you’ll see below, it’s generally more conservative to
consider the non-independence of the data.

## Running a simple ANOVA vs a mixed effects model

In the mixed-effects model, we add a term for grouping variable, in this
case the video, which I’ve called `group`.

You can see the parameter estimates are pretty similar, but when we look
at the confidence intervals in the bottom plot, you can see that
accounting for grouping variable the confidence intervals are a bit
wider - which we’d expect if the data weren’t truly independent. In the
presence of a grouping factor like this, it’s probably best to collect
more groups.

``` r
lm.anova <- Tstar %>% lm(data = ., `T*` ~ genotype) 
lm.anova %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
#>  contrast    estimate     SE df t.ratio p.value
#>  nhr-52 - WT    0.651 0.0827 32 7.874   <.0001

lm.group <- Tstar %>% lme4::lmer(data = ., `T*` ~ 0 + genotype + (1|group)) 
lm.group %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
#>  contrast    estimate   SE   df t.ratio p.value
#>  nhr-52 - WT    0.632 0.15 1.98 4.224   0.0529
  
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
  facet_grid(~genotype) +
  labs(title = "linear vs mixed effects confidence intervals")
```

<img src="man/figures/README-anova v mixed Tstar-1.png" width="100%" />

</p>

</details>
