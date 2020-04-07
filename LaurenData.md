Data independence - Lauren
================
Mike O’Donnell
4/3/2020

  - [Analysis of Lauren’s grouped
    data:](#analysis-of-laurens-grouped-data)
      - [Initial plots:](#initial-plots)
      - [Time as a continuous
        parameter:](#time-as-a-continuous-parameter)
      - [Time as a categorical
        parameter](#time-as-a-categorical-parameter)

<!-- output: -->

<!--   html_document: -->

<!--     keep_md: true -->

<!--     df_print: paged -->

# Analysis of Lauren’s grouped data:

<details>

<summary>Initial plots</summary>

<p>

## Initial plots:

First reading in the data and having a look at data types and columns

``` r
SSTRDrugs <- read_csv("data/merckTIME.csv") 
glimpse(SSTRDrugs)
#> Observations: 566
#> Variables: 17
#> $ Image.Name        <chr> "Red", "Red", "Red", "Red", "Red", "Red", "Red…
#> $ Hour              <time>       NA,       NA, 00:00:00, 01:00:00, 05:00…
#> $ Minute            <time>       NA, 21:00:00,       NA,       NA,      …
#> $ Sec               <dbl> 892, 227, 233, 744, 980, 328, 411, 507, 249, 6…
#> $ Summary.Statistic <chr> "Average", "Average", "Average", "Average", "A…
#> $ Area              <dbl> 0.08647458, 0.09638673, 0.09748974, 0.09193923…
#> $ Average.intensity <dbl> 14519.72, 15771.13, 19273.54, 18991.05, 16789.…
#> $ Total.intensity   <dbl> 182299.28, 219680.88, 293729.12, 252930.91, 28…
#> $ Image             <chr> "Copy of Image 29_Maximum intensity projection…
#> $ Overlay           <chr> "GRB_R", "GRB_R", "GRB_R", "GRB_R", "GRB_R", "…
#> $ Treatment         <chr> "CTL", "CTL", "CTL", "CTL", "CTL", "CTL", "CTL…
#> $ NormAvgAVG        <dbl> 0.9570927, 1.0395811, 1.2704491, 1.2518277, 1.…
#> $ NormAvgTOT        <dbl> 0.9493243, 1.1439891, 1.5295957, 1.3171388, 1.…
#> $ Dissociation      <chr> "VC2780", "VC2780", "VC2780", "VC2780", "VC278…
#> $ SLIDE             <chr> "VC2780_S1", "VC2780_S1", "VC2780_S1", "VC2780…
#> $ Time              <chr> "24H", "24H", "24H", "24H", "24H", "24H", "24H…
#> $ Label             <chr> "CTL", "CTL", "CTL", "CTL", "CTL", "CTL", "CTL…
```

Let’s grab the last 5 rows since it looks like those have all of the
necessary information used for analysis. Let’s make `Time` a numeric
column in the process. Then plot the data without considering grouping.

``` r
SSTRDrugs %<>% select(NormAvgTOT, Dissociation, SLIDE, Time, Label) %>%
  separate(Time, into = "Time", sep = "H") %>%
  mutate(Time = as.numeric(Time))

SSTRDrugs %>%
  ggplot(aes(x = Time, y = NormAvgTOT)) +
  geom_smooth(method = "lm", aes(group = Label, colour = Label)) +
  geom_point(aes(group = Label, colour = Label), position = position_dodge(width = 2)) +
  geom_boxplot(aes(group = interaction(Time,Label), colour = Label),  width = 2) +
  scale_colour_viridis_d(end = .9)
```

<img src="man/figures/README-initial plot Time-1.png" width="100%" />

Looks like the L\_2 group goes down and MK\_1 goes up. Now lets look at
the experiments by dissociation.

``` r
SSTRDrugs %>%
  ggplot(aes(x = Time, y = NormAvgTOT)) +
  geom_smooth(method = "lm", aes(group = Label, colour = Label)) +
  geom_point(aes(group = Label, colour = Label), position = position_dodge(width = 2)) +
  geom_boxplot(aes(group = interaction(Time,Label), colour = Label),  width = 2) +
  scale_colour_viridis_d(end = .9) +
  facet_wrap(~Dissociation)
```

<img src="man/figures/README-by dissociation-1.png" width="100%" />

Looks complicated, let’s look at the difference between `CTL` and each
drug treatment at each time point by dissociation.

</p>

</details>

<details>

<summary>Time as a continuous parameter</summary>

<p>

## Time as a continuous parameter:

``` r
Summary_dissoc <- SSTRDrugs %>%
  group_by(Dissociation, Time, Label) %>%
  summarise(median = median(NormAvgTOT))

Summary_dissoc %>% 
  ggplot(aes(x = Time, y = median)) +
  geom_smooth(method = "lm", aes(group = Label, colour = Label), lty = 2, alpha = 0.25) +
  geom_smooth(data = SSTRDrugs, method = "lm", 
              aes(y = NormAvgTOT, group = Label, colour = Label), fill = "blue") +
  geom_boxplot(aes(group = interaction(Time,Label), colour = Label),  width = 2) +
  geom_point(aes(group = Label, colour = Label), position = position_dodge(width = 2)) +
  scale_colour_viridis_d(end = .9)
```

<img src="man/figures/README-continuous predict-1.png" width="100%" />
This time shoing the effect by `Dissociation`, which has the same
slopes, probably because the data are already normalized by
`Dissociation`. The Standard error for the original data are in blue
with solid lines, the new based on summaries by Dissociation are in grey
with dashed lines. Standard errors are much wider because in this highly
conservative way to look at the data, the n is much smaller. The real
variability is probably somewhere in between these. So let’s look by
`slide` on which the cells were stained, which is another level of
grouping.

``` r
Summary_slide <- SSTRDrugs %>%
  group_by(SLIDE, Time, Label) %>%
  summarise(median = median(NormAvgTOT))

Summary_slide %>% 
  ggplot(aes(x = Time, y = median)) +
  geom_smooth(method = "lm", aes(group = Label, colour = Label), lty = 2, alpha = 0.25) +
  geom_smooth(data = SSTRDrugs, method = "lm", 
              aes(y = NormAvgTOT, group = Label, colour = Label), fill = "blue") +
  geom_boxplot(aes(group = interaction(Time,Label), colour = Label),  width = 2) +
  geom_point(aes(group = Label, colour = Label), position = position_dodge(width = 2)) +
  scale_colour_viridis_d(end = .9)
```

<img src="man/figures/README-by slide-1.png" width="100%" /> This is
comforting because even when grouped by slide on which the cells were
stained, we have the same overall trend. This likely is a bit closer to
the effective n for the experiment, since cells on same `SLIDE` and
`Dissocation` are not truly independent. The linear approximation does
an okay job at capturing the time component. Let’s compare the effects
of modeling these grouping factors.

### Anova vs. Mixed-models:

``` r
#simple ANOVA
lm.anova <- SSTRDrugs %>%
  lm(data = ., formula = NormAvgTOT ~ Time * Label)

lm.grouped <- SSTRDrugs %>%
  lmerTest::lmer(data = ., formula = NormAvgTOT ~ Time *  Label + (1 | Dissociation) + (1 | SLIDE))

lm.anova %>%
  summary()
#> 
#> Call:
#> lm(formula = NormAvgTOT ~ Time * Label, data = .)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.79948 -0.22363 -0.03434  0.20754  1.20796 
#> 
#> Coefficients:
#>                  Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     0.9784568  0.0618946  15.808  < 2e-16 ***
#> Time            0.0009813  0.0031558   0.311  0.75596    
#> LabelL_2        0.0977180  0.1133331   0.862  0.38894    
#> LabelMK_1      -0.0542652  0.0966145  -0.562  0.57457    
#> Time:LabelL_2  -0.0138019  0.0059490  -2.320  0.02070 *  
#> Time:LabelMK_1  0.0161723  0.0050069   3.230  0.00131 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3315 on 560 degrees of freedom
#> Multiple R-squared:  0.195,  Adjusted R-squared:  0.1878 
#> F-statistic: 27.13 on 5 and 560 DF,  p-value: < 2.2e-16

lm.grouped %>%
  summary()
#> Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#> lmerModLmerTest]
#> Formula: NormAvgTOT ~ Time * Label + (1 | Dissociation) + (1 | SLIDE)
#>    Data: .
#> 
#> REML criterion at convergence: 311.8
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -2.6040 -0.5771 -0.0769  0.5195  3.8950 
#> 
#> Random effects:
#>  Groups       Name        Variance Std.Dev.
#>  SLIDE        (Intercept) 0.029225 0.17095 
#>  Dissociation (Intercept) 0.008286 0.09103 
#>  Residual                 0.077145 0.27775 
#> Number of obs: 566, groups:  SLIDE, 103; Dissociation, 15
#> 
#> Fixed effects:
#>                  Estimate Std. Error         df t value Pr(>|t|)    
#> (Intercept)      0.952729   0.108946  64.552368   8.745 1.47e-12 ***
#> Time             0.001750   0.005481  69.417714   0.319    0.750    
#> LabelL_2         0.049658   0.164339 104.994427   0.302    0.763    
#> LabelMK_1       -0.030304   0.144423  96.021929  -0.210    0.834    
#> Time:LabelL_2   -0.010227   0.008747 102.327339  -1.169    0.245    
#> Time:LabelMK_1   0.015027   0.007534  94.229879   1.995    0.049 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation of Fixed Effects:
#>             (Intr) Time   LblL_2 LbMK_1 T:LL_2
#> Time        -0.932                            
#> LabelL_2    -0.492  0.464                     
#> LabelMK_1   -0.602  0.578  0.452              
#> Time:LblL_2  0.439 -0.467 -0.947 -0.409       
#> Tim:LblMK_1  0.557 -0.595 -0.409 -0.934  0.404
```

There appears to be a significant interaction effect between MK\_1 and
time, indicating that MK\_1 is changing the slope of the time effect. In
reality the best approach to estimate uncertainty in mixed-effects
models is using bootstapping.

``` r
mySumm <- function(.) {
  predict(., newdata=expand_grid(Label = c("CTL", "L_2", "MK_1"), Time = 6:24), re.form=NA)
}

PI.boot1.time <- system.time(
  boot1 <- lme4::bootMer(lm.grouped, mySumm, nsim=250, use.u=FALSE, type="parametric")
)

####Collapse bootstrap into median, 95% PI (from https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html)
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}


PI.boot1 <- sumBoot(boot1)
Boot.intervals <- cbind(PI.boot1, expand_grid(Label = c("CTL", "L_2", "MK_1"), Time = 6:24))
```

``` r
#compare bootstrap CIs to lm (anova) CIs
SSTRDrugs %>%
  ggplot(aes(x = Time, y = NormAvgTOT)) +
  geom_ribbon(data = Boot.intervals, 
              aes(y = fit, ymin = lwr, ymax = upr, group = Label), 
              alpha = 0.2, label = "bootstrap intervals") +
  geom_smooth(method = "lm", aes(group = Label, colour = Label),fill = "blue", lty = 2) +
  geom_line(data = Boot.intervals, aes(y = fit, group = Label, colour = Label)) +
  # geom_point(aes(group = Label, colour = Label), position = position_dodge(width = 2)) +
  geom_boxplot(aes(group = interaction(Time,Label), colour = Label),  width = 2) +

  scale_colour_viridis_d(end = .9)
```

<img src="man/figures/README-bootstrap plot-1.png" width="100%" />

The mixed-model captures more of the uncertainty, but still shows we can
be confident of an upward effect of the MK\_1 treatment. Less convincing
is the downward effect of L\_2, with the caveat that this is modeled as
a linear effect, which might not capture the time-dependent effects in
culture.

</p>

</details>

<details>

<summary>Time as a categorical parameter</summary>

<p>

## Time as a categorical parameter

What if we consider all of the time points to be categorical? This is a
situation more similar to Nathan’s categorical grouped data. If the plan
is to use a frequentist comparison, then in this case, it makes sense to
consider what the H0 would be before the analysis. It seems that a major
reason to use categorical `Time` would be if the H0 was none of the drug
treatments at any time points differ from the control. In that case, we
should have 2 comparisons to control for each time point, and also have
an alpha adjustment for the fact that we are conducting tests at 3 time
points.

### Anova vs Mixed-effects model:

``` r
############### for categorical 'Time' #######

lm.anova.cat <- SSTRDrugs %>%
  lm(data = ., formula = NormAvgTOT ~ factor(Time) * Label)

anova.cat.emm <- lm.anova.cat %>% 
  emmeans::emmeans("trt.vs.ctrl" ~ Label | factor(Time))

### now just need to correct for the 3 time points, emmeans does it for you when you use rbind()
anova.cat.emm$contrasts %>% 
  rbind(adjust = "dunnett") %>%
  kableExtra::kable(title = "Anova model") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Time

</th>

<th style="text-align:left;">

contrast

</th>

<th style="text-align:right;">

estimate

</th>

<th style="text-align:right;">

SE

</th>

<th style="text-align:right;">

df

</th>

<th style="text-align:right;">

t.ratio

</th>

<th style="text-align:right;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.0414178

</td>

<td style="text-align:right;">

0.0881842

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

\-0.4696733

</td>

<td style="text-align:right;">

0.9710337

</td>

</tr>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.0381577

</td>

<td style="text-align:right;">

0.0742709

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

0.5137630

</td>

<td style="text-align:right;">

0.9625578

</td>

</tr>

<tr>

<td style="text-align:left;">

18

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.1116093

</td>

<td style="text-align:right;">

0.0505834

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

\-2.2064406

</td>

<td style="text-align:right;">

0.1281621

</td>

</tr>

<tr>

<td style="text-align:left;">

18

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.2432095

</td>

<td style="text-align:right;">

0.0528034

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

4.6059459

</td>

<td style="text-align:right;">

0.0000302

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.2902703

</td>

<td style="text-align:right;">

0.0607541

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

\-4.7777923

</td>

<td style="text-align:right;">

0.0000135

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.3286284

</td>

<td style="text-align:right;">

0.0510866

</td>

<td style="text-align:right;">

557

</td>

<td style="text-align:right;">

6.4327746

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

</tbody>

</table>

``` r


lm.grouped.cat <- SSTRDrugs %>%
  lmerTest::lmer(data = ., formula = NormAvgTOT ~ factor(Time) * Label + 
                   ( 1 | Dissociation) + ( 1 | SLIDE))

grouped.cat.emm <- lm.grouped.cat %>%
  emmeans::emmeans("trt.vs.ctrl" ~ Label | factor(Time))

# these are approx p-values based on the mixed effects model
grouped.cat.emm$contrasts %>% 
  rbind(adjust = "dunnett") %>% 
  kableExtra::kable(title = "Mixed effects model") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Time

</th>

<th style="text-align:left;">

contrast

</th>

<th style="text-align:right;">

estimate

</th>

<th style="text-align:right;">

SE

</th>

<th style="text-align:right;">

df

</th>

<th style="text-align:right;">

t.ratio

</th>

<th style="text-align:right;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.0523626

</td>

<td style="text-align:right;">

0.1276106

</td>

<td style="text-align:right;">

106.94288

</td>

<td style="text-align:right;">

\-0.4103309

</td>

<td style="text-align:right;">

0.9803044

</td>

</tr>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.0608716

</td>

<td style="text-align:right;">

0.1117668

</td>

<td style="text-align:right;">

94.56536

</td>

<td style="text-align:right;">

0.5446298

</td>

<td style="text-align:right;">

0.9556562

</td>

</tr>

<tr>

<td style="text-align:left;">

18

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.0980911

</td>

<td style="text-align:right;">

0.0754872

</td>

<td style="text-align:right;">

89.04039

</td>

<td style="text-align:right;">

\-1.2994400

</td>

<td style="text-align:right;">

0.5934680

</td>

</tr>

<tr>

<td style="text-align:left;">

18

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.2299994

</td>

<td style="text-align:right;">

0.0861084

</td>

<td style="text-align:right;">

80.12418

</td>

<td style="text-align:right;">

2.6710434

</td>

<td style="text-align:right;">

0.0459628

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

L\_2 - CTL

</td>

<td style="text-align:right;">

\-0.2506334

</td>

<td style="text-align:right;">

0.0978356

</td>

<td style="text-align:right;">

86.48226

</td>

<td style="text-align:right;">

\-2.5617819

</td>

<td style="text-align:right;">

0.0598584

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

MK\_1 - CTL

</td>

<td style="text-align:right;">

0.3399407

</td>

<td style="text-align:right;">

0.0809441

</td>

<td style="text-align:right;">

87.10730

</td>

<td style="text-align:right;">

4.1996988

</td>

<td style="text-align:right;">

0.0003721

</td>

</tr>

</tbody>

</table>

We can see that the point estimates for the data and the difference of
means are fairly close, but the SEs are higher and the resulting p
values are more conservative. If, just as we did for Nathan’s data, we
plotted bootstrap confidence intervals, we’d see the intervals are wider
using the mixed-effects model. Using a full bayesian model to estimate
the parameters is best, but maybe unnecessary in this case unless a
better estimation of the effect size is really important. Similar to the
analysis using time as a continuous predictor, there is evidence for an
effect of MK\_1, but weaker evidence for L\_2.

</p>

</details>
