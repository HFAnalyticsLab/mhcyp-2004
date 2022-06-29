# this script is to look at variation in BD rates across regions

library(tidyverse)
library(survey)
library(foreign)

# define a function to plot rates & confidence intervals across different groups
plot_cd_rates <- function(myData, myVar, myWeight = NULL) {
  myData %>%
    count({{myVar}}, anycd_ic, wt = {{myWeight}}) %>%
    group_by({{myVar}}) %>%
    mutate(prop = prop.table(n),
           total = sum(n)) %>%
    filter(anycd_ic == "Disorder present") %>%
    mutate(lower = prop.test(n, total)$conf.int[1],
           upper = prop.test(n, total)$conf.int[2]) %>%
    dplyr::select({{myVar}}, lower, prop, upper) %>%
    ggplot(aes(x = {{myVar}}, y = prop)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA),
                       name = "Proportion") +
    labs(title = "Rates of behavioural disorder")
}

# weights based on age, sex & region
prop.table(table(yr2004$anycd_ic))

yr2004 %>%
  group_by(anycd_ic) %>%
  summarise(n = sum(weightgr)) %>%
  mutate(prop = prop.table(n))

# what's the distribution of conduct disorders?
yr2004 %>%
  count(gormain, anycd_ic) %>%
  group_by(gormain) %>%
  mutate(prop = prop.table(n)) %>%
  filter(anycd_ic == "Disorder present") %>%
  ggplot(aes(x = gormain, y = prop)) +
  geom_col()

# significant differences across regions?
yr2004 %>%
  plot_cd_rates(gormain)

yr2004 %>%
  plot_cd_rates(gormain, weightsc)
# probably shouldn't use the weights for this - CI should be based on the sample size you actually got, not what you wish it was...

# using adjustment factor
yr2004 %>%
  filter(gormain == "London") %>%
  count(gormain, anycd_ic, wt = myweight) %>%
  group_by(gormain) %>%
  mutate(prop = prop.table(n))

yr2004 %>%
  #filter(gormain == "London") %>%
  count(gorgrp, anycd_ic, wt = myweight) %>%
  group_by(gorgrp) %>%
  mutate(prop = prop.table(n))


# just London and UK-wide
cd_uk_lon <- yr2004 %>%
  count(anycd_ic) %>%
  mutate(myRegion = "UK") %>%
  bind_rows(main %>%
              filter(gormain == "London") %>%
              count(anycd_ic) %>%
              mutate(myRegion = "London")) %>%
  dplyr::select(myRegion, anycd_ic, n) %>%
  group_by(myRegion) %>%
  mutate(prop = prop.table(n))

ggplot(cd_uk_lon %>%
         filter(anycd_ic == "Disorder present"), aes(x = myRegion, y = prop)) +
  geom_col()
