library(tidyverse)
library(survey)
library(foreign)

main <- read.spss("\\\\thf-rds-fsvm01\\ode_data\\Analytics\\Tom\\MHCYP 2004\\UKDA-5269-spss\\spss\\spss12\\cpm9904.sav",
                  to.data.frame = TRUE)

yr2004 <- main %>%
  filter(sampyear == 2004) %>%
  mutate(ethgpc1 = as.character(ethgpc1),
         hhinc2 = as.character(hhinc2),
         simple_eth = case_when(
           str_detect(ethgpc1, "Black") ~ "Black",
           ethgpc1 %in% c("Pakistani", "Bangladeshi", " Indian") ~ "Asian",
           is.na(ethgpc1) ~ "Other",
           TRUE ~ ethgpc1),
         completed_hhinc = if_else(is.na(hhinc2), "Refused income", hhinc2)
  )

# weights based on age, sex & region

# can we reproduce survey quoted average of 5.8% with behavioural disorder?
prop.table(table(yr2004$anycd_ic)) # unweighted gives 5.3%

# survey package
svy2004 <- svydesign(id = ~1, weights = ~weightgr, data = yr2004)
svymean(~anycd_ic, svy2004) # weighted gives 5.1% :-S

# applying the weights manually gives the same result - 5.1%
yr2004 %>%
  mutate(cd = as.integer(anycd_ic) - 1,
         cd.weighted = cd * weightgr) %>%
  summarise(cd.tot = sum(cd.weighted),
            all.tot = sum(weightgr)) %>%
  mutate(cd_rate = cd.tot / all.tot)

# scaled weights gives slightly different, still 5.1% to 2sf
yr2004 %>%
  mutate(cd = as.integer(anycd_ic) - 1,
         cd.weighted = cd * weightsc) %>%
  summarise(cd.tot = sum(cd.weighted),
            all.tot = sum(weightsc)) %>%
  mutate(cd_rate = cd.tot / all.tot)

yr2004 %>%
  group_by(anycd_ic) %>%
  summarise(mean(weightsc))


# what about any disorder?
svymean(~any_ic, svy2004)
table(yr2004$any_ic, useNA = "always") %>% prop.table()

table(yr2004$chldage)

# check the selection criteria - can only find a couple of ages out of scope of the sample, and they don't shift the average
yr2004 %>%
  filter(chldage > 4 & chldage < 17) %>%
  mutate(cd = as.integer(anycd_ic) - 1,
         cd.weighted = cd * weightsc) %>%
  summarise(cd.tot = sum(cd),
            cd.tot.wt = sum(cd.weighted),
            all.tot = n(),
            all.tot.wt = sum(weightsc)) %>%
  mutate(cd_rate_uw = 1.13 * (cd.tot / all.tot),
         cd_rate_wt = 1.13 * (cd.tot.wt / all.tot.wt))


# check for missing or zero weights
sum(is.na(yr2004$weightsc))
sum(yr2004$weightsc == 0)

# look for alternative weight
yr2004 %>%
  filter(chldage < 5 | chldage > 16) %>%
  select(weightsc)
unique(yr2004$gormain)
unique(yr2004$chldsex)

# check gross and scaled weights against p267 of the survey report: https://files.digital.nhs.uk/publicationimport/pub06xxx/pub06116/ment-heal-chil-youn-peop-gb-2004-rep1.pdf
yr2004 %>%
  filter(chldsex == "Male") %>%
  filter(chldage >= 16) %>%
  group_by(gormain) %>%
  summarise(unwt_numbers = n(),
            pop_estimate = sum(weightgr),
            grossed = mean(weightgr),
            scaled = mean(weightsc))
# this output matches (have to include 17yos)

# Have to use these adjustment factors
# Conduct disorder: 1.13
# Any disorder: 1.07