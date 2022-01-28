table(yr2004$teachc, useNA = "ifany")
table(is.na(yr2004$ta1), useNA = "ifany")
table(is.na(yr2004$da2), useNA = "ifany")
table(is.na(yr2004$tebddiff), useNA = "ifany") %>% prop.table()
table(is.na(yr2004$ta1), useNA = "ifany") %>% prop.table()

table(is.na(yr2004$ta1), yr2004$chldage, useNA = "ifany") %>% prop.table(2)

# quick z tests
# unweighted data
prop.test(rev(table(yr2004$anycd_ic)))
table(yr2004$anycd_ic)
foo <- prop.test(rev(table(yr2004$anycd_ic)))
foo$estimate * 1.13
foo$conf.int * 1.13

c(420, 7557)

# make weights to match this
(1 - (1.13 * foo$estimate)) / (1 - foo$estimate)
yr2004$myweight <- ifelse(yr2004$anycd_ic == "Disorder present", 1.13, (1 - (1.13 * foo$estimate)) / (1 - foo$estimate))
yr2004 %>%
  count(anycd_ic, wt = myweight)

# now incorporating survey weights
foo <- yr2004 %>%
  count(anycd_ic, wt = weightsc)
bar <- prop.test(foo$n[2], foo$n[1] + foo$n[2])

weighted_prevalence <- bar$estimate
true_prevalence <- bar$estimate * 1.13
bar$conf.int * 1.13

extra_weight <- (1.13 * 7570) / (407 * (1 - 1.13) + 7570)
yr2004$myweight <- ifelse(yr2004$anycd_ic == "Disorder present", yr2004$weightsc * extra_weight, yr2004$weightsc)
baz <- yr2004 %>%
  count(anycd_ic, wt = myweight)
prop.test(baz$n[2], baz$n[1] + baz$n[2])

# actually slightly smaller confidence interval
# gets the target proportion (5.8%) but no longer scales to the sample size
# maybe it's because of the slightly larger sample size?

# just simulate the number of positives directly
prop.test(0.05766406 * 7977, 7977)
# slightly smaller confidence interval than scaling the lower estimate - I'm a bit confused

prop.test(1, 100)
prop.test(2, 100)
prop.test(3, 100)

# CIs do get wider, but not *twice* as wide, *three times* as wide, etc.
prop.test(1, 100)$conf.int[2] - prop.test(1, 100)$conf.int[1]
prop.test(2, 100)$conf.int[2] - prop.test(2, 100)$conf.int[1]
prop.test(3, 100)$conf.int[2] - prop.test(3, 100)$conf.int[1]


# next steps:
# calculate a proper scaled weight
foo <- yr2004 %>%
  count(anycd_ic, wt = weightsc)
bar <- prop.test(foo$n[2], foo$n[1] + foo$n[2])

weighted_prevalence <- bar$estimate
true_prevalence <- bar$estimate * 1.13

yr2004$myweight <- ifelse(yr2004$anycd_ic == "Disorder present", yr2004$weightsc * 1.13, yr2004$weightsc * (1 - true_prevalence) / (1 - weighted_prevalence))
yr2004$myweight <- ifelse(yr2004$anycd_ic == "Disorder present", yr2004$weightsc * true_prevalence / weighted_prevalence, yr2004$weightsc * (1 - true_prevalence) / (1 - weighted_prevalence))

yr2004 %>%
  count(anycd_ic, wt = myweight)

# success! but this won't preserve the age / sex / region distribution - there is no reference distribution for behavioural disorders
# so we either use these weights and slightly distort the distribution
# or we simply scale all final results by 1.13