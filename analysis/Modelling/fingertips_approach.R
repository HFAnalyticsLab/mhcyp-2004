# how good is the Fingertips model - taking the breakdown of prevalences by age, sex, NS-SEC8 from the survey
# and using this to create a weighted average prevalence based on the census breakdown of local areas by age, sex and NS-SEC8 (socio-economic classification)
yr2004$over11 <- ifelse(yr2004$chldage >= 11, 1, 0)
yr2004$nssec8 <- ifelse(is.na(yr2004$nssec8), "Never worked/ long-term unemployed", yr2004$nssec8)

interacted <- glm(anycd_ic ~ chldsex * over11 * nssec8,
                  data = train,
                  family = "binomial")
summary(interacted)

model_performance(interacted, train)
plot_roc(interacted, train)
plot_roc(interacted, test)

confusion_matrix(interacted, test)
confusion_matrix(interacted, test, 0.1)

