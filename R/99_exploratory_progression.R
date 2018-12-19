#### exploratory biomarkers and progression #####
# fev1
fev1_prog_multi_fit <- lm(fev1_utah_delta ~ age_visit + gender + race + smok_cig_now + ats_pack_years +
                                fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging)
cbind(effect = coef(fev1_prog_multi_fit), 
      confint(fev1_prog_multi_fit), 
      p = coef(summary(fev1_prog_multi_fit))[,4])

# fvc
fvc_prog_multi_fit <- lm(fvc_utah_delta ~ age_visit + gender + race + smok_cig_now + ats_pack_years +
                           fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging)
cbind(effect = coef(fvc_prog_multi_fit), 
      confint(fvc_prog_multi_fit), 
      p = coef(summary(fvc_prog_multi_fit))[,4])

# 6WMD
distwalked_prog_multi_fit <- lm(distwalked_delta ~ age_visit + gender + race + smok_cig_now + ats_pack_years +
                           fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging)
cbind(effect = coef(distwalked_prog_multi_fit), 
      confint(distwalked_prog_multi_fit), 
      p = coef(summary(distwalked_prog_multi_fit))[,4])

                                copd_full_imaging_2$cluster_decamp_f <- factor(copd_full_imaging_2$cluster_decamp) # setup comparisons
                                contrasts(copd_full_imaging_2$cluster_decamp_f) <- contr.treatment(3, base=1) # setup comparisons
                                cluster_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") ) # setup comparisons


# percent emphysema rate
emphysema_rate_prog_uni_fit <- lm(percent_emphysema_rate ~ cluster_decamp_f , data=copd_full_imaging_2)
cbind(effect = coef(emphysema_rate_prog_uni_fit), 
      confint(emphysema_rate_prog_uni_fit), 
      p = coef(summary(emphysema_rate_prog_uni_fit))[,4])

emphysema_rate_prog_multi_fit <- lm(percent_emphysema_rate ~ cluster_decamp_f + age_visit + gender + race + smok_cig_now + ats_pack_years +
                                  fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging_2)
cbind(effect = coef(emphysema_rate_prog_multi_fit), 
      confint(emphysema_rate_prog_multi_fit), 
      p = coef(summary(emphysema_rate_prog_multi_fit))[,4])

emph_prog_cluster_plot =  ggplot(copd_full_imaging_2, aes(cluster_decamp,percent_emphysema_rate)) + 
  geom_boxplot(fill="gray") + 
  ylab("Annual Rate of Emphysema Progression") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
emph_prog_cluster_plot

# percent ild rate
ild_rate_prog_uni_fit <- lm(percent_ild_rate ~ cluster_decamp_f , data=copd_full_imaging_2)
cbind(effect = coef(ild_rate_prog_uni_fit), 
      confint(ild_rate_prog_uni_fit), 
      p = coef(summary(ild_rate_prog_uni_fit))[,4])

ild_rate_prog_multi_fit <- lm(percent_ild_rate ~ cluster_decamp_f + age_visit + gender + race + smok_cig_now + ats_pack_years +
                                      fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging_2)
cbind(effect = coef(ild_rate_prog_multi_fit), 
      confint(ild_rate_prog_multi_fit), 
      p = coef(summary(ild_rate_prog_multi_fit))[,4])


copd_long_full$cluster_decamp_f <- factor(copd_long_full$cluster_decamp) # setup comparisons
contrasts(copd_long_full$cluster_decamp_f) <- contr.treatment(3, base=1) # setup comparisons

# percent emphysema longitudinal
emphysema_prog_uni_fit <- lmer(percent_emphysema ~ years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(emphysema_prog_uni_fit)
anova(emphysema_prog_uni_fit)

emphysema_prog_multi_fit <- lmer(percent_emphysema ~ age_visit + gender + race + smok_cig_now + ats_pack_years + bmi +
                                 fev1pp_utah + years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(emphysema_prog_multi_fit)
anova(emphysema_prog_multi_fit)

################# interstitial ############################
ild_fev1_change_fit <- lm(change_p1_p2_fev1_ml.y ~ percent_ild_delta + percent_emphysema_delta + percent_ild + percent_emphysema +
                       age_visit + gender + race + smok_cig_now + ats_pack_years + bmi +
                       fev1_utah, data=copd_full_imaging_2)
ild_fev1_change_fit_table <- 
  (cbind(effect = coef(ild_fev1_change_fit), 
      confint(ild_fev1_change_fit), 
      p = coef(summary(ild_fev1_change_fit))[,4]))[2,]

copd_full_imaging_2 <- copd_full_imaging_2 %>% mutate(fvc_utah_delta_ml=fvc_utah_delta*1000)
ild_fvc_change_fit <- lm(fvc_utah_delta_ml ~ percent_ild_delta + percent_emphysema_delta + percent_ild + percent_emphysema +
                            age_visit + gender + race + smok_cig_now + ats_pack_years + bmi + 
                            fvc_utah, data=copd_full_imaging_2)
ild_fvc_change_fit_table <- 
(cbind(effect = coef(ild_fvc_change_fit), 
      confint(ild_fvc_change_fit), 
      p = coef(summary(ild_fvc_change_fit))[,4]))[2,]

ild_lungfx_multi_table <- rbind(ild_fev1_change_fit_table,ild_fvc_change_fit_table)

row.names(ild_lungfx_multi_table) <- c("Forced Expiratory Volume in 1 Second",
                                       "Forced Vital Capacity")
kable(ild_lungfx_multi_table,row.names=TRUE,col.names=c("Change","Lower Limit","Upper Limit","p"),
      align="c",digits=c(2,2,2,12)) %>% 
  add_header_above(c(" " = 2, "Confidence Interval" = 2, " " = 1)) %>%
  kable_styling(bootstrap_options = c("striped","condensed"), full_width = FALSE) %>% 
  footnote(number=c("Change in mL expressed per 1% increase in the percentage of lung occupied by interstitial features.",
                    "Multivariable analyses adjusted for baseline age, sex, race, smoking status, pack years, body mass index, lung function (FEV1 and FVC respectively), the percentage of lung occupied by interstitial features, the percentage of lung occupied by emphysema, and the change in the percentage of lung occupied by emphysema."))

copd_full_imaging_2 <- copd_full_imaging_2 %>% 
  mutate(percent_ild_gain=(ifelse(percent_ild_delta>0,1,0))) %>% 
  mutate(fvc_ml_loss=(ifelse(fvc_utah_delta<0,1,0)))

ggplot(copd_full_imaging_2, aes(fvc_utah_delta_ml,percent_emphysema_delta)) + geom_point() + geom_smooth(method="lm")

fvc_loss_fit <- glm(fvc_ml_loss ~ percent_ild_gain + percent_ild + percent_emphysema +
                      age_visit + gender + race + smok_cig_now + ats_pack_years + bmi + 
                      fvc_utah, data=copd_full_imaging_2, family="binomial")
summary(fvc_loss_fit)
exp(coef(fvc_loss_fit))
exp(confint(fvc_loss_fit))
