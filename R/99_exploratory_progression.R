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


copd_long$cluster_decamp_f <- factor(copd_long$cluster_decamp) # setup comparisons
contrasts(copd_long$cluster_decamp_f) <- contr.treatment(3, base=1) # setup comparisons

# percent emphysema longitudinal
emphysema_prog_uni_fit <- lmer(percent_emphysema ~ cluster_decamp_f + years + (1|sid),data=copd_long)
summary(emphysema_prog_uni_fit)
anova(emphysema_prog_uni_fit)

