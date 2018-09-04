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

ccl18_ild_fit <- lm(ccl18 ~ age_visit + gender + race + smok_cig_now + ats_pack_years +
                      fev1pp_utah + percent_ild + percent_emphysema + PMA + awt_seg_thirona, data=copd_full_imaging)
summary(ccl18_ild_fit)


