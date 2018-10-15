### longitudinal progression of emphysema using mixed effects modeling ###

copd_long_full_same_scan$cluster_decamp_f <- factor(copd_long_full_same_scan$cluster_decamp) # setup comparisons
contrasts(copd_long_full_same_scan$cluster_decamp_f) <- contr.treatment(3, base=1) # setup comparisons

# percent emphysema longitudinal
emphysema_prog_uni_fit <- lmer(percent_emphysema ~ years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(emphysema_prog_uni_fit)
anova(emphysema_prog_uni_fit)
# note using univariate for simplicity in analysis
emphysema_prog_multi_fit <- lmer(percent_emphysema ~ age_visit + gender + race + smok_cig_now + ats_pack_years + bmi +
                                   fev1pp_utah + years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(emphysema_prog_multi_fit)
anova(emphysema_prog_multi_fit)

# percent ild longitudinal
ild_prog_uni_fit <- lmer(percent_ild ~ years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(ild_prog_uni_fit)
anova(ild_prog_uni_fit)
# note using univariate for simplicity in analysis
ild_prog_multi_fit <- lmer(percent_ild ~ age_visit + gender + race + smok_cig_now + ats_pack_years + bmi +
                                   fev1pp_utah + years + cluster_decamp_f*years + (years||sid),data=copd_long_full_same_scan)
summary(ild_prog_multi_fit)
anova(ild_prog_multi_fit)