### clinical and radiographic differences by clusters ###

## differences in imaging feature types by cluster to check cluster assignment ##
cluster_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )

## copdgene radiographic differences by cluster
copd_emph_cluster_plot =  ggplot(copd_full_imaging, aes(cluster_decamp,percent_emphysema)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Emphysema") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 80, method="anova")     # Add global p-value
copd_ild_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,percent_ild)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Interstitial Changes") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 32, method="anova")     # Add global p-value
copd_awt_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,awt_seg_thirona)) + 
  geom_boxplot(fill="gray") + 
  ylab("Airway Wall Thickness") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 2.2, method="anova")     # Add global p-value
copd_pma_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,PMA)) + 
  geom_boxplot(fill="gray") + 
  ylab("PMA") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 120, method="anova")     # Add global p-value

plot_grid(copd_emph_cluster_plot,copd_ild_cluster_plot,copd_awt_cluster_plot,copd_pma_cluster_plot, ncol=2)

## decamp radiographic differences by cluster
decamp_emph_cluster_plot =  ggplot(decamp_full_imaging_cluster, aes(cluster_decamp,Percent_Emphysema)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Emphysema") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 80, method="anova")     # Add global p-value
decamp_ild_cluster_plot = ggplot(decamp_full_imaging_cluster, aes(cluster_decamp,Percent_ILA)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Interstitial Changes") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50, method="anova")     # Add global p-value
decamp_awt_cluster_plot = ggplot(decamp_full_imaging_cluster, aes(cluster_decamp,WallThickness)) + 
  geom_boxplot(fill="gray") + 
  ylab("Airway Wall Thickness") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 3, method="anova")     # Add global p-value
decamp_pma_cluster_plot = ggplot(decamp_full_imaging_cluster, aes(cluster_decamp,PMA_10)) + 
  geom_boxplot(fill="gray") + 
  ylab("PMA") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 100, method="anova")     # Add global p-value


plot_grid(decamp_emph_cluster_plot,decamp_ild_cluster_plot,decamp_awt_cluster_plot,decamp_pma_cluster_plot, ncol=2)

### mortality ###
# kaplan meier curve for mortality
km_fit <- survfit(Surv(days_followed, vital_status) ~ cluster_decamp, data=copd_full_imaging)
ggsurvplot(km_fit, data=copd_full_imaging, 
           risk.table=TRUE,
           pval = TRUE,
           break.time.by = 500,
           ggtheme = theme_minimal(),
           risk.table.y.text.col = T,
           risk.table.y.text = FALSE,
           legend.title = "Clusters",
           legend.labs = c("A","B","C"))

### lung function, exercise capacity and quality of life ###
cluster_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )

fev1_cluster_plot =  ggplot(copd_full_imaging, aes(cluster_decamp,fev1pp_utah)) + 
  geom_boxplot(fill="gray") + 
  ylab("FEV1") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 220, method="anova")     # Add global p-value
fvc_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,fvcpp_utah)) + 
  geom_boxplot(fill="gray") + 
  ylab("FVC") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 220, method="anova")     # Add global p-value
sixmin_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,distwalked)) + 
  geom_boxplot(fill="gray") + 
  ylab("6 Minute Walk Distance") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 3800, method="anova")     # Add global p-value
sgrq_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,sgrq_score_total)) + 
  geom_boxplot(fill="gray") + 
  ylab("SGRQ") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 150, method="anova")     # Add global p-value

plot_grid(fev1_cluster_plot,fvc_cluster_plot,sixmin_cluster_plot,sgrq_cluster_plot, ncol=2)

fev1prog_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,fev1_utah_delta)) + 
  geom_boxplot(fill="gray") + 
  ylab("FEV1 Change") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
distwalked_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,distwalked_delta)) + 
  geom_boxplot(fill="gray") + 
  ylab("6MWD Change") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value

plot_grid(fev1prog_cluster_plot,distwalked_cluster_plot, ncol=2)

copd_long_clinical$cluster_decamp_f <- factor(copd_long_clinical$cluster_decamp) # setup comparisons
contrasts(copd_long_clinical$cluster_decamp_f) <- contr.treatment(3, base=1) # setup comparisons

distwalked_prog_uni_fit <- lmer(distwalked ~ years + cluster_decamp_f*years + (years||sid),data=copd_long_clinical)
summary(distwalked_prog_uni_fit)
anova(distwalked_prog_uni_fit)

fev1_prog_uni_fit <- lmer(fev1pp_utah ~ years + cluster_decamp_f*years + (years||sid),data=copd_long_clinical)
summary(fev1_prog_uni_fit)
anova(fev1_prog_uni_fit)

### multivariable logistic regression analysis of exaerbation odds by cluster ###
copd_full_imaging$cluster_decamp_f <- factor(copd_full_imaging$cluster_decamp)
contrasts(copd_full_imaging$cluster_decamp_f) <- contr.treatment(3, base=1)
exacerbation_fit_Aref <- glm(reportedExacerbation ~ cluster_decamp_f + age_visit + gender + race + smok_cig_now +
                          fev1pp_utah + priorexacerbation + sgrq_score_total +
                          gastro_esoph_reflx, data=copd_full_imaging)
cbind(OR = exp(coef(exacerbation_fit_Aref)), 
      exp(confint(exacerbation_fit_Aref)), 
      p = coef(summary(exacerbation_fit_Aref))[,4])

contrasts(copd_full_imaging$cluster_decamp_f) <- contr.treatment(3, base=2)
exacerbation_fit_Bref <- glm(reportedExacerbation ~ cluster_decamp_f + age_visit + gender + race + smok_cig_now +
                               fev1pp_utah + priorexacerbation + sgrq_score_total +
                               gastro_esoph_reflx, data=copd_full_imaging)
cbind(OR = exp(coef(exacerbation_fit_Bref)), 
      exp(confint(exacerbation_fit_Bref)), 
      p = coef(summary(exacerbation_fit_Bref))[,4])

### multivariable zero inflated negative binomial regression
copd_full_imaging_follow <- copd_full_imaging %>% mutate(lyears = ifelse(Years_Followed>0, log(Years_Followed), 0))

rate_exac_multi <- zeroinfl(Total_Exacerbations ~ cluster_decamp_f + age_visit + gender + race + smok_cig_now +
                              fev1pp_utah + priorexacerbation + sgrq_score_total +
                              gastro_esoph_reflx, data=copd_full_imaging_follow, dist="negbin", offset=lyears, EM=TRUE)
summary(rate_exac_multi)
cbind(IRR = exp(coef(rate_exac_multi)), 
      exp(confint(rate_exac_multi)))

### univariate exaerbation rate by cluster ###
# exacerbation rate
copd_full_imaging <- copd_full_imaging %>% 
  mutate(exacerbation_rate = Total_Exacerbations/Years_Followed,
         sev_exacerbation_rate = Total_Severe_Exacer/Years_Followed)

cluster_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )

exacerbation_cluster_plot =  ggplot(subset(copd_full_imaging, priorexacerbation == 1), aes(cluster_decamp,exacerbation_rate)) + 
  geom_boxplot(fill="gray") + 
  ylab("Exacerbation Rate") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 20)     # Add global p-value

sev_exacerbation_cluster_plot =  ggplot(subset(copd_full_imaging, sev_exacerbation_rate > 0), aes(cluster_decamp,sev_exacerbation_rate)) + 
  geom_boxplot(fill="gray") + 
  ylab("Severe Exacerbation Rate") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 13.5)     # Add global p-value

plot_grid(exacerbation_cluster_plot, sev_exacerbation_cluster_plot)
