### clinical differences by clusters ###

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
