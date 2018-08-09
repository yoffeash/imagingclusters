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
           legend.labs = c("1","2","3"))

### lung function, exercise capacity and quality of life ###
cluster_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

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
