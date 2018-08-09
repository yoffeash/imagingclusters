### biomarkers by cluster ###
cluster_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

crp_cluster_plot =  ggplot(copd_full_imaging, aes(cluster_decamp,crp)) + 
  geom_boxplot(fill="gray") + 
  ylab("CRP") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 70, method="anova")     # Add global p-value
tgf_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,tgfb1_lap)) + 
  geom_boxplot(fill="gray") + 
  ylab("TGFb LAP1") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 25, method="anova")     # Add global p-value
mmp9_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,mmp9)) + 
  geom_boxplot(fill="gray") + 
  ylab("MMP9") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 3500, method="anova")     # Add global p-value
ager_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,ager)) + 
  geom_boxplot(fill="gray") + 
  ylab("AGER") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y=30, method="anova")     # Add global p-value
tnfrsf1b_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,tnfrsf1b)) + 
  geom_boxplot(fill="gray") + 
  ylab("TNFFRSF1B") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
icam1_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,icam1)) + 
  geom_boxplot(fill="gray") + 
  ylab("ICAM1") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
mmp9_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,mmp9)) + 
  geom_boxplot(fill="gray") + 
  ylab("MMP9") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
serpine1_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,serpine1)) + 
  geom_boxplot(fill="gray") + 
  ylab("SERPINE1") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
vegfa_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,vegfa)) + 
  geom_boxplot(fill="gray") + 
  ylab("VEGF Alpha") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
il1a_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,il1a)) + 
  geom_boxplot(fill="gray") + 
  ylab("IL1 alpha") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
ccl20_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,ccl20)) + 
  geom_boxplot(fill="gray") + 
  ylab("CCL20") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
il1b_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,il1b)) + 
  geom_boxplot(fill="gray") + 
  ylab("IL1 beta") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
il1rn_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,il1rn)) + 
  geom_boxplot(fill="gray") + 
  ylab("IL1 RN") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
cxcl5_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,cxcl5)) + 
  geom_boxplot(fill="gray") + 
  ylab("CXCL5") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
olr1_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,olr1)) + 
  geom_boxplot(fill="gray") + 
  ylab("OLR1") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
serpina3_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,serpina3)) + 
  geom_boxplot(fill="gray") + 
  ylab("SERPINA3") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
tnfrsf10c_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,tnfrsf10c)) + 
  geom_boxplot(fill="gray") + 
  ylab("tnfrsf10c") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value
il8_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,il8)) + 
  geom_boxplot(fill="gray") + 
  ylab("IL8") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(method="anova")     # Add global p-value

plot_grid(crp_cluster_plot,tgf_cluster_plot,mmp9_cluster_plot,ager_cluster_plot, ncol=2)
plot_grid(tnfrsf1b_cluster_plot,icam1_cluster_plot,serpine1_cluster_plot,vegfa_cluster_plot, ncol=2)
plot_grid(il1a_cluster_plot,ccl20_cluster_plot,il1b_cluster_plot,il1rn_cluster_plot, ncol=2)
plot_grid(cxcl5_cluster_plot,olr1_cluster_plot,serpina3_cluster_plot,tnfrsf10c_cluster_plot,il8_cluster_plot,ncol=2)
