### image clustering based on emphysema, ILD, PMA and AWT ###
# note use of ggfortify as described: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

# initial clustering based on imaging features - see https://www.datacamp.com/community/tutorials/k-means-clustering-r
set.seed(9)
copd_cluster_pre = copd_full_imaging %>% select(sid,percent_ild_z,percent_emphysema_z,PMA_z,awt_seg_thirona_z) # dataset with just SID and normalized imaging variables
km = kmeans(copd_cluster_pre[,2:5],3)
copd_full_imaging$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
copd_full_imaging = copd_full_imaging %>% mutate(cluster_decamp = ifelse(cluster=="1","1", ifelse(cluster=="2","3","2")))

# principal components plotting of clusters (use either of the 2 below)
autoplot(prcomp(copd_cluster_pre[,2:5]), data=copd_full_imaging, colour='cluster_decamp')
autoplot(kmeans(copd_cluster_pre[,2:5],3), data=copd_cluster_pre)

## differences in imaging feature types by cluster to check cluster assignment ##
cluster_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

emph_cluster_plot =  ggplot(copd_full_imaging, aes(cluster_decamp,percent_emphysema)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Emphysema") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 80, method="anova")     # Add global p-value
ild_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,percent_ild)) + 
  geom_boxplot(fill="gray") + 
  ylab("Percentage Interstitial Changes") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 32, method="anova")     # Add global p-value
awt_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,awt_seg_thirona)) + 
  geom_boxplot(fill="gray") + 
  ylab("Airway Wall Thickness") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 2.2, method="anova")     # Add global p-value
pma_cluster_plot = ggplot(copd_full_imaging, aes(cluster_decamp,PMA)) + 
  geom_boxplot(fill="gray") + 
  ylab("PMA") +
  xlab("Cluster") +
  stat_compare_means(comparisons = cluster_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 120, method="anova")     # Add global p-value


plot_grid(emph_cluster_plot,ild_cluster_plot,awt_cluster_plot,pma_cluster_plot, ncol=2)
