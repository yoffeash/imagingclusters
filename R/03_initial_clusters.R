### image clustering based on emphysema, ILD, PMA and AWT ###
# note use of ggfortify as described: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

# initial clustering based on imaging features - see https://www.datacamp.com/community/tutorials/k-means-clustering-r
set.seed(9)
copd_cluster_pre = copd_full_imaging %>% select(sid,percent_ild_z,percent_emphysema_z,
                                                percent_normal_z,PMA_z,awt_seg_thirona_z) # dataset with just SID and normalized imaging variables
km = kmeans(copd_cluster_pre[,2:6],3)
copd_full_imaging$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
copd_full_imaging = copd_full_imaging %>% mutate(cluster_decamp = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))

# principal components plotting of clusters (use either of the 2 below)
autoplot(prcomp(copd_cluster_pre[,2:6]), data=copd_full_imaging, colour='cluster_decamp')
autoplot(kmeans(copd_cluster_pre[,2:6],3), data=copd_cluster_pre)

## differences in imaging feature types by cluster to check cluster assignment ##
cluster_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )

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


#########################################################DECAMP##################################################################################################################
set.seed(9)
km = kmeans(decamp_pre2[,2:6],3)
decamp_clusters <- decamp_pre2
decamp_clusters$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
decamp_clusters = decamp_clusters %>% mutate(cluster_decamp = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))

# principal components plotting of clusters (use either of the 2 below)
autoplot(prcomp(decamp_pre2[,2:6]), data=decamp_clusters, colour='cluster_decamp')
autoplot(kmeans(decamp_pre2[,2:6],3), data=decamp_clusters)

#### plot of both COPDGene and DECAMP using PCA on both
#merge copdgene and decamp
decamp_pre3 <- decamp_pre2 %>% mutate(sid=Case) %>% select(-Case) %>% mutate(Cohort="DECAMP")
copd_cluster_pre2 <- copd_cluster_pre %>% mutate(Cohort="COPDGene")
decamp_copd <- full_join(decamp_pre3,copd_cluster_pre2)
decamp_copd <- decamp_copd[c(6,7,1,2,3,4,5)]

km = kmeans(decamp_copd[,3:7],3)
decamp_copd_clusters <- decamp_copd
decamp_copd_clusters$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
decamp_copd_clusters = decamp_copd_clusters %>% mutate(Cluster = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))

autoplot(kmeans(decamp_copd[,3:7],3), data=decamp_copd, shape='cohort')
autoplot(prcomp(decamp_copd[,3:7]), data=decamp_copd_clusters, colour='Cluster', shape='Cohort')


#plot of PCA from copdgene 
set.seed(9)
pca_copd <- prcomp(copd_cluster_pre[,2:6])
expl.var <- round(pca_copd$sdev^2/sum(pca_copd$sdev^2)*100)

pred_decamp <- predict(pca_copd,newdata=decamp_pre2[,2:6])

###Plot result
COLOR <- c(2:4)
PCH <- c(1,16)

pc <- c(1,2) # principal components to plot

png("pca_pred.png", units="in", width=5, height=4, res=200)
op <- par(mar=c(4,4,1,1), ps=10)
plot(pca_copd$x[,pc], col=COLOR[copd_full_imaging$cluster_decamp], cex=PCH[1], 
     xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
     ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)
points(pred_decamp[,pc], col=COLOR[decamp_clusters$cluster_decamp], pch=PCH[2])
legend("topright", legend=levels(decamp_copd_clusters$Cluster), fill = COLOR, border=COLOR)
legend("topleft", legend=c("COPDGene", "DECAMP"), col=1, pch=PCH)
par(op)
dev.off()
