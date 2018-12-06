### image clustering based on emphysema, ILD, PMA and AWT ###
# note use of ggfortify as described: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

# initial clustering based on imaging features - see https://www.datacamp.com/community/tutorials/k-means-clustering-r
set.seed(9)
copd_cluster_pre = copd_full_imaging %>% select(sid,percent_ild_z,percent_emphysema_z,
                                                percent_normal_z,PMA_z,awt_seg_thirona_z) # dataset with just SID and normalized imaging variables
km = kmeans(copd_cluster_pre[,2:6],3)

copd_full_imaging$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
copd_full_imaging = copd_full_imaging %>% mutate(cluster_decamp = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))

copd_imaging_cluster <- copd_cluster_pre
copd_imaging_cluster$cluster <- as.factor(km$cluster) # add initial cluster label to imaging data only
copd_imaging_cluster <- copd_imaging_cluster %>% mutate(cluster_decamp = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A"))) %>% 
  mutate(Cohort="COPDGene") %>% select(-cluster)

# principal components plotting of clusters (use either of the 2 below)
autoplot(prcomp(copd_cluster_pre[,2:6]), data=copd_full_imaging, colour='cluster_decamp')
autoplot(kmeans(copd_cluster_pre[,2:6],3), data=copd_cluster_pre)

# optimal number of clusters using COPDGene data
set.seed(9)
# Elbow method
df <- copd_cluster_pre[,2:6]
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
# set.seed(9)
# fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
#  labs(subtitle = "Gap statistic method")

#########################################################DECAMP##################################################################################################################
set.seed(9)
km = kmeans(decamp_pre2[,2:6],3)
decamp_clusters <- decamp_pre2
decamp_clusters$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
decamp_clusters = decamp_clusters %>% mutate(cluster_decamp = ifelse(cluster=="1","A", ifelse(cluster=="2","C","B")))
decamp_imaging_clusters <- decamp_clusters %>% mutate(Cohort="DECAMP") %>% mutate(sid=Case) %>% select(-Case, -cluster)
decamp_full_imaging_cluster <- left_join(decamp_pre1,decamp_clusters) %>% mutate(PMA_10 = Standard_PMA/100)

# principal components plotting of clusters (use either of the 2 below)
autoplot(prcomp(decamp_pre2[,2:6]), data=decamp_clusters, colour='cluster_decamp')
autoplot(kmeans(decamp_pre2[,2:6],3), data=decamp_clusters)

#### plot of both COPDGene and DECAMP using PCA on both - combined plot
#merge copdgene and decamp
decamp_pre3 <- decamp_pre2 %>% mutate(sid=Case) %>% select(-Case) %>% mutate(Cohort="DECAMP")
copd_cluster_pre2 <- copd_cluster_pre %>% mutate(Cohort="COPDGene")
decamp_copd <- full_join(decamp_pre3,copd_cluster_pre2)
decamp_copd <- decamp_copd[c(6,7,1,2,3,4,5)]

## test and train on same plot using a combined dataset
# set.seed(9)
# km = kmeans(decamp_copd[,3:7],3)
# decamp_copd_clusters <- decamp_copd
# decamp_copd_clusters$cluster <- as.factor(km$cluster) # add initial cluster label to dataset of people with complete imaging data
# decamp_copd_clusters = decamp_copd_clusters %>% mutate(Cluster = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))

# autoplot(kmeans(decamp_copd[,3:7],3), data=decamp_copd, shape='cohort')
# autoplot(prcomp(decamp_copd[,3:7]), data=decamp_copd_clusters, colour='Cluster', shape='Cohort')

## test and train on same plot using split dataset
## see: https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction

# set.seed(9)
# copd.train <- decamp_copd_clusters %>% filter(Cohort=="COPDGene")
# copd.valid <- decamp_copd_clusters %>% filter(Cohort=="DECAMP")

# conduct PCA on training dataset (COPDGene)
pc <- c(1,2)
pca <- prcomp(copd_imaging_cluster[,2:6], retx=TRUE)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) # percent explained variance
copd_imaging_cluster_pca <- cbind(copd_imaging_cluster,pca$x[,pc]) %>% select(sid,PC1,PC2,Cohort,cluster_decamp)

# prediction of PCs for validation dataset (DECAMP)
pred <- predict(pca, newdata=decamp_imaging_clusters[,1:5])
decamp_imaging_cluster_pca <- cbind(decamp_imaging_clusters,pred[,pc]) %>% select(sid,PC1,PC2,Cohort,cluster_decamp)

###Plot result
copd_decamp_imaging_cluster_pca <- full_join(copd_imaging_cluster_pca,decamp_imaging_cluster_pca)
copd_decamp_imaging_cluster_pca <- copd_decamp_imaging_cluster_pca %>% mutate(Cluster=cluster_decamp)

copd_decamp_imaging_cluster_pca_plot <- ggplot(copd_decamp_imaging_cluster_pca, aes(x=PC1,y=PC2,colour=Cluster,shape=Cohort)) + 
  geom_point()
copd_decamp_imaging_cluster_pca_plot
