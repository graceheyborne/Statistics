#Problem Set 5
#Grace Heyborne

#set working directory
setwd("/Users/u1100249/Desktop/PS5/")

#libraries
library(ggplot2)
library(factoextra)
library(caret)
library(Rtsne)
library(gridExtra)
library(fpc)
library(clusterSim)
library(dplyr)



#1 LOAD DATA
data <- read.csv("Breast_Cancer.csv")
head(data)
#separate the diagnosis variable to standardize the others variables
data_variables <- data %>% select(-diagnosis)
data_standard <- scale(data_variables)
#set diagnosis as a factor for later
data$diagnosis <- factor(data$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
data_diagnosis <- data$diagnosis


#2 PCA
data.pca <- prcomp(data_standard, center = TRUE, scale. = TRUE) #run PCA

#2A: scree plot
eig = fviz_eig(data.pca)
eig
eig$data #the first 4 components explain most of the variance

#2B: biplot
fviz_pca_biplot(data.pca,
                label = "var",
                habillage = data_diagnosis,
                repel = TRUE)

#2C: which variables contribute most on PC 1,2?
fviz_pca_var(data.pca, axes = c(1,2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#ANSWER: the variables that contribute most are radius_mean, radius_worst, area_mean, area_worst, perimeter_mean, perimeter_worst, and concave.points_mean.

#2D: confusion matrix
pca_scores <- data.pca$x[, 1:4] #use principle components 1-4
set.seed(224)
pca_clusters <- kmeans(pca_scores, centers = 2, nstart = 25)$cluster #use K-means clustering
table(pca_clusters, data_diagnosis) #check table to assign clusters to diagnoses
predicted_labels <- ifelse(pca_clusters == 1, "Benign", "Malignant") #cluster 1 = B, cluster 2 = M
predicted_labels <- factor(predicted_labels, levels = c("Benign", "Malignant")) #set factor
confusionMatrix(predicted_labels, data_diagnosis)
#ACURRANCY: 0.9104



#3 t-SNE
#3A: try different perplexities
set.seed(45)
tsne_result1 <- Rtsne(data_standard, dims = 2, perplexity = 5, verbose = TRUE) #5
tsne_result2 <- Rtsne(data_standard, dims = 2, perplexity = 30, verbose = TRUE) #30
tsne_result3 <- Rtsne(data_standard, dims = 2, perplexity = 50, verbose = TRUE) #50

#3B: visualize colored by true labels
#first convert to df
tsne_data1 <- as.data.frame(tsne_result1$Y)
tsne_data2 <- as.data.frame(tsne_result2$Y)
tsne_data3 <- as.data.frame(tsne_result3$Y)
tsne_data1$diagnosis <- data_diagnosis
tsne_data2$diagnosis <- data_diagnosis
tsne_data3$diagnosis <- data_diagnosis
#plot
ggplot(tsne_data1, aes(x = V1, y = V2, color = diagnosis)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "t-SNE (Perplexity = 5)", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal()
ggplot(tsne_data2, aes(x = V1, y = V2, color = diagnosis)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "t-SNE (Perplexity = 30)", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal()
ggplot(tsne_data3, aes(x = V1, y = V2, color = diagnosis)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "t-SNE (Perplexity = 50)", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal()
#ANSWER: perplexity 50 looks the best (separate and tight clusters)

#3C: confusion matrix
set.seed(345)
tsne_clusters <- kmeans(tsne_data3[, 1:2], centers = 2, nstart = 25)$cluster #use K-means again for clustering
table(tsne_clusters, tsne_data3$diagnosis) #check table to assign clusters to diagnoses
predicted_labels_tsne <- ifelse(tsne_clusters == 1, "Benign", "Malignant") #cluster 1 = B, cluster 2 = M
predicted_labels_tsne <- factor(predicted_labels_tsne, levels = c("Benign", "Malignant")) #set factor
confusionMatrix(predicted_labels_tsne, tsne_data3$diagnosis)
#ACCURACY: 0.9402



#4 K-means Clustering
#4A: elbow method
fviz_nbclust(data_standard, FUN = kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")
#ANSWER: k=2 or k=3 looks optimal

#4B: silhouette score
fviz_nbclust(data_standard, FUN = kmeans, method = "silhouette") +
  labs(title = "Silhouette Score for Optimal k")
#ANSWER: k=2 is optimal

#4C: confusion matrix
set.seed(533)
kmeans_final <- kmeans(data_standard, centers = 2, nstart = 25) #use K-means clustering
table(kmeans_final$cluster, data_diagnosis) #check table to assign clusters to diagnoses
predicted_kmeans <- ifelse(kmeans_final$cluster == 1, "Benign", "Malignant") #cluster 1 = B, cluster 2 = M
predicted_kmeans <- factor(predicted_kmeans, levels = c("Benign", "Malignant")) #set factor
confusionMatrix(predicted_kmeans, data_diagnosis)
#ACCURACY: 0.9104 (exactly the same as the PCA confusion matrix)



#5 Hierarchical Clustering
#5A and 5B: try different linkage methods/plot dendrograms
dist_matrix <- dist(data_standard, method = "euclidean") #find distance
methods <- c("average", "single", "complete", "ward.D2") #set methods
plots <- list() #create empty list for plots
for (m in methods) { #loop through the methods and preform hierarchical clustering
  hc <- hclust(dist_matrix, method = m)
  plots[[m]] <- fviz_dend(hc, k = 2, rect = TRUE, main = paste("Dendrogram -", m), cex = 0.6) #plot
}
grid.arrange(grobs = plots, ncol = 2)
#ANSWER: ward.D2 is superior for clustering

#5C: confusion matrix
hc_ward <- hclust(dist_matrix, method = "ward.D2") #use superior ward.D2
cluster_labels <- cutree(hc_ward, k = 2)
table(cluster_labels, data_diagnosis) #check table to assign clusters to diagnoses
predicted_hc <- ifelse(cluster_labels == 1, "Malignant", "Benign") # cluster 1 = M, cluster 2 = B
predicted_hc <- factor(predicted_hc, levels = c("Benign", "Malignant"))
confusionMatrix(predicted_hc, data_diagnosis)
#ACCURACY: 0.8805 (the worst so far)



#6 Combination of Methods
#6A:does combining the methods work better? (set up the combinations)
#using t-SNE with ward.D2 hierarchical clustering
dist_tsne <- dist(tsne_data3[, 1:2])
hc_tsne <- hclust(dist_tsne, method = "ward.D2")
clusters_tsne_hc <- cutree(hc_tsne, k = 2)
#using PCA and ward.D2 hierarchical clustering
pca_dist_matrix <- dist(pca_scores, method = "euclidean") #find distance
hc_pca_ward <- hclust(pca_dist_matrix, method = "ward.D2") 
pca_cluster_labels <- cutree(hc_pca_ward, k = 2)

#6B: confusion matrix 
#for t-SNE and ward.D2 hierarchical clustering
table(clusters_tsne_hc, tsne_data3$diagnosis) #check table to assign clusters to diagnoses
predicted_combined <- ifelse(clusters_tsne_hc == 1, "Malignant", "Benign") #cluster 1 = M, cluster 2 = B
predicted_combined <- factor(predicted_combined, levels = c("Benign", "Malignant"))
confusionMatrix(predicted_combined, tsne_data3$diagnosis)
#ACCURACY: 0.9508
#for PCA and ward.D2 hierarchical clustering
table(pca_cluster_labels, data_diagnosis) #check table to assign clusters to diagnoses
predicted_pca_hc <- ifelse(pca_cluster_labels == 1, "Malignant", "Benign") #cluster 1 = M, cluster 2 = B
predicted_pca_hc <- factor(predicted_pca_hc, levels = c("Benign", "Malignant"))
confusionMatrix(predicted_pca_hc, data_diagnosis)
#ACCURACY 0.9069

#ANSWER FOR BOTH: combining t-SNE and hierarchical clustering improved the score (the highest score yet), but combining PCA and hierarchical did not improve scores.



#7 Evaluation
#7A: compare confusion matrices
accuracy_df <- data.frame(
  Method = c("PCA", "t-SNE", "K-means", "Hierarchical", 
             "t-SNE + Hierarchical", "PCA + Hierarchical"),
  Accuracy = c(0.9104, 0.9402, 0.9104, 0.8805, 0.9508, 0.9069)
)
print(accuracy_df)

#7B: Dunn index
#PCA (+ K-means)
kmeans_pca <- kmeans(pca_scores, centers = 2, nstart = 25)
dunn.pca.kmeans <- cluster.stats(dist(pca_scores), kmeans_pca$cluster)$dunn
print(dunn.pca.kmeans) #0.026
#t-SNE (+ K-means)
dunn.tsne.kmeans <- cluster.stats(dist(tsne_data3[, 1:2]), tsne_clusters)$dunn
print(dunn.tsne.kmeans) #0.018
#K-means
dunn.kmeans <- cluster.stats(dist(data_standard), kmeans_final$cluster)$dunn
print(dunn.kmeans) #0.061
#Hierarchical
dunn.ward <- cluster.stats(dist(data_standard), cluster_labels)$dunn
print(dunn.ward) #0.072
#t-SNE + Hierarchical 
dist_tsne <- dist(tsne_data3[,1:2])
hc_tsne <- hclust(dist_tsne, method = "ward.D2")
clusters_tsne_hc <- cutree(hc_tsne, k = 2)
dunn.tsne.ward <- cluster.stats(dist_tsne, clusters_tsne_hc)$dunn
print(dunn.tsne.ward) #0.057
#PCA + Hierarchical
dunn.pca.ward <- cluster.stats(pca_dist_matrix, pca_cluster_labels)$dunn
print(dunn.pca.ward) #0.037
#ANSWER: Dunn index metrics differ substantially from accuracy metrics. 
#Here, the hierarchical clustering preformed the best (despite having the lowest accuracy)
#and the t-SNE (+ K-means) preformed the worst (despite having the second highest accuracy).

#7C: Davies-Bouldin index
#PCA (+ K-means)
db_pca.kmeans <- index.DB(pca_scores, kmeans_pca$cluster, centrotypes = "centroids")$DB
print(db_pca.kmeans) #1.207
#t-SNE (+ K-means)
db_tsne.kmeans <- index.DB(tsne_data3[, 1:2], k_tsne$cluster, centrotypes = "centroids")$DB
print(db_tsne.kmeans) #0.812
#K-means
db_kmeans <- index.DB(data_standard, kmeans_final$cluster, centrotypes = "centroids")$DB
print(db_kmeans) #1.444
#Hierarchical
db_ward <- index.DB(data_standard, cluster_labels, centrotypes = "centroids")$DB
print(db_ward) #1.500
#t-SNE + Hierarchical
db_tsne.ward <- index.DB(tsne_data2[, 1:2], clusters_tsne_hc, centrotypes = "centroids")$DB
print(db_tsne.ward) #0.655
#PCA + Hierarchical
db_pca.ward <- index.DB(pca_scores, pca_cluster_labels, centrotypes = "centroids")$DB
print(db_pca.ward) #1.242
#ANSWER t-SNE + Hierarchical and t-SNE alone preformed the best by this metric while Hierarchical preformed the worst. 
#Overall, this pattern closely follows the accuracy metric trends.



#8 Conclusion
'''
Based on this analysis, we can compare the accuracy of different clustering techniques. 
Overall, all techniques were very accurate. Ranking accuracy scores from best to worst: 
t-SNE (+ k-means) + Hierarchical (95%)
t-SNE (+ k-means) (94%)
k-means (91%)
PCA (+ k-means) (91%)
PCA (+ k-means) + Hierarchical (90%)
Hierarchical (88%)

Combining Hierarchical clustering with t-SNE improved accuracy and had the best accuracy score. Combining Hierarchical with PCA actually reduced accuracy by 1%. 
It is also of note that PCA (+ k-means) had the same accuracy as k-means alone, indicating that PCA didn’t enhance clustering. 
Hierarchical clustering fared the worst, and only Ward’s method managed to clearly seperate diagnosis cases. 
Specificity metrics were lower than sensitivity metrics across all techniques, indicating that malignant cases were easier to identify than benign cases.
It is also of note that the Dunn Index metric and the Davies-Boulin Index conflict. 
The Davies Bouldin index closely mirrored the accuracy metric rankings (best to worst: t-SNE + Hierarchical, t-SNE, PCA, PCA + Hierarchical,  K-means, Hierarchical).
The Dunn index didn’t (best to worst: Hierarchical, K-means, t-SNE + Hierarchical, PCA + Hierarchical, PCA, t-SNE). 
This is an interesting finding, and illustrates the importance of comparing multiple evaluation metrics when assessing clusters.
'''



#9 Additional Questions
#how many PCA's are required to explain 80% of the variance?
#ANSWER: 5 for this dataset (based on scree plot from part 1)
#T/F: t-SNE preserves global distances between samples.
#ANSWER: FALSE, it preserves local structure but not global distance.
#why do we scale the variables in this dataset?
#ANSWER: because clustering uses distance, the variables diverse ranges would distort relative distances.
#which metric favors high separation and low intra-cluster spread?
#ANSWER: Dunn Index, because its formula is min inter-cluster dist / max intra-cluster dist (higher values indicate more separate / tight clusters)

