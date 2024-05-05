# Load the data

install.packages(c("factoextra","cluster","NbClust"))
library(factoextra)
library(cluster)
library(NbClust)
data=Sales_Product_Details
str(data)
data$Product_Description =as.factor(data$Product_Description)
data$Product_Category = as.factor(data$Product_Category)
data$Product_Line = as.factor(data$Product_Line)
data$Raw_Material = as.factor(data$Raw_Material)
data$Region = as.factor(data$Region)

data$Product_Description = as.numeric(data$Product_Description)
data$Product_Category = as.numeric(data$Product_Category)
data$Product_Line = as.numeric(data$Product_Line)
data$Raw_Material = as.numeric(data$Raw_Material)
data$Region = as.numeric(data$Region)
View(data)
colSums(is.na(data))
# Standardize the data
df <- scale(data)
# Show the first 6 rows
head(df, nrow = 6)
# Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]

fviz_nbclust(df, kmeans, method = "wss")+ geom_vline(xintercept = 3, linetype = 2)
#k-means model
set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
# Print the results 
print(km.res)

#To find mean of the each variable
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster) 
head(dd)
table(km.res$cluster)

km.res$size


#To represent in diagram
fviz_cluster(km.res, data = df, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), ellipse.type = "euclid",  star.plot = TRUE,  repel = TRUE,  ggtheme = theme_minimal() )

fviz_cluster(km.res, data = df, palette = c("set2"), ellipse.type = "euclid",  star.plot = TRUE,  repel = TRUE,  ggtheme = theme_minimal() )        




#Hierarchical clustering
res.hc <- hclust(d = res.dist, method = "ward.D2")
print(res.hc)
#cex: label size
fviz_dend(res.hc, cex = 0.5)
# Compute cophentic distance
res.coph <- cophenetic(res.hc)
# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))
grp <- cutree(res.hc, k = 3)
head(grp, n = 3)
table(grp)
table(grp)/nrow(df)
aggregate(data,list(grp),mean)





final_data <- cbind(data, cluster = km.res$cluster)
# Function to create bar graphs for Product_ID against Sales Revenue within each

plot_product_id_vs_salesrev = function(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Product_ID), y = Sales_Revenue)) +
    geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
    labs(title = "Product ID vs. Sales_Revenue by Cluster",
         x = "Product ID", y = "Sales_Revenue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
# Create bar graphs for each cluster
for (i in unique(final_data$cluster)) {
  cluster_subset <- final_data[final_data$cluster == i, ]
  print(plot_product_id_vs_quantity(cluster_subset) + ggtitle(paste("Cluster", i)))
}
