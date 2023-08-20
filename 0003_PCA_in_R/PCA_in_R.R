# Load necessary libraries
library(ggplot2)

# The iris dataset contains measurements for Sepal Length, Sepal Width, Petal Length, 
# and Petal Width for three different species of iris flowers (Setosa, Versicolour, and Virginica). 
# The aim of the PCA in this tutorial is to see if the flowers can be distinguished based on 
# their measurements alone.

# Remove missing data with complete cases.
# Before doing PCA, it's crucial to handle missing data. In R, we can use complete.cases()

View(iris)
iris_clean <- iris[complete.cases(iris), ]

# We're going to use the princomp function in R. Setting cor=T means we're doing PCA on the correlation matrix, 
# not the covariance matrix, which can be better when features have different scales. 
# However, the iris dataset features are on a similar scale.

pca_result <- princomp(iris_clean[, 1:4], cor=T)
summary(pca_result)

# Screeplot
# Visualize the importance of each component:

screeplot(pca_result, type="lines")

# Loadings
# This gives an idea of how each feature contributes to each principal component:

pca_result$loadings

# PCA default plot
# Visualize the data in the reduced dimension space:

plot(pca_result)


# Biplot visual and explanation
# A biplot helps visualize both the data points and feature vectors:

biplot(pca_result)

# In this biplot, the arrows represent the original features of the data 
# (like Sepal.Length and Petal.Width). The direction and length of the arrows 
# show how each feature contributes to the principal components.

# Extract scores (data points in the PC space)
df_scores <- as.data.frame(pca_result$scores)

# Extract loadings
loadings_data <- as.data.frame(pca_result$loadings[,1:2])  # Extracting only first 2 components

# Plot using ggplot2
biplot_gg <- ggplot(df_scores, aes(Comp.1, Comp.2)) + 
  geom_point(aes(color=iris_clean$Species), size=3, alpha=0.6) +
  # Add the feature vectors (loadings)
  geom_segment(data=loadings_data, 
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2,"inches")),
               alpha = 0.75, color = "black") +
  geom_text(data=loadings_data, 
            aes(x=Comp.1, y=Comp.2, label=rownames(loadings_data)), 
            vjust=1, hjust=1) +
  theme_minimal() +
  labs(title = "PCA Biplot", x = "Principal Component 1", y = "Principal Component 2", color="Species")

# Print the plot
print(biplot_gg)

