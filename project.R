setwd('C:/Users/khanh/OneDrive/Desktop/IB BOOKS/Multivariate Statistical Analysis/Project')

library(ggplot2)
library(stringr)
library(tidyverse)
library(dendextend)
library(RColorBrewer)

# Preprocessing
data <- read.csv("Employment.csv", skip = 2)

data <- data[, -1]

colnames(data) <- c("field_of_education", "area", "employment_rate_of_students")
data$area <- str_replace(data$area, "^MK\\d+ ", "")

transformed_data <- data %>%
  pivot_wider(names_from = field_of_education, values_from = employment_rate_of_students) %>%
  arrange(area)
rwnames <- transformed_data$area
transformed_data <- transformed_data[, -1]
row.names(transformed_data) <- rwnames
summary(transformed_data)
head(transformed_data)


#Univariate

# Define column names
column_names <- colnames(transformed_data)

par(mfrow = c(6, 2))
# Plot histograms for columns 1 to 12

for (i in 1:12) {
  hist(transformed_data[[i]], xlab = column_names[i], main = paste("Histogram of", column_names[i]), cex.main = 1)
}
par()

#Bivariate
pairs(transformed_data, gap = 0, upper.panel = NULL,
      bg = RColorBrewer::brewer.pal(nrow(transformed_data), "Set3"), pch = 21, cex = 1.25)


ggplot(data, aes(x = area, y = employment_rate_of_students, color = area)) +
  geom_point() +
  facet_wrap(~ field_of_education, scales = "free", ncol = 2) +  # Set ncol to 2
  labs(x = NULL, y = "Employment Rate of Students",
       title = "Scatter Plots of Employment Rate by Area for Each Field of Education") +
  theme(axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank(),  # Hide x-axis ticks
        strip.text = element_text(size = 7)) +  # Adjust size of plot titles
  scale_color_discrete(name = "Area")


#Multivariate: Clustering

data_dist <- dist(transformed_data, method = "euclidean", diag = FALSE, upper = FALSE)
data_dist
d <- as.matrix(data_dist)
round(d, 1)

data_clust <- hclust(data_dist, method = "average")

plot(data_clust, xlab = "Area / Clusters",
     ylab = "Tree height [Euclidean distance]", main = "Average linkage",
     sub = NA, hang = -1)
abline(h = 30, lty = 2, col = 2, lwd = 2)

plot(hclust(data_dist, method = "single"), main = "Single linkage",
     xlab = "Area / Clusters", sub = NA, hang = -1)


plot(hclust(data_dist, method = "complete"), main = "Complete linkage",
     xlab = "Area / Clusters", sub = NA, hang = -1)

cutree(data_clust, h = 30)
par(mar = c(9, 4, 2, 2))
dend <- as.dendrogram(data_clust)
dend <- color_labels(dend, h = 30,
                     col = RColorBrewer::brewer.pal(5, "Dark2"))
plot(dend)
abline(h = 30, lty = 2, col = 2, lwd = 2)

wcss <- c()
for (i in 2:19) {
  clusters <- cutree(data_clust, k = i)  # Cut the dendrogram into 'i' clusters
  wcss[i] <- sum((data_dist)^2 * (clusters - 1)^2)  # Calculate WCSS
}

# Plot the Elbow Method graph
plot(2:19, wcss[2:19], type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)")
