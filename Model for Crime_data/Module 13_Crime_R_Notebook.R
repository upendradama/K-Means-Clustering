---
output:
  html_notebook: default
  html_document: default
---
# K-Means clustering

### Problem Statement:- 

  - Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
  
### Data Understanding and Preparation

```{r}
#Reading the dataset
crime <- read.csv("~/desktop/Digi 360/Module 13/crime_data.csv")
```

```{r}
head(crime)
```

```{r}
any(is.na(crime))
```


```{r}
#Removing First column since it is categorical
crime_num <- crime[, -c(1)]
head(crime_num)
```


```{r}
summary(crime_num)
```

### Scaling

Before applying Hierarchical Clustering, we have to normalize the data so that the scale of each variable is the same. Why is this important? Well, if the scale of the variables is not the same, the model might become biased towards the variables with a higher magnitude.

```{r}
crime_scaled <- as.data.frame(scale(crime_num))
summary(crime_scaled)
```

Since all the values here are continuous numerical values, you will use the euclidean distance method.

```{r}
nrow(crime_scaled)
```

### Cluster Tendency with Hopkins Statistic
```{r}
library(factoextra)
library(ggplot2)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(crime_scaled, n = 49, graph = FALSE)
res$hopkins_stat
```

Since hopkins is above the threshold, our data is high tendency to clustering.

### Elbow Curve
```{r}
wss = (nrow(crime_scaled)-1)*sum(apply(crime_scaled, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(crime_scaled, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
```

### Building the Model

```{r}
model <- kmeans(crime_scaled, 4) # 4 cluster solution
str(model)
```

```{r}
#Appending the clusters to original dataframe.
suppressPackageStartupMessages(library(dplyr))
crime_cl <- mutate(crime, model$cluster)
```

```{r}
head(crime_cl)
```

```{r}
#Rename column X as "State"
colnames(crime_cl) <- c("State", "Murder","Assault", "Urbanpop", "Rape", "Cluster")
head(crime_cl)
```
### Visualizing Entire Data points

```{r}
fviz_cluster(model, data = crime_scaled)
```


### Visualizing few attributes

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(crime_cl, aes(x=Assault, y = Urbanpop, color = factor(model$cluster))) + geom_point()
```

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(crime_cl, aes(x=Rape, y = Urbanpop, color = factor(model$cluster))) + geom_point()
```

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(crime_cl, aes(x=Murder, y = Urbanpop, color = factor(model$cluster))) + geom_point()
```

```{r}
crime_num <- crime_num %>%
  mutate(Cluster = model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
```

```{r}
head(crime_num)
```


```{r}
#Writing the final file with assigned cluster IDs.
write.csv(crime_cl, file="~/desktop/Digi 360/Module 12/crime_final_R.csv")
```

### Conclusion

- As per Hopkins Statistics the data is tendency to clustering.
- As per Elbow curve, the optimal clusters are 4
