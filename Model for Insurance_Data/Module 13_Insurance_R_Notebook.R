---
output:
  html_notebook: default
  html_document: default
---
# K-Means clustering

### Problem Statement:- 

  - Analyze the information given in the ‘Insurance Policy dataset’ to create clusters of persons falling in the same type
  
### Data Understanding and Preparation

```{r}
#Reading the dataset
library(readxl)
ins <- read.csv("~/desktop/Digi 360/Module 13/Insurance Dataset.csv")
```

```{r}
head(ins)
```

```{r}
summary(ins)
```

```{r}
any(is.na(ins))
```

### Scaling

Before applying K Means Clustering, we have to normalize the data so that the scale of each variable is the same. Why is this important? Well, if the scale of the variables is not the same, the model might become biased towards the variables with a higher magnitude.

```{r}
ins_scaled <- as.data.frame(scale(ins))
summary(ins_scaled)
```

Since all the values here are continuous numerical values, you will use the euclidean distance method.

```{r}
nrow(ins_scaled)
```

### Cluster Tendency with Hopkins Statistic
```{r}
library(factoextra)
library(ggplot2)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(ins_scaled, n = 99, graph = FALSE)
res$hopkins_stat
```
Since hopkins is above the threshold, our data is high tendency to clustering.

### Elbow Curve
```{r}
wss = (nrow(ins_scaled)-1)*sum(apply(ins_scaled, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(ins_scaled, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
```

### Building the Model

```{r}
model <- kmeans(ins_scaled, 4) # 4 cluster solution
str(model)
```


```{r}
#Appending the clusters to original dataframe.
suppressPackageStartupMessages(library(dplyr))
ins_cl <- mutate(ins, model$cluster)
```

```{r}
head(ins_cl)
```

### Visualizing Entire Data points

```{r}
fviz_cluster(model, data = ins_scaled)
```

### Visualizing few attributes

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(ins_cl, aes(x=Premiums.Paid, y = Income, color = factor(model$cluster))) + geom_point()
```

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(ins_cl, aes(x=Age, y = Income, color = factor(model$cluster))) + geom_point()
```

```{r}
ins_cl <- ins_cl %>%
  mutate(Cluster = model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
```

```{r}
head(ins_cl)
```


```{r}
#Writing the final file with assigned cluster IDs.
write.csv(ins_cl, file="~/desktop/Digi 360/Module 13/Insurance_final_R.csv")
```

### Conclusion

- As per Hopkins Statistics the data is tendency to clustering.
- As per Elbow curve, the optimal clusters are 4
