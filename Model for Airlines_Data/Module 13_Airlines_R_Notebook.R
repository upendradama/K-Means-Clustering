# K-Means clustering

### Problem Statement:- 

  - Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. Draw the inferences from the clusters    obtained.
  
### Data Understanding and Preparation

```{r}
#Reading the dataset
library(readxl)
airlines <- read_excel("~/desktop/Digi 360/Module 13/EastWestAirlines.xlsx")
```

```{r}
head(airlines)
```

```{r}
summary(airlines)
```

```{r}
any(is.na(airlines))
```

### Scaling

Before applying Hierarchical Clustering, we have to normalize the data so that the scale of each variable is the same. Why is this important? Well, if the scale of the variables is not the same, the model might become biased towards the variables with a higher magnitude.

```{r}
airlines_scaled <- as.data.frame(scale(airlines))
summary(airlines_scaled)
```

Since all the values here are continuous numerical values, you will use the euclidean distance method.

```{r}
nrow(airlines_scaled)
```

### Cluster Tendency with Hopkins Statistic
```{r}
library(factoextra)
library(ggplot2)
# Compute Hopkins statistic for iris dataset
#res <- get_clust_tendency(airlines_scaled, n = 3998, graph = FALSE)
#R takes lot of time to calculate hopkins stat value for all [nrow(airlines_scaled)-1], so calulating just with n = 10
res <- get_clust_tendency(airlines_scaled, n = 10, graph = FALSE)
res$hopkins_stat
```
Since hopkins is above the threshold, our data is high tendency to clustering.

### Elbow Curve
```{r}
wss = (nrow(airlines_scaled)-1)*sum(apply(airlines_scaled, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(airlines_scaled, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
```

### Building the Model

```{r}
model <- kmeans(airlines_scaled, 4) # 4 cluster solution
str(model)
```


```{r}
#Appending the clusters to original dataframe.
suppressPackageStartupMessages(library(dplyr))
airlines_cl <- mutate(airlines, model$cluster)
```

```{r}
head(airlines_cl)
```

### Visualizing Entire Data points

```{r}
fviz_cluster(model, data = airlines_scaled)
```

### Visualizing few attributes

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(airlines_cl, aes(x=Balance, y = Qual_miles, color = factor(model$cluster))) + geom_point()
```

```{r}
suppressPackageStartupMessages(library(ggplot2))
ggplot(airlines_cl, aes(x=Bonus_trans, y = Bonus_miles, color = factor(model$cluster))) + geom_point()
```

```{r}
airlines <- airlines %>%
  mutate(Cluster = model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
```

```{r}
head(airlines)
```


```{r}
#Writing the final file with assigned cluster IDs.
write.csv(airlines, file="~/desktop/Digi 360/Module 13/airlines_final_R.csv")
```

### Conclusion

- As per Hopkins Statistics the data is tendency to clustering.
- As per Elbow curve, the optimal clusters are 4
