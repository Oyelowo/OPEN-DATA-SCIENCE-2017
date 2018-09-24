---
  title: "Chapter 4: Clustering and classification"
author: "Anni Norring"
date: "23 marraskuuta 2017"
output: html_document
---
  
  # Chapter 4: Clustering and classification
  
  ## RStudio exercise 4: Data analysis and wrangling
  
  ## Data analysis
  
  
  ```{r, include=FALSE}
# Access the needed libraries:
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)
library(MASS)
library(tidyverse)
library(corrplot)
```

### 2. Data

The Boston dataset contains data from a study on housing values in suburbs of Boston. With the data you can for example study the effect of crime rates or air quality on the value of owner-ocupied homes. You can learn more about the data [here](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html). 

The dataset contains 506 observations on 14 variables. The observations are numeric with most variables taking natural number values. There are also some with integer values and one dichotomous variable. 

```{r}
# load the data
data("Boston")

# glimpse at the dataset
glimpse(Boston)
```

### 3. Overview of the data

Next we will look at the data in a bit more detail. Start by printing out the column/variable names and writing down the definitions of the different variables (from [here](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html)):
  
  ```{r}
colnames(Boston)
```

|Variable|Definition
|---|------------------------------------------------------------------------------------|
  |crim |per capita crime rate by town|
  |zn|proportion of residential land zoned for lots over 25,000 sq.ft|
  |indus|proportion of non-retail business acres per town|
  |chas|Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)| 
  |nox|nitrogen oxides concentration (parts per 10 million)| 
  |rm|average number of rooms per dwelling|
  |age|proportion of owner-occupied units built prior to 1940|
  |dis|weighted mean of distances to five Boston employment centres|
  |rad|index of accessibility to radial highways|
  |tax|full-value property-tax rate per \$10,000|
  |ptratio|pupil-teacher ratio by town|
  |black|1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town|
  |lstat|lower status of the population (percent)|
  |medv|median value of owner-occupied homes in \$1000s|
  
  From the summary of the variables we can see minimum, maximum, median and mean values as well as the 1st and 3rd quartiles of the variables.

```{r}
summary(Boston)
```

We can study and visualize the correlations between the different variables with the help of a correlations matrix and a correlations plot.

```{r}
# First calculate the correlation matrix and round it so that it includes only two digits:
cor_matrix<-cor(Boston) %>% round(digits = 2)

# Print the correlation matrix:
cor_matrix

```

```{r}
# Visualize the correlation matrix with a correlations plot:
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)

```

From the plot above we can easily see which variables correlate with which and is that correlation positive or negative. So for example, we can see that the distance from the employment centres (dis) correlates negatively with age of the houses (age), nitrogen oxides concentration (nox) and proportion of non-retail business acres (indus). We can also see that accessibility to radial highways (rad) is highly positively correlated with property taxes (tax), as are industrial land use (indus) and air pollution (nox). On the first glimpse, the correlation appear quite intuitive and nothing stands out as very suprising. One could note that the correlation of the only dummy variable chas is almost negligible with all other variables. This could be because the overall share of observations where chas = 1 is very small. 

### 4. Standardizing and scaling the dataset

We will later use linear discriminant analysis on our data, so we need to have varibles that are normally distributed and share the same covariance matrix across different classes. This requires the data to be scaled before fitting a model.

Thus we want to standardize the data set by scaling it. This is very easy to do in R by using the scale() function, which subtracts the mean of a variable from each value of the variable and divides this by the standard error. From the summary we can see that the standardized values are much more similar in magnitude across the different variables, so their relationships are easier to grasp. Note that the mean of each variable is now zero, as it should be. Also note that previously all variables were positive, but now at least the min values are negative for all variables. This is because the mean of all variables is larger than the smallest value of a variable. 

```{r}
# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)
```

Note that the scaled data set is now in matrix format. We want to change the class of the scaled data set into a dataframe, which we can do by converting it with a function as.data.frame():
  
  ```{r}
# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

```

Next we want to create a categorical (or factor) variable of the crime rate in the scaled Boston data set. We will use the quantiles as the break points in the categorical variable. We will print the summary of the scaled crime rate, the quantile vector of this variable and a table of the new factor variable to check that everything goes as intended.

```{r}
# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)
```

In order to avoid confusion, we want to drop the old crime rate variable from the dataset and replace it with the new categorical variable for crime rates.  This is easily done with the following two lines of code:
  
  ```{r}
# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)
```

Lastly for this step, we will divide the dataset to train and test sets. This will allow as further down the line to check how well our model works. We will assign 80 % of the data to the train set and the remaining 20 % to the test set. The training of the model is done with the train set and predictions on new data is conducted on the test set. 

```{r}
# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)
```

### 5. Linear discriminant analysis

Now we will fit the linear discriminant analysis on the train set. The LDA is a classification method that finds the (linear) combination of the variables that separate the target variable classes. We will use the categorical crime rate as the target variable and all the other variables in the dataset as predictor variables.

```{r}
# linear discriminant analysis
lda.fit <- lda(crime ~., data = train)

# print the lda.fit object
lda.fit

```

The LDA calculates the probabilities of a new observation belonging to each of the classes by using the trained model. After that the model classifies each observation to the most probable class. 

The most convenient and informative way to consider the results of an LDA is to visualize with a biplot. This we can do following the Datacamp exercise with the following code:
  
  ```{r}
# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 2)
```

The arrows in this plot represent the relationships of the original variables and the LDA solution.


### 6. LDA predicting performance

In this part we want to see how the LDA model performs when predicting on new data. For this end, we can use the predict() function. we start by predicting the crime classes with the test data.

```{r}
# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)
```

From the cross tabulation of the results we can see that the model appears to predict the correct results reasonably well. The model has most problems in separating med_low from low, but most of the predictions seems to fall in the correct class.


### 7. Clustering

Next we will take few steps towards clustering the data. Start by reloading the Boston dataset and standardize the dataset again. We need to do this again, because we have made also other changes to dataset. 

```{r}
# load the data
data("Boston")

# center and standardize variables
boston_scaled2 <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled2)

# class of the boston_scaled object
class(boston_scaled2)

# change the object to data frame
boston_scaled2 <- as.data.frame(boston_scaled)

```

Next we wish to calculate the distances between the observations. We do this by forming a standard Euclidian distance matrix:
  
  ```{r}
# euclidean distance matrix
dist_eu <- dist(boston_scaled2)

# look at the summary of the distances
summary(dist_eu)
```
I ran into trouble here because there are NA values in the boston_scaled2 distance matrix. The k-means algorithm wouldn't run on this dataset, retuning an error on the NA values. Despite googling I was not able to find a solution, so I'm doing the rest of this exercise with the original Boston dataset (as in the Datacamp exercises). So, a novel try:
  
  ```{r}
# euclidean distance matrix
dist_eu <- dist(Boston)

# look at the summary of the distances
summary(dist_eu)
```
From the summary we can see the relevant moments of the calculated distances between the observations. 

Then let's move on to K-means clustering, that assigns observations to groups or clusters based on similarity of the objects. The following code runs k-means algorithm on the dataset:

```{r}
# k-means clustering
km <-kmeans(Boston, centers = 3)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)
```

It's difficult to see anything from this, so let's zoom in a bit and look at only the last five columns:

```{r}
# plot the Boston dataset with clusters
pairs(Boston[6:10], col = km$cluster)
```

Next we want to know what the optimal number of clusters is. We can do this by looking at how the total of within cluster sum of squares (WCSS) behaves when the number of cluster changes. When you plot the number of clusters and the total WCSS, the optimal number of clusters is when the total WCSS drops radically.

```{r}
# MASS, ggplot2 and Boston dataset are available
set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')
```

From the plot above it's evident that optimal number of clusters is not three, but two. Let's run the k-means algorithm again with two clusters and then visualize the results:

```{r}
# k-means clustering
km <-kmeans(Boston, centers = 2)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)

# plot the Boston dataset with clusters
pairs(Boston[6:10], col = km$cluster)

```


### Super-bonus

Start by running the code below for the (scaled) train data that we used to fit the LDA. The code creates a matrix product, which is a projection of the data points.

```{r}
model_predictors <- dplyr::select(train, -crime)

# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)

# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)
```

```{r, echo = FALSE}
# Access the needed libraries:
library(plotly)
```

Then we will use the package plotly to create a 3D plot of the columns of the matrix product:

```{r}
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers')

```

This is very cool indeed! But addingdifferent colors for the different crime rate classes definetly makes it even cooler still:

```{r}
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color = train$crime)

```
Lastly, we are supposed to define the color by the clusters of the k-means. Here I must be doing something wrong, because the colors disappear completely. 

```{r}
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color = km)

```


## Data wrangling

You can find my [preprocessed data set](https://github.com/anorring/IODS-project/blob/master/human.csv) and the [data wrangling R script](https://github.com/anorring/IODS-project/blob/master/data/create_human.R) from my [GitHub repository.](https://github.com/anorring/IODS-project/tree/master/data)

