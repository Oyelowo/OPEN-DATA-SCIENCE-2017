# access the MASS package
library(MASS)
library(tidyverse)
#install.packages("corrplot")
library(corrplot)

# load the data
data("Boston")

# explore the dataset
str(Boston)
summary(Boston)

# plot matrix of the variables
#pairs(Boston)


# MASS, corrplot, tidyverse and Boston dataset are available

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) 

# print the correlation matrix


# visualize the correlation matrix
corrplot(cor_matrix, method="circle")


# MASS and Boston dataset are available

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled<-as.data.frame(boston_scaled)


# MASS, Boston and boston_scaled are available

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label=c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)


# boston_scaled is available

# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test[,"crime"]

# remove the crime variable from test data
test <- dplyr::select(test, -crime)

# MASS and train are available

# linear discriminant analysis
lda.fit <- lda(crime~., data = train)
?corrplot
# print the lda.fit object
lda.fit

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
train$crime <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col = train$crime, pch= train$crime)
lda.arrows(lda.fit, myscale = 2)

plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', col=train$crime)