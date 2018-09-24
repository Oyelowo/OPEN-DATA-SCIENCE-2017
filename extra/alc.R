"Name: OYEDAYO OYELOWO"
"sTUDENT'S NUMBER: 014717208"

# Data Set Information:
#data from: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt 
#   

# access the tidyverse libraries tidyr, dplyr, ggplot2
#install.packages("tidyr")
library(tidyr); library(dplyr); library(ggplot2); library(GGally)

alc<- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt",
                 sep = ",", header = T)


# glimpse at the alc data
colnames(alc)

#Describe the data briefly here.
#The data includes students' secondary education accomplishment of two portuguese schools.
# The data attributes include student grades, demographic, social and school related features') 
# a'nd it was collected by using school reports and questionnaires. Two datasets are 
# provided regarding the performance in two distinct subjects: Mathematics (mat) and 
# Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled 
# under binary/five-level classification and regression tasks. Important note: the target 
# attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 
# is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 
# 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but 
# such prediction is much more useful (see paper source for more details).
#source: https://archive.ics.uci.edu/ml/datasets/Student+Performance


attach(alc)
#####2
#hypotheses.
#age is not related to alcohol consumption
#sex is not related to alcohol consumption
#absence from school is not related to alcóhol use
#alcohol use is not related to performance
#freetime after is not related to alcohol consumption
#romantic relationhip is not related to alcohol consumption


# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

#subsetting my chosen variables
hyp<- alc[,c("age", "sex",  "absences", "G3", "freetime", "romantic","alc_use")]

#exploring my chosen variables
# draw a bar plot of each variable
g1=gather(hyp) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")

g1 + geom_bar()


p2 <- ggpairs(hyp, mapping = aes(col=sex, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))

#draw the plot
p2



cor.test(alc$age, alc$alc_use)
cor.test(alc$absences, alc$alc_use)
cor.test(alc$freetime, alc$alc_use)
cor.test(alc$G3, alc$alc_use)

alc$freetime2 <- cut(alc$freetime, breaks = (c(0,3,5)))
levels(alc$freetime2)<- c("low", "high")
colnames(alc)

#it is also possible to use simple crosstabs e.g
#xtabs(high_use~age, data = alc)

#but below is a more comprehensive crosstab.
#Crosstabs
alc %>% group_by(romantic, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))
#an alternative and preferrable way of doing the above
#summarise(group_by(alc, romantic,high_use), count=n(), mean_grade=mean(G3))

alc %>% group_by(sex, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))
alc %>% group_by(freetime2, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))
alc %>% group_by(paid, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))
alc %>% group_by(paid, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))
alc %>% group_by(paid, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))




############
#The age distribution by sex
# initialize a plot of 'age'
sex_age <- ggplot(data = alc, aes(x=age, col=sex))

# draw a bar plot of age by sex
sex_age + geom_bar() + facet_wrap("sex")


#high alcohol consumption by sex
# initialize a plot of 'high_use'
hu_sex <- ggplot(data = alc, aes(x=sex, col=sex))

# draw a bar plot of high_use by sex
hu_sex + geom_bar() + facet_wrap("high_use")



#######################################3
#High use vs freetime
# initialize a plot of 'high_use'
hu_ft <- ggplot(data = alc, aes(x=freetime, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ft + geom_bar() + facet_wrap("high_use")


#######################################
#High use vs absences
# initialize a plot of 'high_use'
hu_ab <- ggplot(data = alc, aes(x=absences, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ab + geom_bar() + facet_wrap("high_use")


#######################################
#High use vs age
# initialize a plot of 'high_use'
hu_ag <- ggplot(data = alc, aes(x=age, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ag + geom_bar() + facet_wrap("high_use")


#######################################
#High use vs romantic
# initialize a plot of 'high_use'
hu_rom <- ggplot(data = alc, aes(x=romantic, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_rom + geom_bar() + facet_wrap("high_use")




############################
############################
#relationship of alcohol consumption with grades
# initialize a plot of high_use and G3
h_s <- ggplot(alc, aes(x = high_use, y = G3, col=sex))

# define the plot as a boxplot and draw it
h_s + geom_boxplot() + ylab("grade")+ ggtitle("Student grades by alcohol consumption and sex")

############################
#relationship of alcohol consumption with absences
# initialise a plot of high_use and absences
h_ab<- ggplot(alc, aes(x=high_use, y=absences,col=sex))


# define the plot as a boxplot and draw it
h_ab + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")


#############################
#relationship of alcohol consumption with age
# initialise a plot of high_use and age
h_ag<- ggplot(alc, aes(x=high_use, y=age,col=sex))


# define the plot as a boxplot and draw it
h_ag + geom_boxplot() + ggtitle("Student's age by alcohol consumption and sex")


#############################
#relationship of alcohol consumption with freetime
# initialise a plot of high_use and freetime
h_fr<- ggplot(alc, aes(x=high_use, y=freetime,col=sex))


# define the plot as a boxplot and draw it
h_fr + geom_boxplot() + ggtitle("Student's freetime by alcohol consumption and sex")



#############################
#relationship of alcohol consumption with romantic relationship
# initialise a plot of high_use and romantic
alc_ro<- ggplot(alc, aes(y=alc_use, x=romantic,col=sex))


# define the plot as a boxplot and draw it
alc_ro + geom_boxplot() + ggtitle("Student's romantic relationship by alcohol consumption and sex")



#############################
#relationship of alcohol consumption with sex
# initialise a plot of high_use and sex
alc_sex<- ggplot(alc, aes(y=alc_use, x=sex,col=sex))


# define the plot as a boxplot and draw it
alc_sex + geom_boxplot() + ggtitle("Student's alcohol consumption by sex")








# Use logistic regression to statistically explore the relationship between your
# chosen variables and the binary high/low alcohol consumption variable as the 
# target variable. Present and interpret a summary of the fitted model. Present 
# and interpret the coefficients of the model as odds ratios and provide confidence
# intervals for them. Interpret the results and compare them to your previously 
# stated hypothesis. Hint: If your model includes factor variables see for 
# example the first answer of this stackexchange thread on how R treats and how 
# you should interpret these variables in the model output (or use some other 
# resource to study this). (0-5 points)

#fitting the glm model
high_use_mod1<- glm(high_use~ age + sex + absences + freetime, data= alc, family = "binomial")
summary(high_use_mod1)
# coef(high_use_mod1)
# coefficients(high_use_mod1)

high_use_mod2<- glm(high_use~  sex + absences, data= alc, family = "binomial")
summary(high_use_mod2)

#options("contrasts")


#check the overall effect of the variable by performing a likelihood ratio test
anova(high_use_mod1, high_use_mod2, test="LRT")
#there seems to be no significant difference between the two models.  
#Hence, 'sex' and 'absences' are significant enough without the redundant variables.




#calculating the odds ratio
# compute odds ratios (OR)
odds_ra <- exp(coef(high_use_mod2))
#odds_ra <- coef(high_use_mod2) %>% exp     #alternaive


# compute confidence intervals (conf_int)
conf_int <- exp(confint(high_use_mod2)) 
#Conf_Int <- high_use_mod2 %>%  confint() %>% exp   #alternative


# print out the odds ratios with their confidence intervals
cbind(Odds_Ra, Conf_Int)



#Using the variables which, according to your logistic regression model, 
#had a statistical relationship with high/low alcohol consumption, explore
#the predictive power of you model. Provide a 2x2 cross tabulation of 
#predictions versus the actual values and optionally display a graphic 
#visualizing both the actual values and the predictions. Compute the 
#total proportion of inaccurately classified individuals (= the training error)
#and comment on all the results. Compare the performance of the model with 
#performance achieved by some simple guessing strategy. (0-3 points)

# fit the model


# predict() the probability of high_use
probs<- predict(high_use_mod2, type = "response")

# add the predicted probabilities to 'alc'
alc$prob_high_use <- probs
#alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use, setting 0.5 as threshold
alc$predict_high_use<- (alc$prob_high_use)>0.5
#alc <- mutate(alc, prediction = prob_high_use>0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, prob_high_use, predict_high_use) %>% tail(10)
tail(hyp, 10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$predict_high_use)


####################################################################

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = prob_high_use, y = high_use, col= predict_high_use))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
conf_mat<-table(high_use = alc$high_use, prediction = alc$predict_high_use)
conf_mat<-prop.table(conf_mat)
addmargins(conf_mat)

#Alternatively, this can be done as shown below:
#addmargins(prop.table(table(high_use = alc$high_use, prediction = alc$predict_high_use)))
#table(high_use = alc$high_use, prediction = alc$predict_high_use) %>%  prop.table() %>% addmargins()


#a function could be used but not necessary, since there is no reptitive process.
mean(abs(alc$high_use-alc$prob_high_use)>0.5)

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$prob_high_use)


# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# # compute the average number of wrong predictions in the (training) data
loss_func(class=alc$high_use, pro=alc$predict_high_use)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = high_use_mod2, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]
