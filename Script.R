#loading "The Rolling Stones" dataset
dataset <- read.csv('stones_analysis.csv',stringsAsFactors = FALSE,check.names = FALSE)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#transforming dataset
source('Utility.R')
dataset <- transormed.dataframe(dataset = dataset)

#checking for NA values
all(complete.cases(dataset))

# creating outcome variable OnChart
# If there is at least one value in columns, from 'British charts' to 'POL', that is different
# than 'No' the value for OnChart variable will be TRUE, alternatively the value will be FALSE
# this is checked for every row

# Create a logical matrix indicating where the value is "No"
first.chart <- which(colnames(dataset) == 'British charts')
last.chart <- which(colnames(dataset) == 'POL')
no.matrix <- dataset[, first.chart : last.chart] == "No"
no.matrix <- rowSums(!no.matrix) > 0
# adding outcome variable to dataset based on the value in no.matrix 
dataset$OnChart <- as.factor(ifelse(no.matrix == 'TRUE', "Yes","No"))
#eliminating variables that were used for creating outcome variable
dataset <- dataset[,-c(first.chart:last.chart)]

# examining the proportion through distribution
prop.table(table(dataset$OnChart))
# about 76% of songs have been on some chart, while about 24% have not been

#checking for NA values
apply(dataset, 2, function(x) sum(is.na(x)))

# Based on classification tree analysis variables that have been found to be important for classification of data points
# are Album type, liveness, energy, danceability 

class(dataset$`Album type`)
# 'Album type' is factor variable and as such doesn't need to be modified for Naive Bayes analysis

apply(dataset[,c('liveness', 'energy','danceability')], 2, class)
# Other predictor variables are numeric and before including them in the model we should examine their distribution

apply(dataset[,c('liveness', 'energy','danceability')], 2, class)

# Doing the shapiro test to see if variable is normally distributed 
apply(dataset[,c('liveness', 'energy','danceability')], 2, shapiro.test)
# Only 'danceability' variable is normally distributed and we can use it for the model without any modifications
# Numeric variables that are not normally distributed should be discretized 

library(bnlearn)
# variables to be discretized
to.discretize <- c('liveness', 'energy')
# plotting to.discretize variables to see how many k intervals should be used in discretization
library(ggplot2)
# histogram plot for 'liveness' feature
ggplot(data = dataset, mapping = aes(x = liveness)) +
  geom_histogram(bins = 30) +
  theme_minimal()

# histogram plot for 'energy' feature
ggplot(data = dataset, mapping = aes(x = energy)) +
  geom_histogram(bins = 30) +
  theme_minimal()

# discretize all variables into 3 bins each
discretized.dataset <- discretize(data = dataset[,to.discretize],
                          method = 'quantile',
                          breaks = 3)
summary(discretized.dataset)

# calculate the difference between the two vectors (with variable names)
cols.to.add <- setdiff(names(dataset), names(discretized.dataset))
# merge the discretized data frame with other columns from the original data frame
discretized.dataset<- cbind(dataset[,cols.to.add], discretized.dataset)
str(discretized.dataset)

# rearranging collumns order
discretized.dataset <- discretized.dataset[,names(dataset)]

# creating training and test sets
library(caret)
set.seed(1)
train.indices <- createDataPartition(discretized.dataset$OnChart, p = 0.8, list = FALSE)
train.data <- discretized.dataset[train.indices,]
test.data <- discretized.dataset[-train.indices,]

# Firstly we will create the model with the most important predictor variables and the default threshold
library(e1071)
nb1 <- naiveBayes(OnChart ~ `Album type`+liveness+energy+danceability, data = train.data)
# printing the model
nb1

# making the predictions with nb1 model over the test dataset
nb1.pred <- predict(nb1, newdata = test.data, type = 'class')
# print several predictions
head(nb1.pred)
# creating the confusion matrix
nb1.cm <- table(true = test.data$OnChart, predicted = nb1.pred)
nb1.cm

# evaluating classification metrics, 'No' class is positive class( the song hasn't been on any charts)
nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval
# Accuracy is 74.5%, precision is 80%, recall is 88.8% and F1 is 84.2%

# finding the best probability threshold with ROC curve

# computing probabilities for each class value for the observations in the test set
nb1.pred.prob <- predict(nb1, newdata = test.data, type = "raw") 
nb1.pred.prob

library(pROC)
nb1.roc <- roc(response = as.numeric(test.data$OnChart),
               predictor = nb1.pred.prob[,1],
               levels = c(2, 1))
plot.roc(nb1.roc)
