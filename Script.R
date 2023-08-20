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

# discretize all variables into 5 bins each
discretized.dataset <- discretize(data = dataset[,to.discretize],
                          method = 'quantile',
                          breaks = 5)
summary(discretized.dataset)

# calculate the difference between the two vectors (with variable names)
cols.to.add <- setdiff(names(dataset), names(discretized.dataset))
# merge the discretized data frame with other columns from the original data frame
discretized.dataset<- cbind(dataset[,cols.to.add], discretized.dataset)
str(discretized.dataset)

# rearranging collumns order
discretized.dataset <- discretized.dataset[,names(dataset)]
