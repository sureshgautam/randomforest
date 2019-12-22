# Basic of Random Forest
#
#https://www.youtube.com/watch?v=J4Wdy0Wc_xQ
#######################################################
#Classifying wines
#Refer to the R Markdown file, random_forest.Rmd, for more information.

#Classifying wines from this dataset.
#There are 13 features/predictors/variables, which are the results of a chemical analysis of wines
#There are three labels, representing three different cultivars
#We can build a Random Forest classifier to classify wines based on their 13 features
#Related blog post http://davetang.org/muse/2012/12/20/random-forests-in-predicting-wines/
install.packages("randomForest")
library(randomForest)

# preparing the data
data_url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
df <- read.table(file=url(data_url), header=FALSE, sep=",")
head(df)
summary(df)
tail(df)
header <- c('class',
            'alcohol',
            'malic_acid',
            'ash',
            'ash_alcalinity',
            'magnesium',
            'total_phenols',
            'flavanoids',
            'nonflavanoid_phenols',
            'proanthocyanins',
            'colour',
            'hue',
            'od280_od315',
            'proline')
names(df) <- header
df$class <- as.factor(df$class)
table(df$class)

# analysis
# install if necessary
# install.packages("randomForest")
library(randomForest)

set.seed(31)
my_sample <- sort(sample(x = 1:nrow(df), replace = FALSE, size = nrow(df)/2))
my_sample_comp <- setdiff(1:nrow(df), my_sample)

test <- df[my_sample, ]
train <- df[my_sample_comp, ]

#one of regression, classification, or unsupervised
r <- randomForest(class ~ ., data=train, importance=TRUE, do.trace=10)
r <- randomForest(class ~ ., data=train, importance=TRUE, do.trace=100)

##
#
#The original call to randomForest
r$call
#one of regression, classification, or unsupervised
r$type
#MeanDecrease Accuracy Column is the mean decrease in accuracy over all classes
#MeanDecreaseGini is the mean decrease in Gini Index
r$importance
round(importance(r), 2)
#Standard Error permutation based importance measure
r$importanceSD
#Number of Tree grown
r$ntree
#Number of predictors sampled for spliting at each node
r$mtry
#A list that contains the entire forest
r$forest
#Obtain an individual tree
getTree(r,k=1)
getTree(r,k=2)
getTree(r,k=200)
#vector error rates of the prediction on the input data
head(r$err.rate)
#Confusion Matrics
r$confusion
#predicted values
r$predicted
table(r$predicted)
r$test
r$votes
#Out of Bag (OOB) data
r$oob.times

print(r)


# plots
# install if necessary
# install.packages(ggplot2)
library(ggplot2)
class_1_importance <- data.frame(feature=names(r$importance[,1]), importance=r$importance[,1])
ggplot(class_1_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

class_2_importance <- data.frame(feature=names(r$importance[,2]), importance=r$importance[,2])
ggplot(class_2_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

class_3_importance <- data.frame(feature=names(r$importance[,3]), importance=r$importance[,3])
ggplot(class_2_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

boxplot(df$colour ~ df$class, main="Colour by class")
boxplot(df$alcohol ~ df$class, main="Alcohol by class")

#Scatter plot of alcohol versus colour by class
ggplot(df, aes(x=alcohol, y=colour, colour=class)) + geom_point()




#######################
#######Reference#######
#######################
# Code Example Explanation - https://github.com/davetang/learning_random_forest/blob/master/random_forest.pdf
# Code Example - https://github.com/davetang/learning_random_forest
# Random Forest CRAN Document - https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
#######################################################
######################################################

## Regression:
## data(airquality)
summary(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)
#R SQuare error, only available in regression
# goodness-of-fit measure for linear regression models
ozone.rf$rsq 
#Mean Squiare error, only available in regression
#measure of the dispersion of sample means around the population mean
ozone.rf$mse
