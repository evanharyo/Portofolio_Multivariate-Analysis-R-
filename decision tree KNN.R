packages <- c("Hmisc","Formula", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class")
install.packages(packages)
lapply(packages, library, character.only = TRUE)
library( c("Hmisc","Formula", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class"))

data<- read.csv("C:/Users/Andri/Downloads/winequality-red (2).csv", sep=";")
data <- as.data.frame(data)
data
str(data)

sapply(data, function(x) sum(is.na(x)))

library(mvnormtest)
mshapiro.test(t(data))
              
data$type <- as.factor(ifelse(data$quality <= 5, 'Kurang Baik', 'Cukup Baik'))
data
str(data)

library(tree)
?tree
quality.tree <- tree(type~.-quality,data = data)
summary(quality.tree)
options(repr.plot.width = 13, repr.plot.height = 7, repr.plot.res = 100)
plot(quality.tree)
text(quality.tree, pretty=0)

set.seed(40)
tree.train <- sample(1:nrow(data),250)
quality.tree <- tree(type~.-quality,data,subset=tree.train) 
options(repr.plot.width = 13, repr.plot.height = 7, repr.plot.res = 100)
plot(quality.tree)
text(quality.tree, pretty=0)

quality.pred = predict(quality.tree, data[-tree.train,], type="class")
with(data[-tree.train,], table(quality.pred, type))

quality.cv = cv.tree(quality.tree, FUN = prune.misclass)
quality.cv
plot(quality.cv)

quality.prune = prune.misclass(quality.tree, best = 12)
plot(quality.prune)
text(quality.prune, pretty=0)

quality.pred = predict(quality.prune, data[-tree.train,], type="class")
with(data[-tree.train,], table(quality.pred, type))


#KNN
# Count the number of signs of each type
table(data$type)
library(class)
# Use kNN to identify the test road signs
data_types <- data$type
data_pred <- knn(train = data[-13], test = data[-13], cl = data_types)

# Create a confusion matrix of the predicted versus actual values
data_actual <- data$type
table(data_pred, data_actual)

# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = data[-13], test = data[-13], cl = data_types)
mean(k_1 == data_actual)

# Modify the above to set k = 7
k_7 <- knn(train = data[-13], test = data[-13], cl = data_types, k = 7)
mean(k_7 == data_actual)

# Use the prob parameter to get the proportion of votes for the winning class
data_pred <- knn(train = data[-13], test = data[-13], cl = data_types, k = 7, prob = TRUE)


# Get the "prob" attribute from the predicted classes
data_prob <- attr(data_pred, "prob")

# Examine the first several predictions
head(data_pred)

# Examine the proportion of votes for the winning class
head(data_prob)
