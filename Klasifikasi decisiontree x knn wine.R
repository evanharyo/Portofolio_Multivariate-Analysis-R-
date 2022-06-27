#DECIDION TREE
shapiro.test(wine$quality)
library(tree)
library(ISLR)
data <- wine
data <- as.data.frame(data)
data
winecat<-as.factor(ifelse(data$quality<=5,"bad","good"))
data<-data.frame(data,winecat)
str(data)
wine.tree<-tree(winecat~.-quality,data=data)
summary(wine.tree)
options(repr.plot.width = 13, repr.plot.height = 7, repr.plot.res = 100)
plot(wine.tree)
text(wine.tree, pretty=0)

set.seed(40)
tree.train <- sample(1:nrow(data),250)
wine.tree <- tree(winecat~.-quality,data,subset=tree.train) 

options(repr.plot.width = 13, repr.plot.height = 7, repr.plot.res = 100)
plot(wine.tree)
text(wine.tree, pretty=0)
wine.pred = predict(wine.tree, data[-tree.train,], type="class")
with(data[-tree.train,], table(wine.pred, winecat))

quality.cv = cv.tree(wine.tree, FUN = prune.misclass)
quality.cv
plot(quality.cv)
quality.prune = prune.misclass(wine.tree, best = 12)
plot(quality.prune)
text(quality.prune, pretty=0)

quality.pred = predict(quality.prune, data[-tree.train,], type="class")
with(data[-tree.train,], table(wine.pred, winecat))


#KNN
# Count the number of signs of each type
table(winecat)

# Use kNN to identify the test road signs
data_types <- winecat
data_pred <- knn(train = data[-13], test = data[-13], cl = data_types)
# Create a confusion matrix of the predicted versus actual values
data_actual <- winecat
table(data_pred, data_actual)
mean(data_pred == data_actual)

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
