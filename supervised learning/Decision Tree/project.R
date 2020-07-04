# projet(methode par arbre)

#-------------------- partie 1 donnes synthétique  -----------

rm(list=ls())
data <- read.table(choose.files(), header=TRUE)
dim(data)
data$y <- as.factor(data$y)

library(rpart)
help(rpart)
# construire un arbre 
t = rpart(formula = data$y ~., data = data)
plot(t)
text(t)

pred <- predict(t, newdata=data, type="class")

train_error = sum(data$y!=pred)/length(data$y)
train_error

test <- read.table(choose.files(), header=TRUE)

tpred <- predict(t, newdata=test, type="class")

test_error = sum(test$y!=tpred)/length(test$y)
test_error

# pour arbre maximal training set
arbre_Max <- rpart(y~., data=data, control=rpart.control(minsplit=2,cp=0))
arbre_Max

plot(arbre_Max)
text(arbre_Max)

pred_max <- predict(arbre_Max, newdata=data, type="class")

train_error1 = sum(data$y!=pred_max)/length(data$y)
train_error1

# pour arbre maximal test set
tpred1 <- predict(arbre_Max, newdata=test, type="class")

test_error1 = sum(test$y!=tpred1)/length(test$y)
test_error1

#pour la premiere arbre 
X1 = seq(min(data$x1) - 1, max(data$x1) + 1, by = 0.01)
X2 = seq(min(data$x2) - 1, max(data$x2) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x1', 'x2')
y_grid = predict(t, newdata= grid_set, type="class")

plot(data[,-1],
     main = 'Decision Tree Classification',
     xlab = 'x1', ylab = 'x2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(data[,-1], pch = 21, bg = ifelse(data$y == 1, 'green4', 'red3'))

# pour l'arbre maximal 
y_gridMax = predict(arbre_Max, newdata= grid_set, type="class")

plot(data[,-1],
     main = 'Decision Tree Classification',
     xlab = 'x1', ylab = 'x2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_gridMax), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_gridMax == 1, 'springgreen3', 'tomato'))
points(data[,-1], pch = 21, bg = ifelse(data$y == 1, 'green4', 'red3'))

# -----------  partie 2 Reconnaissance automatique de caractères manuscrits  ------

zip_train = read.table(choose.files(), header=TRUE)
zip_test = read.table(choose.files(), header=TRUE)

x_train = zip_train[,-1]
y_train = zip_train$y
x_train = as.matrix(x_train)
# representation quleque images de l'ensemble training set

par(mfrow=c(2,4))
for(i in 1:5){
        
        image(matrix(x_train[i,], 16,16), col = gray.colors(100), ylim=c(1,0))
        
}

# calcule le taux d'error pour training set 
library(rpart)
zip_train$y = as.factor(zip_train$y)
classifier = rpart(formula = y ~., data = zip_train)


predCart_train = predict(classifier, newdata=zip_train, type="class")

train_error2 = sum(zip_train$y!=predCart_train)/length(zip_train$y)
train_error2
  
# calcule le taux d'error pour test set      

predCart_test = predict(classifier, newdata=zip_test, type="class")

test_error2 = sum(zip_test$y!=predCart_test)/length(zip_test$y)
test_error2






