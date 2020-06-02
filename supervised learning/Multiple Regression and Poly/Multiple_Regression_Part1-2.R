# Linear regression withe multiple variable 
# 
# ---------------------      Partie 1     --------------------------------
LungCapData <- read.table(file.choose(), header = T, sep="\t")
attach(LungCapData)
names(LungCapData)
class(Age)
class(Smoke)
levels(Smoke)
model1 <- lm(LungCap ~ Age + Height)
summary(model1)
cor(Age, Height, method="pearson")
confint(model1, coef.level=0.95)
model2 <- lm(LungCap ~ Age + Height + Smoke + Gender +
               Caesarean)
summary(model2)
plot(model2)

# ---------------------      Partie 2     --------------------------------

library('MASS')
data = Boston
attach(data)
names(data)
str(data)
training_set = data[0:400,]
test_set = data[401:506,]

# relation entre 2 varibles

cor(training_set$age,training_set$medv)
plot(training_set$age,training_set$medv,type = 'p',col='red',  main='age vs medv', xlab = 'age', ylab = 'medv')

cor(training_set$medv,training_set$lstat)
plot(training_set$lstat,training_set$medv,type = 'p', col='blue', main='lstat vs medv', xlab = 'lstat', ylab = 'medv')

regressor = lm(formula = medv ~ lstat, data = training_set)
abline(regressor,col='red')
summary(regressor)
pairs(Boston)
pairs(Boston[,c(1,3,7)])

m1= lm(formula = medv ~ lstat + age, data = training_set)
summary(m1)

m2= lm(formula = medv ~ log(lstat) + age, data = training_set)
summary(m2)

m3= lm(formula = medv ~., data = training_set)
summary(m3)

m4= lm(formula = medv ~. -indus-age-black, data = training_set)
summary(m4)

m5= lm(formula = medv ~ lstat * age, data = training_set)
summary(m5)

# le modele non linear 

d2 = lm(medv~ poly(lstat,2) , data = training_set)
summary(d2)

d3 = lm(medv~ poly(lstat,3) , data = training_set)
summary(d3)

d4 = lm(medv~ poly(lstat,4) , data = training_set)
summary(d4)

d5 = lm(medv~ poly(lstat,5) , data = training_set)
summary(d5)

d6 = lm(medv~ poly(lstat,6) , data = training_set)
summary(d6)

d7 = lm(medv~ poly(lstat,7) , data = training_set)
summary(d7)





