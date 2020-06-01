# Tp1 Linear Regression 

tab = read.table(file.choose(), header = T)
x = tab[,2]
y= tab[,3]
library(ggplot2)
plot(x, y, main="age vs height", ylab="height", xlab="age", col="blue", type="p")
qplot(x, y, xlab = 'age', ylab = 'height', main = 'age vs height', col='red')
library(caTools)
#fiting simple linear regression 
regressor = lm(formula = y ~ x, data= tab)
summary(regressor)
# value of theta0 = 0.75 and value of theta 1 = 0.06
#vresidual error = 0.45
# equation => Y= 0.75 + 0.06x
abline(regressor)
#predicting the test set result

predict(regressor, newdata = data.frame('x'=3))
predict(regressor, newdata = data.frame('x'=7))
predict(regressor, newdata = data.frame('x'=9))
predict(regressor, newdata = data.frame('x'=12))
# confidence interval 95%
predict(regressor, newdata = data.frame('x'=10), interval="confidence", level=0.95)


