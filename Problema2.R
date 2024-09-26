#Codigo para problema 2
iris
mis_dadas <- iris

y <- mis_dadas$Sepal.Length
y

x <- mis_dadas$Petal.Length
x

plot(x,y)

xbar = mean(x)
xbar

ybar= mean(y)
ybar

m=sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
m
b= ybar-m*xbar
b
pred= b+m*1.5
pred

mod=lm(y~x)

ypredicted=predict(mod,data.frame(x=x))
ypredicted

plot(x,ypredicted, type="l")
points(x,y)
plot(x,y, pch=16, col="red")
lines(x, ypredicted)

Rsq = sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
Rsq

summary(mod)
sqrt(Rsq)
cor.test(x,y)
