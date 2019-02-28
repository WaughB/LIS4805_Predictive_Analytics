# Brett Waugh
# 27 February 2019
# week8_lab.R
# Decision Trees

require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8, "No", "Yes")
Carseats=data.frame(Carseats, High)

tree.carseats=tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

set.seed(1011)
train=sample(1:nrow(Carseats), 250)
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
plot(tree.carseats); text(tree.carseats, pretty = 0)
tree.pred=predict(tree.carseats, Carseats[-train,], type="class")
with(Carseats[-train,], table(tree.pred, High))

cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats=prune.misclass(tree.carseats, best = 13)
plot(prune.carseats); text(prune.carseats, pretty=0)

tree.pred=predict(prune.carseats, Carseats[-train,], type = "class")
with(Carseats[-train,], table(tree.pred, High))
