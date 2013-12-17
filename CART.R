setwd("../../../Documents/R practice/")
calif = read.table("cadata.dat", header = TRUE)

if(!require(tree)) install.packages("tree")
require(tree)
treefit = tree(log(MedianHouseValue)~ Longitude + Latitude, data = calif)

## plot of the classification trees
plot(treefit); text(treefit)

## overlay the classification on the plot of medican house value by latitude against longitude.
price.deciles = quantile(calif$MedianHouseValue, 0:10/10)
cut.prices = cut(calif$MedianHouseValue, price.deciles, include.lowest=TRUE)
plot(Latitude ~ Longitude, data = calif, col = grey(10:2/11)[cut.prices], pch = 19, xlab="Longitude", ylab = "Latitude")
partition.tree(treefit, ordvars=c("Longitude", "Latitude"), add = TRUE)

treefit2 = tree(log(MedianHouseValue)~ Longitude + Latitude, data = calif, mindev = 0.001)
treefit = treefit2

treefit3 = tree(log(MedianHouseValue) ~ ., data = calif)
plot(treefit3); text(treefit3)


## Prune the tree, with certain certeria.
prune.tree(treefit, best = 5)

my.tree = prune.tree(treefit)
plot(my.tree)

opt.trees = which(my.tree$dev == min(my.tree$dev))

my.tree$size[opt.trees]

## use cross validation to fit the best tree
my.tree = cv.tree(treefit, best = 6)

