
library("rpart")
library("tree")

sb = read.csv("strange_binary.csv")
tree = rpart(sb$c ~ .,data = sb, method ="class",parms = list(split = 'gini'))
print(tree)

summary(tree)
split=tree$cptable[which(tree$cptable[,"nsplit"] == 3),"CP"]

pruned = prune(tree,cp=split)

plot(pruned, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pruned, use.n=TRUE, all=TRUE,cex=.8)

summary(pruned)
summary(tree(pruned))

