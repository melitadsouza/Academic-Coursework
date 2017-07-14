library(rpart)
library(tree)
sb=read.csv("strange_binary.csv")
sb$v11 = rowSums(sb==0)*rowSums(sb==1)
tree = rpart(sb$c ~ .,data = sb, method ="class",parms = list(split = 'gini'),control= rpart.control(xval = 10, cp = 0))
print(tree)
split = tree$cptable[which(tree$cptable[,"nsplit"] == 3),"CP"]
pruned = prune(tree,cp=split)
summary(tree(pruned))

