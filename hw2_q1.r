
library("mlbench")
data("Ionosphere")
X = apply(subset(Ionosphere[,3:34]), 2, as.numeric)

#create two matrices good and bad
good=apply(subset(Ionosphere[,3:34], Ionosphere$Class=="good"), 2, as.numeric)
bad=apply(subset(Ionosphere[,3:34], Ionosphere$Class=="bad"), 2, as.numeric)

#compute covariance of classes good and bad each
cov_good = cov(good)
cov_bad = cov(bad)

#compute mean of good and bad
mean_good=matrix(colMeans(good))
mean_bad=matrix(colMeans(bad))

#compute mahalanobis distance
mahalanobis_good = mahalanobis(X, t(mean_good), solve(cov_good), TRUE)
mahalanobis_bad = mahalanobis(X, t(mean_bad), solve(cov_bad), TRUE)

#compute confusion matrix
conf_matrix=ifelse(mahalanobis_good < mahalanobis_bad, "good", "bad")

#create confusion matrix in table format
output=table(Ionosphere$Class, conf_matrix)

#print the table output
print(output)