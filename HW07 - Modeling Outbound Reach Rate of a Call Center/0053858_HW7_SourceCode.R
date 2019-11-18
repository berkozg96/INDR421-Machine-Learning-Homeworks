library(AUC)

X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)
Y_train <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])

safelog <- function(x) {
  return (log(x + 1e-100))
}

normalizer <- function(mtrx) {
  norm_mtrx <- matrix(nrow = nrow(mtrx), ncol = 2, 0)
  norm_mtrx[,1] <- mtrx[,1]/(mtrx[,1]+mtrx[,2])
  norm_mtrx[,2] <- mtrx[,2]/(mtrx[,1]+mtrx[,2])
  return (norm_mtrx)
}

pcd <- matrix(nrow = length(X_train), ncol = 2)
pcd[,1] <- colMeans(X_train[Y_train==0,])
pcd[,2] <- colMeans(X_train[Y_train==1,])

norm_pcd <- normalizer(pcd)

norm_pcd <- replace(norm_pcd, is.nan(norm_pcd), 0.5)

class_priors <- sapply(X = 0:1, FUN = function(c) {mean(Y_train == c)})

score_values <- data.matrix(X_train) %*% safelog(norm_pcd) + (1-data.matrix(X_train)) %*% safelog(1-norm_pcd) + log(class_priors)
log_posteriors <- score_values - sapply(X = 1:nrow(score_values), FUN = function(r) {max(score_values[r,]) + log(sum(exp(score_values[r,] - max(score_values[r,]))))})

class_assignments <- apply(X = log_posteriors, MARGIN = 1, FUN = which.max)
table(class_assignments, Y_train)

roc_curve <- roc(predictions = log_posteriors[,2], labels = Y_train)
auc(roc_curve)

plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

##########

score_values <- data.matrix(X_test) %*% safelog(norm_pcd) + (1-data.matrix(X_test)) %*% safelog(1-norm_pcd) + log(class_priors)
log_posteriors <- score_values - sapply(X = 1:nrow(score_values), FUN = function(r) {max(score_values[r,]) + log(sum(exp(score_values[r,] - max(score_values[r,]))))})

class_assignments <- apply(X = log_posteriors, MARGIN = 1, FUN = which.max)
class_assignments <- class_assignments-1

write.table(class_assignments, file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)
