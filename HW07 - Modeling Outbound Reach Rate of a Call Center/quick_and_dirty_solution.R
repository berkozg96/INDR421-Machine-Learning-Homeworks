library(tree)
library(AUC)

X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)

Y_train <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])

tree_classifier <- tree(y ~ ., data = cbind(X_train, y = Y_train))
training_scores <- predict(tree_classifier, X_train)

class_assignments <- apply(X = training_scores[,2], MARGIN = 1, FUN = which.max)
table(class_assignments, y_train)
#                 y_train
#class_assignments      0      1
#                1 254650  45350

roc_curve <- roc(predictions = training_scores[,2], labels = Y_train)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

test_scores <- predict(tree_classifier, X_test)
write.table(test_scores[,2], file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)
