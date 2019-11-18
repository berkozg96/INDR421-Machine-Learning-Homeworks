#Q2
image <- read.csv("hw01_data_set_images.csv", header = FALSE)
label <- read.csv("hw01_data_set_labels.csv", header = FALSE)

#Q3
trainA <- image[1:25, ]
testA <- image[26:39, ]
trainB <- image[40:64, ]
testB <- image[65:78, ]
trainC <- image[79:103, ]
testC <- image[104:117, ]
trainD <- image[118:142, ]
testD <- image[143:156, ]
trainE <- image[157:181, ]
testE <- image[182:195, ]

trainData <- rbind(trainA, trainB, trainC, trainD, trainE)
testData <- rbind(testA, testB, testC, testD, testE)

#Q4
pcd <- matrix(nrow = 320, ncol = 5)
pcd[,1] <- colMeans(trainA)
pcd[,2] <- colMeans(trainB)
pcd[,3] <- colMeans(trainC)
pcd[,4] <- colMeans(trainD)
pcd[,5] <- colMeans(trainE)

print(pcd[,1])
print(pcd[,2])
print(pcd[,3])
print(pcd[,4])
print(pcd[,5])

#Q5 
y_train <- matrix(nrow = 125, ncol = 1)
y_train[1:25, 1] <- label[1:25, ]
y_train[26:50, 1] <- label[40:64, ]
y_train[51:75, 1] <- label[79:103, ]
y_train[76:100, 1] <- label[118:142, ]
y_train[101:125, 1] <- label[157:181, ]

class_priors <- sapply(X = 1:5, FUN = function(c) {mean(y_train == c)})

scoreA <- data.matrix(trainData) %*% log(pcd[,1]+1e-100) + (1-data.matrix(trainData)) %*% log(1-pcd[,1]+1e-100) + log(class_priors[1])
scoreB <- data.matrix(trainData) %*% log(pcd[,2]+1e-100) + (1-data.matrix(trainData)) %*% log(1-pcd[,2]+1e-100) + log(class_priors[2])
scoreC <- data.matrix(trainData) %*% log(pcd[,3]+1e-100) + (1-data.matrix(trainData)) %*% log(1-pcd[,3]+1e-100) + log(class_priors[3])
scoreD <- data.matrix(trainData) %*% log(pcd[,4]+1e-100) + (1-data.matrix(trainData)) %*% log(1-pcd[,4]+1e-100) + log(class_priors[4])
scoreE <- data.matrix(trainData) %*% log(pcd[,5]+1e-100) + (1-data.matrix(trainData)) %*% log(1-pcd[,5]+1e-100) + log(class_priors[5])

scores <- cbind(scoreA, scoreB, scoreC, scoreD, scoreE)
y_hat <- apply(X = scores, MARGIN = 1, FUN = which.max)
table(y_hat, y_train)

#Q6
y_test <- matrix(nrow = 70, ncol = 1)
y_test[1:14, 1] <- label[26:39, ]
y_test[15:28, 1] <- label[65:78, ]
y_test[29:42, 1] <- label[104:117, ]
y_test[43:56, 1] <- label[143:156, ]
y_test[57:70, 1] <- label[182:195, ]

testScoreA <- data.matrix(testData) %*% log(pcd[,1]+1e-100) + (1-data.matrix(testData)) %*% log(1-pcd[,1]+1e-100) + log(class_priors[1])
testScoreB <- data.matrix(testData) %*% log(pcd[,2]+1e-100) + (1-data.matrix(testData)) %*% log(1-pcd[,2]+1e-100) + log(class_priors[2])
testScoreC <- data.matrix(testData) %*% log(pcd[,3]+1e-100) + (1-data.matrix(testData)) %*% log(1-pcd[,3]+1e-100) + log(class_priors[3])
testScoreD <- data.matrix(testData) %*% log(pcd[,4]+1e-100) + (1-data.matrix(testData)) %*% log(1-pcd[,4]+1e-100) + log(class_priors[4])
testScoreE <- data.matrix(testData) %*% log(pcd[,5]+1e-100) + (1-data.matrix(testData)) %*% log(1-pcd[,5]+1e-100) + log(class_priors[5])

testScores <- cbind(testScoreA, testScoreB, testScoreC, testScoreD, testScoreE)
y_hat <- apply(X = testScores, MARGIN = 1, FUN = which.max)
table(y_hat, y_test)
