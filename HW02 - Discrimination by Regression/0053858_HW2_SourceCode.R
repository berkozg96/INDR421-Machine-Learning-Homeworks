#Q2
image <- read.csv("hw02_data_set_images.csv", header = FALSE)
label <- read.csv("hw02_data_set_labels.csv", header = FALSE)

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
sigmoid <- function(X, W, w0) {
  scores <- 1 / (1 + exp(-(X %*% W + w0)))
  return (scores)
}

gradient_W <- function(X, Y_truth, Y_predicted) {
  return (t(X) %*% ((Y_truth - Y_predicted)*Y_predicted*(1-Y_predicted)))
}

gradient_w0 <- function(Y_truth, Y_predicted) {
  return (colSums((Y_truth - Y_predicted)*Y_predicted*(1-Y_predicted)))
}

X <- data.matrix(trainData)

y_train <- matrix(0, nrow = 125, ncol = 5)
y_train[1:25, 1] <- 1
y_train[26:50, 2] <- 1
y_train[51:75, 3] <- 1
y_train[76:100, 4] <- 1
y_train[101:125, 5] <- 1

eta <- 0.01
epsilon <- 1e-3
set.seed(521)

W <- matrix(runif(ncol(X), min = -0.01, max = 0.01), ncol(X), 5)
w0 <- runif(5, min = -0.01, max = 0.01)
w0ext <- matrix(nrow = 125, ncol = 5)
w0ext[,1] = w0[1]
w0ext[,2] = w0[2]
w0ext[,3] = w0[3]
w0ext[,4] = w0[4]
w0ext[,5] = w0[5]

iteration <- 1
objective_values <- c()

while (1) {
  Y_predicted <- sigmoid(X, W, w0)
  
  objective_values <- c(objective_values, 1/2*sum((y_train-Y_predicted)^2))
  
  W_old <- W
  w0_old <- w0
  
  W <- W + eta * gradient_W(X, y_train, Y_predicted)
  w0 <- w0 + eta * gradient_w0(y_train, Y_predicted)
  
  if (sqrt(sum((-eta * gradient_w0(y_train, Y_predicted))^2) + sum((-eta * gradient_W(X, y_train, Y_predicted))^2)) < epsilon) {
    break
  }
    iteration <- iteration + 1
}

#Q5
plot(1:iteration, objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

#Q6
Y_train <- matrix(nrow = 125, ncol = 1)
Y_train[1:25, 1] <- label[1:25, ]
Y_train[26:50, 1] <- label[40:64, ]
Y_train[51:75, 1] <- label[79:103, ]
Y_train[76:100, 1] <- label[118:142, ]
Y_train[101:125, 1] <- label[157:181, ]

y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, Y_train)
print(confusion_matrix)

#Q7
Xtest <- data.matrix(testData)

w0ext <- matrix(nrow = 70, ncol = 5)
w0ext[,1] = w0[1]
w0ext[,2] = w0[2]
w0ext[,3] = w0[3]
w0ext[,4] = w0[4]
w0ext[,5] = w0[5]

Y_predicted = Xtest %*% W + w0ext

Y_test <- matrix(nrow = 70, ncol = 1)
Y_test[1:14, 1] <- label[26:39, ]
Y_test[15:28, 1] <- label[65:78, ]
Y_test[29:42, 1] <- label[104:117, ]
Y_test[43:56, 1] <- label[143:156, ]
Y_test[57:70, 1] <- label[182:195, ]

y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, Y_test)
print(confusion_matrix)