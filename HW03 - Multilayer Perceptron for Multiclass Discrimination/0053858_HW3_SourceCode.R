#Q2
image <- read.csv("hw03_data_set_images.csv", header = FALSE)
label <- read.csv("hw03_data_set_labels.csv", header = FALSE)

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
X = data.matrix(trainData)

y_train <- matrix(0, nrow = 125, ncol = 5)
y_train[1:25, 1] <- 1
y_train[26:50, 2] <- 1
y_train[51:75, 3] <- 1
y_train[76:100, 4] <- 1
y_train[101:125, 5] <- 1

safelog <- function(x) {
  return (log(x + 1e-100))
}

sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

softmax <- function(a) {
  scores <- exp(a)
  scores <- scores / rowSums(scores)
  return (scores)
}

eta <- 0.005
epsilon <- 1e-3
H <- 20
max_iteration <- 200

set.seed(521)
W <- matrix(runif((320) * H, min = -0.01, max = 0.01), 320, H)
v <- matrix(runif((5) * 20, min = -0.01, max = 0.01), 5, H)

Z <- sigmoid(X %*% W)
y_predicted <- softmax(Z %*% t(v))
objective_values <- -sum(y_train * safelog(y_predicted))

iteration <- 1
while (1) {
  for (i in sample(125)) {
    Z[i,] <- sigmoid(X[i,] %*% W)
    y_predicted[i,] <- softmax(Z[i, ] %*% t(v))
    
    v <- v + eta * (y_train[i,] - y_predicted[i,]) %*% t(Z[i,])
    W <- W + eta * X[i,] %*% (((y_train[i,] - y_predicted[i,]) %*% v)*Z[i,]*(1-Z[i,]))
  }

  Z <- sigmoid(X %*% W)
  y_predicted <- softmax(Z %*% t(v))
  objective_values <- c(objective_values, -sum(y_train * safelog(y_predicted)))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}

#Q5
plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

#Q6
Y_train <- matrix(nrow = 125, ncol = 1)
Y_train[1:25, 1] <- label[1:25, ]
Y_train[26:50, 1] <- label[40:64, ]
Y_train[51:75, 1] <- label[79:103, ]
Y_train[76:100, 1] <- label[118:142, ]
Y_train[101:125, 1] <- label[157:181, ]

Y_predicted <- apply(y_predicted, 1, which.max)
confusion_matrix <- table(Y_predicted, Y_train)
print(confusion_matrix)

#Q7
Xtest <- data.matrix(testData)

y_predicted = Xtest %*% W %*% t(v)

Y_test <- matrix(nrow = 70, ncol = 1)
Y_test[1:14, 1] <- label[26:39, ]
Y_test[15:28, 1] <- label[65:78, ]
Y_test[29:42, 1] <- label[104:117, ]
Y_test[43:56, 1] <- label[143:156, ]
Y_test[57:70, 1] <- label[182:195, ]

Y_predicted <- apply(y_predicted, 1, which.max)
confusion_matrix <- table(Y_predicted, Y_test)
print(confusion_matrix)