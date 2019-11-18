#Q1
data_set <- read.csv("hw05_data_set.csv")
train <- data_set[1:100,]
test <- data_set[101:133,]

X_train <- train$x
y_train <- train$y
X_test <- test$x
y_test <- test$y

#Q2
N_train <- length(y_train)
N_test <- length(y_test)

node_means <- c()
node_splits <- c()
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

regression_algorithm <- function(P) {
  while (1) {
    split_nodes <- which(need_split)
    if (length(split_nodes) == 0) {
      break
    }
    
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_means[split_node] <- mean(y_train[data_indices])
      
      if (length((y_train[data_indices])) <= P) {
        is_terminal[split_node] <- TRUE
      } else {
        is_terminal[split_node] <- FALSE
  
        unique_values <- sort(unique(X_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
          split_scores[s] <- 1/(length(left_indices)+length(right_indices))*(sum((y_train[left_indices]
              -mean(y_train[left_indices]))^2)+sum((y_train[right_indices]-mean(y_train[right_indices]))^2))
        }
        
        if (length(unique_values) == 1) {
          is_terminal[split_node] <- TRUE
          next 
        }
        node_splits[split_node] <- split_positions[which.min(split_scores)]
        
        left_indices <- data_indices[which(X_train[data_indices] < node_splits[split_node])]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        right_indices <- data_indices[which(X_train[data_indices] >= node_splits[split_node])]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  return(list(node_splits, node_means, is_terminal))
}

#Q3
P=10
node_splits <- unlist(regression_algorithm(P)[[1]])
node_means <- unlist(regression_algorithm(P)[[2]])
is_terminal <- unlist(regression_algorithm(P)[[3]])

minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

decision <- function(x){
  i <- 1
  while(is_terminal[i]==FALSE){
    if(x <= node_splits[i]){
      i <- 2*i
    } else {
      i <- 2*i+1
    }
  }
  return(node_means[i])
}

plot(X_train, y_train, type = "p", pch = 20, col = "blue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("P = %g", P))
points(X_test, y_test, type = "p", pch = 20, col = "red")
legend(53, 83, legend=c("training", "test"), pch=20, col=c("blue", "red"), cex=0.7)
for(b in 1:length(data_interval)){
  lines(c(data_interval[b], data_interval[b+1]), c(decision(data_interval[b]), decision(data_interval[b])),
        lwd = 1.5, col = "black")
  if (b < length(data_interval)) {
    lines(c(data_interval[b+1], data_interval[b+1]), c(decision(data_interval[b]), decision(data_interval[b+1])),
          lwd = 1.5, col = "black") 
  }
}

#Q4
error <- function(P){
  rmse <- 0
  for (b in 1:length(y_test)) {
    rmse <- rmse + (y_test[b]-decision(X_test[b]))^2
  }
  rmse <- sqrt(rmse / length(y_test))
  return(rmse)
}
rmse <- error(P)
paste0("RMSE is ", format(round(rmse, 4), nsmall=4), " when P is ", P)

#Q5
rmse_list <- c()
for (i in 1:20){
  node_splits <- unlist(regression_algorithm(i)[[1]])
  node_means <- unlist(regression_algorithm(i)[[2]])
  is_terminal <- unlist(regression_algorithm(i)[[3]])
  rmse <- error(i)
  rmse_list <- c(rmse_list, rmse)
}

plot(1:20, rmse_list, type = "b", lwd = 1.5, las = 1, pch = 1, lty = 1,
     cex=0.7, cex.axis=0.7, cex.lab=0.7, xlab = "P", ylab = "RMSE")

