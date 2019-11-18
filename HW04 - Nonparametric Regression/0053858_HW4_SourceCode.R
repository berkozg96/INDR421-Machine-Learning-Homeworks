#Q2
data_set <- read.csv("hw04_data_set.csv")
train <- data_set[1:100,]
test <- data_set[101:133,]

x_train <- train$x
y_train <- train$y
x_test <- test$x
y_test <- test$y

#Q3
minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

bin_width <- 3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)

p_head <- sapply(1:length(left_borders), function(b) {(mean(y_train[left_borders[b] < x_train & x_train <= right_borders[b]]))})

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend(53, 83, legend=c("training", "test"), pch=20, col=c("blue", "red"), cex=0.7)
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

#Q4
rmse <- 0
for (b in 1:length(left_borders)) {
  rmse <- rmse + sum((y_test[left_borders[b] < x_test & x_test <= right_borders[b]]-p_head[b])^2)
}
rmse <- sqrt(rmse / length(y_test))
paste0("Regressogram => RMSE is ", format(round(rmse, 4), nsmall=4), " when h is ", bin_width)

#Q5
bin_width <- 3
p_head <- sapply(data_interval, function(x) {mean(y_train[abs((x-x_train)/bin_width) <= 1/2])})

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend(53, 83, legend=c("training", "test"), pch=20, col=c("blue", "red"), cex=0.7)
lines(data_interval, p_head, type = "l", lwd = 2, col = "black")

#Q6
rmse <- 0
for (b in 1:length(y_test)) {
  rmse <- rmse + (y_test[b]-p_head[x_test[b]*100+1])^2
}
rmse <- sqrt(rmse / length(y_test))
paste0("Regressogram => RMSE is ", format(round(rmse, 4), nsmall=4), " when h is ", bin_width)

#Q7
bin_width <- 1
p_head <- sapply(data_interval, function(x) {sum((1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))*y_train)/sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))})

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend(53, 83, legend=c("training", "test"), pch=20, col=c("blue", "red"), cex=0.7)
lines(data_interval, p_head, type = "l", lwd = 2, col = "black")

#Q8
rmse <- 0
for (b in 1:length(y_test)) {
  rmse <- rmse + (y_test[b]-p_head[x_test[b]*100+1])^2
}
rmse <- sqrt(rmse / length(y_test))
paste0("Regressogram => RMSE is ", format(round(rmse, 4), nsmall=4), " when h is ", bin_width)
