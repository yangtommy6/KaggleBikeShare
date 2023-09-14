y <- c(1, 6.6, 8)
mean(y)
median(y)


#Absolute error loss
abs_loss <- NA
candidates <- seq(0,9, by = .1)
candidates

for(i in 1:length(candidates)){
  abs_loss[i] <- sum(abs(y - candidates[i]))
}
par(mfrow = c(1,2)) #spilt plotting window into 1 row and 2 cols
plot(x = candidates,
     y = abs_loss,
     type = "l",
     col = "red",
     xlab = "Candidates",
     ylab = "Absolute Error Loss")
abline(v=mean(y))
abline(v = median(y),
       col = "red",
       lty = 2)



#Squared error loss

#Absolute error loss
sqr_loss <- NA
candidates <- seq(0,9, by = .1)
candidates

for(i in 1:length(candidates)){
  sqr_loss[i] <- sum(abs(y - candidates[i])^2)
}
par(mfrow = c(1,2)) #spilt plotting window into 1 row and 2 cols
plot(x = candidates,
     y = sqr_loss,
     type = "l",
     col = "blue",
     xlab = "Candidates",
     ylab = "Squared Error Loss")
abline(v=mean(y), col = "blue", lty = 2)
abline(v = median(y),
       col = "red",
       lty = 2)
#mean is easier to do!!!
#Regression is to make a model where y = E(expected)( Y to X ) + Error
#>>>>>> Y is what we are trying to model
