# simulating fake data 
x_1 <- rnorm(1000,5,7) #1000 values with mean of ~5 and sd ~7
hist(x_1, col = "grey")

true_error <- rnorm(1000,0,2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2

y <- true_beta_0 + true_beta_1*x_1 + true_error
hist(y)
plot(x_1, y, pch = 20, col = "red")

# 1. Does regression model recover true values of the Betas? 
model <- lm(y ~ x_1)
summary(model)
# running the linear regression model does seem to recover the true betas
# Beta1 in model is -8.2 and Beta0 in model is 1.2 

# Additional notes
  # model$fitted.values (returns the values from plugging each i (observation) into the equation)
  # model$residuals (returns the values of the residual, which is the observed value - fitted value)

# 2. 
  # a. simulate another fake variable x2 that has a gamma distribution with parameters you pick
  x_2 <- rgamma(1000, shape = 2, rate = .5)
  mean(x_2) # 2 * 1/.5 (2)
  var(x_2) # 2 * (1/.5)^2 (2^2)
  