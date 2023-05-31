library(tidyverse)

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
# Beta1 in model is around -8.2 and Beta0 in model is around 1.1

# Additional notes
  # model$fitted.values (returns the values from plugging each i (observation) into the equation)
  # model$residuals (returns the values of the residual, which is the observed value - fitted value)

# 2. 
  # a. simulate another fake variable x2 that has a gamma distribution with parameters you pick
  x_2 <- rgamma(1000, shape = 2, rate = .5)
  mean(x_2) # 2 * 1/.5 (2)
  var(x_2) # 2 * (1/.5)^2 (2^2)
  # b. make the truth be that y is a linear combination of both x1 and x2
  true_beta_0 <- 1.1
  true_beta_1 <- -8.2
  true_beta_2 <- 16 
  true_error <- rnorm(1000,0,2)
  y <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_error
  # c. fit a model that only depends on x_1 
  model2_a <- lm(y ~ x_1)
  summary(model2_a)
  # d. fit a model that only depends on x_2 
  model2_b <- lm(y ~ x_2)
  summary(model2_b)
  # e. fit a model that uses both
  model2_c <- lm(y ~ x_1 + x_2)
  summary(model2_c)
  
  ## the model that only depends on x_1 recovers the true slope for x_1, but not the intercept
  ## the model that only depends on x_2 recovers the true slope for x_2, but not the intercept
  ## the model that depends on both x_1 and x_2 recovers the true slope for both variables and the intercept
  
  # f. vary the sample size and make a plot of mean square error of the training set and 
  # of the test set versus sample size
  
  # calculating mse of model2_c 
  mean(model2_c$residuals^2)
  # calculating mse (unbiased) of model2_c
  model2_c_mse <- sum(model2_c$residuals^2)/(1000-2)
  
  df <- data.frame(y, x_1, x_2)
  df <- df %>% mutate(u_id = 1:nrow(df))
  
  set.seed(17)
  
  s_frac <- c(.20, .40, .60, .80)
  mse_train <- c()
  mse_test <- c()
  size_train <- c()
  
  for(i in 1:length(s_frac)){
    
    train <- df %>% dplyr::sample_frac(s_frac[i])
    test  <- dplyr::anti_join(df, train, by = 'u_id')
    
    model_train <-  lm(y ~ x_1 + x_2, data = train)
    model_test <- lm(y ~ x_1 + x_2, data = test)
    
    mse_train[i] <- sum(model_train$residuals^2)/(nrow(train)-2)
    mse_test[i] <- sum(model_test$residuals^2)/(nrow(test)-2)
    
    size_train[i] <- nrow(train)
  }
  
  plot(size_train, mse_train, col = "green", type = "p", pch = 1)
  lines(size_train, mse_test, col = "blue", type = "p", pch = 2)
  abline(h = model2_c_mse, col = "red") 
  legend("bottomleft", legend = c("mse (train)", "mse (test)"), col = c("green", "blue"),
         pch = 1:2)
  
# 3. Create a new variable, z, that is equal to (x_1)^2. Include this as one of the predictors
# in your model. See what happens when you fit a model that depends on x1 only and then also on z
  z <- x_1^2
  true_beta_0 <- 1.1
  true_beta_1 <- -8.2
  true_beta_2 <- 16 
  true_beta_3 <- 60 
  true_error <- rnorm(1000,0,2)
  y <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_beta_3*z + true_error
  model3 <- lm(y ~ x_1 + x_2 + z)
  summary(model3)
  
  # mse of model 3
  model3_mse <- sum(model3$residuals^2)/1000-2
  
  # a. model that depends on x1 only
  model3_a <- lm(y ~ x_1)
  summary(model3_a)
  # b. model that depends on z only
  model3_b <- lm(y ~ z)
  summary(model3_b)

  # the model for x_1 only does not recover the true slope for x_1 
  # the model for z only does appear to recover the true slope for z
  
  df2 <- data.frame(y, x_1, x_2, z) 
  df2 <- df2 %>% mutate(u_id = 1:nrow(df2))
  
  
  s_frac <- c(.20, .40, .60, .80, .90, .95, .98, .99)
  mse_train_q3 <- c()
  mse_test_q3 <- c()
  size_train_q3 <- c()
  
  for(i in 1:length(s_frac)){
    
    train <- df2 %>% dplyr::sample_frac(s_frac[i])
    test  <- dplyr::anti_join(df2, train, by = 'u_id')
    
    model_train <-  lm(y ~ x_1 + x_2 + z, data = train)
    model_test <- lm(y ~ x_1 + x_2 + z, data = test)
    
    mse_train_q3[i] <- sum(model_train$residuals^2)/(nrow(train)-2)
    mse_test_q3[i] <- sum(model_test$residuals^2)/(nrow(test)-2)
    
    size_train_q3[i] <- nrow(train)
  }
  
  plot(size_train_q3, mse_train_q3, col = "green", type = "p", pch = 1)
  lines(size_train_q3, mse_test_q3, col = "blue", type = "p", pch = 2)
  abline(h = model3_mse, col = "red") 
  legend("bottomleft", legend = c("mse (train)", "mse (test)"), col = c("green", "blue"),
         pch = 1:2)
  


  