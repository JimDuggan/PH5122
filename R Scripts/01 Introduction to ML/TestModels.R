library(dplyr)
library(ggplot2)

generate_data <- function(data_x,
                          beta_0 = 2.0,
                          beta_1 = 0.5,
                          beta_2 = -0.02,
                          sd=0.2,
                          seed=100){
  noise <- rnorm(length(data_x),0,sd)
  data_y <- beta_0 +
              beta_1 * data_x +
              beta_2 * data_x^2 +
              noise
  
  data_y
}

set.seed(100)
NUM_POINTS <- 200
data_x <- runif(NUM_POINTS,0,10)

# Experiment 1
data_y <- generate_data(data_x,
                        beta_0 = 2,
                        beta_1 = 0.5,
                        beta_2 = 0,
                        sd = 0.2)

df1 <- tibble(X=data_x,Y=data_y)

ggplot(df1,aes(X,Y))+geom_point()+geom_smooth(method="lm")

m1 <- lm(Y~X, data=df1)

ggplot(df1,aes(X,Y))+geom_point()+
  geom_abline(slope=coef(m1)[2],intercept = coef(m1)[1],colour="red")

df1$Y_PRED <- predict(m1)


# Plot predictions v actuals
ggplot(df1,aes(Y_PRED,Y))+geom_point()+geom_abline(slope=1,intercept = 0,colour="red")

# Experiment 2
data_y <- generate_data(data_x,
                        beta_0 = 2,
                        beta_1 = 0.5,
                        beta_2 = -0.02,
                        sd = 0.2)

df2 <- tibble(X=data_x,Y=data_y)

df2$X_SQ <- df2$X^2

m2_1 <- lm(Y~X+X_SQ,data=df2)
m2_2 <- lm(Y~X+I(X*X),data=df2)








