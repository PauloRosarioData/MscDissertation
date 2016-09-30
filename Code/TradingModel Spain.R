#Econometrics Research Paper

library("vars")
library('ggplot2')
library("dynlm")
library("DataCombine")
library(stargazer)
library(dyn)

setwd("E:/Dissertation/Data/Spanish")

#Load the data series


Banks_quotes<- read.csv("rawdataspain.csv")




#For the trading simulation
Banks_quotes[is.na(Banks_quotes)] <- 0
#Initializing the variables and dataframes
sum_profit <- 100
sum_short <- 100
sum_long <- 100
profit <- 0
profit_average <- 0
trade_date <- 0
cumulative_profit <- data.frame()
cumulative_short <- data.frame()
cumulative_long <- data.frame()
average_profit <- data.frame()
period_profit <- data.frame()
trade_date_cumulative <- data.frame()

for(i in 50:3532)
{
  #slice the data into a train period and estimates the model
  train <- Banks_quotes[1:i,]
  fit <- lm(train$ES_var ~ train$Lag1_Var_BBVA + train$Lag2_Var_BBVA+
                               train$Lag1_Var_SAN + train$Lag2_Var_SAN +
                               train$Lag1_Var_POP + train$Lag2_Var_POP)

  #runs the model and computes the errors
  prediction <- summary(fit)$coefficients[1] + summary(fit)$coefficients[2] * Banks_quotes$Lag1_Var_BBVA[i+1] +
    summary(fit)$coefficients[3] *Banks_quotes$Lag2_Var_BBVA[i+1] +
    summary(fit)$coefficients[4] *Banks_quotes$Lag1_Var_SAN[i+1] +
    summary(fit)$coefficients[5] *Banks_quotes$Lag2_Var_SAN[i+1] +
    summary(fit)$coefficients[6] *Banks_quotes$Lag1_Var_POP[i+1] +
    summary(fit)$coefficients[7] *Banks_quotes$Lag2_Var_POP[i+1]


  actual <- Banks_quotes$IT_var[i + 1]
  long_spread <- (Banks_quotes$DE_price[i + 1]/Banks_quotes$DE_price[i]-1) - (Banks_quotes$ES_price[i + 1]/Banks_quotes$ES_price[i]-1)
  short_spread <- -long_spread
  profit <- ifelse(prediction > 0, long_spread, short_spread)
  sum_profit <- sum_profit * (1 + profit)
  sum_short <- sum_short * (1 + short_spread)
  sum_long <- sum_long * (1 + long_spread)
  profit_average <- sum_profit/ (i-1499)
  trade_date <- Banks_quotes$Numeric_date[i+1]

  #saves the results into a dataframe
  cumulative_profit <- rbind(cumulative_profit, sum_profit)
  cumulative_short <- rbind(cumulative_short, sum_short)
  cumulative_long <- rbind(cumulative_long, sum_long)
  average_profit <- rbind(average_profit, profit_average)
  period_profit <- rbind(period_profit, profit)
  trade_date_cumulative <- rbind(trade_date_cumulative , trade_date)
  print(i)
}


Chart_data <- data.frame()

Chart_data$Model_Profit <- cumulative_profit[1]


ts.plot(cumulative_profit)
ts.plot(cumulative_short)
ts.plot(cumulative_long)

output <- cbind(trade_date_cumulative, cumulative_profit, cumulative_short, cumulative_long)

write.csv(output, "chartsdataSpain.csv")

