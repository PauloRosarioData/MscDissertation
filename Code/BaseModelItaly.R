#Econometrics Research Paper
#Implements a VAR model trying to undestand the relatio between
#portuguse sovereing yield and the evolution/perfomance of
#the Banking sector. The main portuguese banks are

#1.BPE
#2.BP
#3.BMPS
#IDEA: limit the sample to the date of BP bankruptcy
library("vars")
library('ggplot2')
library("dynlm")
library("DataCombine")
library(stargazer)
library(dyn)

setwd("E:/Dissertation/Data/Italian")
#Load the data series


Banks_quotes<- read.csv("rawdata.csv")



#Decomposing the variations into positive and negative for one of the models
Banks_quotes$BMPS_Positive = ifelse(Banks_quotes$Var_BMPS > 0, 0, Banks_quotes$Var_BMPS)
Banks_quotes$BMPS_Negative = ifelse(Banks_quotes$Var_BMPS < 0, 0, Banks_quotes$Var_BMPS)

Banks_quotes$BPE_Positive = ifelse(Banks_quotes$Var_BPE > 0, 0, Banks_quotes$Var_BPE)
Banks_quotes$BPE_Negative = ifelse(Banks_quotes$Var_BPE < 0, 0, Banks_quotes$Var_BPE)

Banks_quotes$BP_Positive = ifelse(Banks_quotes$Var_BP > 0, 0, Banks_quotes$Var_BP)
Banks_quotes$BP_Negative = ifelse(Banks_quotes$Var_BP < 0, 0, Banks_quotes$Var_BP)


Banks_quotes$FBK_Positive = ifelse(Banks_quotes$Var_FBK > 0, 0, Banks_quotes$Var_FBK)
Banks_quotes$FBK_Negative = ifelse(Banks_quotes$Var_FBK < 0, 0, Banks_quotes$Var_FBK)

Banks_quotes$ISP_Positive = ifelse(Banks_quotes$Var_ISP > 0, 0, Banks_quotes$Var_ISP)
Banks_quotes$ISP_Negative = ifelse(Banks_quotes$Var_ISP < 0, 0, Banks_quotes$Var_ISP)

Banks_quotes$MB_Positive = ifelse(Banks_quotes$Var_MB > 0, 0, Banks_quotes$Var_MB)
Banks_quotes$MB_Negative = ifelse(Banks_quotes$Var_MB < 0, 0, Banks_quotes$Var_MB)

Banks_quotes$PMI_Positive = ifelse(Banks_quotes$Var_PMI > 0, 0, Banks_quotes$Var_PMI)
Banks_quotes$PMI_Negative = ifelse(Banks_quotes$Var_PMI < 0, 0, Banks_quotes$Var_PMI)

Banks_quotes$UBI_Positive = ifelse(Banks_quotes$Var_UBI > 0, 0, Banks_quotes$Var_UBI)
Banks_quotes$UBI_Negative = ifelse(Banks_quotes$Var_UBI < 0, 0, Banks_quotes$Var_UBI)

Banks_quotes$UCG_Positive = ifelse(Banks_quotes$Var_UCG > 0, 0, Banks_quotes$Var_UCG)
Banks_quotes$UCG_Negative = ifelse(Banks_quotes$Var_UCG < 0, 0, Banks_quotes$Var_UCG)


Banks_quotes <-Banks_quotes[order(Banks_quotes$Numeric_date),]
Banks_quotes <- Banks_quotes[2:3911,]
Banks_quotes <- Banks_quotes[complete.cases(Banks_quotes),]



#BaselineModel
BaseLineModel <- lm(Banks_quotes$IT_var ~ Banks_quotes$Var_ISP +
   Banks_quotes$Var_BMPS + Banks_quotes$Var_UCG +
   Banks_quotes$Var_BPE + Banks_quotes$Var_MB +
   Banks_quotes$Var_PMI + Banks_quotes$Var_BP +
   Banks_quotes$Var_UBI)



summary(BaseLineModel)

stargazer(BaseLineModel, title="Results", align=TRUE)

cov <- vcovHC(BaseLineModel, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(BaseLineModel, BaseLineModel, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)

#Model with different coefficients for positive and negative variations
Different_coefficients <- lm(Banks_quotes$IT_var ~ Banks_quotes$BMPS_Negative + Banks_quotes$BMPS_Positive + Banks_quotes$BPE_Negative +
                     Banks_quotes$BPE_Positive + 
                       Banks_quotes$BP_Negative + Banks_quotes$BP_Positive +
                       Banks_quotes$FBK_Negative + Banks_quotes$FBK_Positive +
                       Banks_quotes$ISP_Negative + Banks_quotes$ISP_Positive +
                       Banks_quotes$MB_Negative + Banks_quotes$MB_Positive +
                       Banks_quotes$PMI_Negative + Banks_quotes$PMI_Positive +
                       Banks_quotes$UBI_Negative + Banks_quotes$UBI_Positive +
                       Banks_quotes$UCG_Negative + Banks_quotes$UCG_Positive)

summary(Different_coefficients)

stargazer(Different_coefficients, title="Results", align=TRUE)

cov <- vcovHC(Different_coefficients, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Different_coefficients, Different_coefficients, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)



#ModelWithLags
Model_with_lags <- lm(Banks_quotes$IT_var ~ Banks_quotes$Lag1_Var_ISP + Banks_quotes$Lag2_Var_ISP+
                        Banks_quotes$Lag1_Var_BMPS + Banks_quotes$Lag2_Var_BMPS +
                        Banks_quotes$Lag1_Var_BPE + Banks_quotes$Lag2_Var_BPE +
                        Banks_quotes$Lag1_Var_MB + Banks_quotes$Lag2_Var_MB +
                        Banks_quotes$Lag1_Var_PMI + Banks_quotes$Lag2_Var_PMI +
                        Banks_quotes$Lag1_Var_BP + Banks_quotes$Lag2_Var_BP +
                        Banks_quotes$Lag1_Var_UBI + Banks_quotes$Lag2_Var_UBI)


cov <- vcovHC(Model_with_lags, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Model_with_lags, Model_with_lags, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
