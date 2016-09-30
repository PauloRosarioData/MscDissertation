#Econometrics Research Paper
#Implements a VAR model trying to undestand the relatio between
#portuguse sovereing yield and the evolution/perfomance of
#the Banking sector. The main portuguese banks are

#1.BKIA
#2.BKT
#3.BBVA
#IDEA: limit the sample to the date of BKT bankruptcy
library("vars")
library('ggplot2')
library("dynlm")
library("DataCombine")
library(stargazer)
library(dyn)

setwd("E:/Dissertation/Data/Spanish")
Banks_quotes<- read.csv("rawdataspain.csv")

#Decomposing the variations into positive and negative for one of the models
Banks_quotes$BBVA_Positive = ifelse(Banks_quotes$Var_BBVA > 0, 0, Banks_quotes$Var_BBVA)
Banks_quotes$BBVA_Negative = ifelse(Banks_quotes$Var_BBVA < 0, 0, Banks_quotes$Var_BBVA)

Banks_quotes$BKIA_Positive = ifelse(Banks_quotes$Var_BKIA > 0, 0, Banks_quotes$Var_BKIA)
Banks_quotes$BKIA_Negative = ifelse(Banks_quotes$Var_BKIA < 0, 0, Banks_quotes$Var_BKIA)

Banks_quotes$BKT_Positive = ifelse(Banks_quotes$Var_BKT > 0, 0, Banks_quotes$Var_BKT)
Banks_quotes$BKT_Negative = ifelse(Banks_quotes$Var_BKT < 0, 0, Banks_quotes$Var_BKT)

Banks_quotes$CABK_Positive = ifelse(Banks_quotes$Var_CABK > 0, 0, Banks_quotes$Var_CABK)
Banks_quotes$CABK_Negative = ifelse(Banks_quotes$Var_CABK < 0, 0, Banks_quotes$Var_CABK)

Banks_quotes$POP_Positive = ifelse(Banks_quotes$Var_POP > 0, 0, Banks_quotes$Var_POP)
Banks_quotes$POP_Negative = ifelse(Banks_quotes$Var_POP < 0, 0, Banks_quotes$Var_POP)

Banks_quotes$SAB_Positive = ifelse(Banks_quotes$Var_SAB > 0, 0, Banks_quotes$Var_SAB)
Banks_quotes$SAB_Negative = ifelse(Banks_quotes$Var_SAB < 0, 0, Banks_quotes$Var_SAB)

Banks_quotes$SAN_Positive = ifelse(Banks_quotes$Var_SAN > 0, 0, Banks_quotes$Var_SAN)
Banks_quotes$SAN_Negative = ifelse(Banks_quotes$Var_SAN < 0, 0, Banks_quotes$Var_SAN)


Banks_quotes <-Banks_quotes[order(Banks_quotes$Numeric_date),]
Banks_quotes <- Banks_quotes[2:3911,]
Banks_quotes <- Banks_quotes[complete.cases(Banks_quotes),]



#BaselineModel
BaseLineModel <- lm(Banks_quotes$ES_var ~ Banks_quotes$Var_BBVA +
   Banks_quotes$Var_BKIA + Banks_quotes$Var_BKT +
   Banks_quotes$Var_CABK + Banks_quotes$Var_POP +
   Banks_quotes$Var_SAB + Banks_quotes$Var_SAN)



summary(BaseLineModel)

stargazer(BaseLineModel, title="Results", align=TRUE)

cov <- vcovHC(BaseLineModel, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(BaseLineModel, BaseLineModel, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)

#Model with different coefficients for positive and negative variations
Different_coefficients <- lm(Banks_quotes$ES_var ~ Banks_quotes$BBVA_Negative + Banks_quotes$BBVA_Positive + Banks_quotes$BKIA_Negative +
                     Banks_quotes$BKIA_Positive + 
                     Banks_quotes$BKT_Negative + Banks_quotes$BKT_Positive +
                       Banks_quotes$CABK_Negative + Banks_quotes$CABK_Positive +
                       Banks_quotes$POP_Negative + Banks_quotes$POP_Positive +
                       Banks_quotes$SAB_Negative + Banks_quotes$SAB_Positive +
                       Banks_quotes$SAN_Negative + Banks_quotes$SAN_Positive)

summary(Different_coefficients)

stargazer(Different_coefficients, title="Results", align=TRUE)

cov <- vcovHC(Different_coefficients, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Different_coefficients, Different_coefficients, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)



#ModelWithLags
Model_with_lags <- lm(Banks_quotes$ES_var ~ Banks_quotes$Lag1_Var_BBVA + Banks_quotes$Lag2_Var_BBVA+
                        Banks_quotes$Lag1_Var_BKIA + Banks_quotes$Lag2_Var_BKIA +
                        Banks_quotes$Lag1_Var_BKT + Banks_quotes$Lag2_Var_BKT +
                        Banks_quotes$Lag1_Var_CABK + Banks_quotes$Lag2_Var_CABK +
                        Banks_quotes$Lag1_Var_POP + Banks_quotes$Lag2_Var_POP +
                        Banks_quotes$Lag1_Var_SAB + Banks_quotes$Lag2_Var_SAB +
                        Banks_quotes$Lag1_Var_SAN + Banks_quotes$Lag2_Var_SAN )


cov <- vcovHC(Model_with_lags, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Model_with_lags, Model_with_lags, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)
