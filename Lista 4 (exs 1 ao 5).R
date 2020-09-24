# install.packages("olsrr")
# install.packages("tidyverse")  3 pacotes para rodar mais facil os métodos de selecão de variáveis
# install.packages("leaps")

library(olsrr)
library(tidyverse)
library(leaps)
library(MPV)


# EX 1 


# forward regression
model_fw <- lm(y ~ ., data = table.b2)
ols_step_forward_aic(model_fw, details = T)

# backward regression
model_bw <- lm(y ~ ., data = table.b2)
ols_step_backward_aic(model_bw, details = T )

# stepwise aic regression
model_step <- lm(y ~ ., data = table.b2)
ols_step_both_aic(model_step, details = T)




# Ex 2 

# forward regression
model_fw <- lm(y ~ ., data = table.b3)
ols_step_forward_aic(model_fw, details = T)

# backward regression
model_bw <- lm(y ~ ., data = table.b3)
ols_step_backward_aic(model_bw, details = T )

# stepwise aic regression
model_step <- lm(y ~ ., data = table.b3)
ols_step_both_aic(model_step, details = T) 



# EX 3


# forward regression
model_fw <- lm(y ~ ., data = table.b4)
ols_step_forward_aic(model_fw) 

# backward regression
model_bw <- lm(y ~ ., data = table.b4)
ols_step_backward_aic(model_bw)

# stepwise aic regression
model_step <- lm(y ~ ., data = table.b4)
ols_step_both_aic(model_step)




# Ex 4

# Table.b15 - Air Pollution and Mortality Data

MORT<-c(790.73, 823.76, 839.71, 844.05, 857.62, 860.1, 861.44, 861.83, 871.34, 871.77, 874.28, 887.47, 891.71, 893.99, 895.7
        , 899.26, 899.53, 904.16, 911.7, 911.82, 912.2, 912.35, 919.73, 921.87, 923.23, 929.15, 934.7, 936.23, 938.5, 941.18, 
        946.18, 950.67, 952.53, 953.56, 954.44, 958.84, 959.22, 961.01, 962.35, 967.8, 968.66, 970.47, 971.12, 972.46, 982.29, 
        985.95, 989.27, 991.29, 994.65, 997.88, 1001.9, 1003.5, 1006.49,
        1015.02, 1017.61, 1024.89, 1025.5, 1030.38, 1071.29, 1113.06)
PRECIP<-c(13, 28, 10, 43, 25, 35, 60, 11, 31, 15, 32, 43, 31, 37, 45, 35, 45, 45, 18, 42, 40, 36, 35, 36, 46, 30, 43, 36, 42, 
          30, 41, 38, 46, 34, 38, 37, 31, 45, 44, 41, 39, 40, 42, 31, 47, 35, 30, 36, 42, 35, 36, 45, 50, 42, 52, 33, 44, 53, 
          43, 54)

EDUC<-c(12.2, 12.1, 12.1, 9.5, 12.10, 11.8, 11.5, 12.1, 10.9, 12.2, 11.1, 11.5, 11.4, 12, 11.1, 12.2, 10.6, 11.1, 12.2, 
        9, 10.3,10.7, 12, 11.4, 11.3, 11.1, 12.1, 11.4, 10.1, 10.8, 9.6, 11.4, 11.4, 9.70, 10.7, 11.9, 10.8, 10.1, 9.8,
        12.3, 11.4, 10.2, 10.4, 10.7, 11.1, 11.10, 9.9, 10.6, 10.7, 11, 10.5, 11.3, 10.4, 10.5, 9.6, 10.9, 11, 10.2, 9.6, 9.7)

NONWHITE<-c(3,7.5, 5.9, 2.9, 3, 14.8, 11.5, 7.8, 5.1, 4.7, 5, 7.2, 11.5, 3.6, 1, 5.7, 5.3, 3.4, 13.7, 4.8, 2.5, 6.7, 12.6, 8.8,
            8.8, 5.8, 3.5, 12.4, 2.2, 13.1, 2.7, 3.8, 21, 17.2, 11.7, 13.1, 15.8, 21, 0.8, 25.9, 15.6, 13, 22.7, 9.5, 27.1, 14.7, 
            13.1, 8.1, 11.3, 3.5, 8.1, 12.1, 36.7, 17.5, 22.2, 16.3, 28.6, 38.5, 24.4, 31.4)

NOX<-c(32, 2, 66, 7, 11, 1, 1, 319, 3, 8, 4, 3, 1, 21, 3, 7, 4, 4, 171, 8, 2, 7, 4, 15, 3, 23, 32, 4, 4, 4, 11, 5, 5, 15, 13, 
       9, 35, 14, 6, 28, 7, 26, 3, 7, 8, 21, 37, 59, 26, 10, 12, 11, 18, 32, 8, 63, 9, 32, 38, 17)

SO2<-c(3, 1, 20, 32, 26, 1, 1, 130, 10, 28, 18, 10, 1, 44, 8, 20, 4, 20, 86, 49, 11, 20, 4, 59, 8, 125, 62, 16, 18, 11, 89, 
       25, 1, 68, 39, 15, 124, 78, 33, 102, 33, 146, 5, 25, 24, 64, 193, 263, 108, 39, 37, 42, 34, 161, 27, 278, 48, 
       72, 206, 1)

table.b15 <- data.frame(MORT,PRECIP, EDUC, NONWHITE, NOX, SO2)


## a) 

model_apr = lm(MORT ~ PRECIP + EDUC + NONWHITE + SO2, data = table.b15)
ols_step_all_possible(model_apr)
ols_step_best_subset(model_apr) # resumo do 'best subset model'


## b) 

# forward regression
model_fw <- lm(MORT ~ ., data = table.b15)
ols_step_forward_aic(model_fw, details =  T) 

# backward regression
model_bw <- lm(MORT ~ ., data = table.b15)
ols_step_backward_aic(model_bw, details =  T)

# stepwise aic regression
model_step <- lm(MORT ~ ., data = table.b15)
ols_step_both_aic(model_step, details =  T)


## c) 

# forward regression
model_fw <- lm(MORT ~ ., data = table.b15)
ols_step_forward_p(model_fw, details =  T) 

# backward regression
model_bw <- lm(MORT ~ ., data = table.b15)
ols_step_backward_p(model_bw, details =  T)

# stepwise regression
model_step <- lm(MORT ~ ., data = table.b15)
ols_step_both_p(model_step, details =  T)



# EX 5

attach(cement) 
cement
model_apr = lm(y ~ . , data = cement)
ols_step_all_possible(model_apr)
ols_step_best_subset(model_apr)


