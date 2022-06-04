getwd()

#SIMPLE RANDOM SAMPLING

library(DandEFA)
library(readxl)
churnmodelling <- read_excel("C:/Users/tugce/churnmodelling.xls")
View(churnmodelling)
data<-(churnmodelling)
mean(data[["Age"]])#population mean 38.9218
var(data[["Age"]])#population variance 109.9941
data_age <- c(data[["Age"]])
x_bar <- mean(data_age) ## 38.9218
x_bar
s = sqrt(sum((data_age - x_bar)^2)/(length(data_age)-1)) ## 10.48781
s
## n/N
30/10000 ## 0.03 < 0.05 not use finite correction factor
sigma_x = s/sqrt(30) #3.793757
sigma_x
## confidence interval, since n=30 z table should be used, 95%
x_bar - qnorm(1-0.05/2)*sigma_x #27.56437
x_bar + qnorm(1-0.05/2)*sigma_x #42.43563
##SCATTER PLOT
plot(Exited~Age,data)
hist(data_age,col="red")


#Stratified Sampling
install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
## DIVIDE THE DATASET
age_over50 <- data_age[data_age >50]
age_less50 <- data_age[data_age <=50]
age_over50
age_less50
## CALCULATE SAMPLE MEANS
x1_bar <- mean(age_over50)#59.57573
x1_bar
x2_bar <- mean(age_less50)#35.94153
x2_bar
## CALCULATE SAMPLE VARIANCE FOR 1ST STRATA
sigma_1_sq <- sum((age_over50 - x1_bar)^2)/(length(age_over50)-1)
sigma_1_sq #53.727
length(age_over50)#1261
length(age_less50)#8739
## n1/N1
10/1261#0.007930214<0.05 not use finite correction factor
0.007930214>0.05#FALSE
## CALCULATE STANDARD ERROR FOR 1ST STRATA
sigma_1_xbar_sq = sigma_1_sq/length(age_over50)
sigma_1_xbar_sq #0.4260666
## CALCULATE SAMPLE VARIANCE FOR 2ND STRATA
sigma_2_sq <- sum((age_less50 - x2_bar)^2)/(length(age_less50)-1)
sigma_2_sq #47.6758
## n2/N2
20/8739 #0.002288591
0.002288591>0.05#FALSE
## CALCULATE STANDARD ERROR FOR 2ND STRATA
sigma_2_xbar_sq = sigma_2_sq/length(age_less50)
sigma_2_xbar_sq#0.005455521
## MERGE TWO STRATUM
x_bar_st = (1261*x1_bar + 8739*x2_bar)/10000
x_bar_st#38.9218
sigma_bar_x_st = (1/10000)* sqrt(1261^2*sigma_1_xbar_sq + 8739^2*sigma_2_xbar_sq)
sigma_bar_x_st#0.06959803
## CONFIDENCE INTERVAL
x_bar_st - qnorm(1-0.05/2,mean = 0,sd = 1 )*sigma_bar_x_st#38.78539
x_bar_st + qnorm(1-0.05/2,mean = 0,sd = 1 )*sigma_bar_x_st#39.05821



#Cluster Sampling 
library(dplyr)
data<-(churnmodelling)
mean(data_age)#population mean #38.9218
var(data_age)#population variance 109.9941
# CREATE A DATAFRAME
customer_id <- 1:10000
cluster <- rep(1:1000, each = 10)
df<-data.frame(cluster,customer_id,data_age)
df
# DEFINE N AND M
N = 10  # number of elements in each cluster 
M = 9   #  number of clusters
# CALCULATE MEAN AND VARIANCE FOR ALL DATA AND FOR CLUSTERS n=30
df %>% summarise(
  mean_age = mean(data_age),
  var_age = var(data_age)
) 
df
summary_df <- df %>% 
  group_by(cluster) %>% 
  summarise(
    mean_age = mean(data_age),
    var_age = var(data_age)
  ) %>% filter(cluster %in% c(6,5,7))
summary_df
# ESTIMATE FOR MEAN
grand_mean <- mean(summary_df$mean_age)
grand_mean # 40.03333
# NUMBER OF SELECTED CLUSTER IS 3, TOTAL NUMBER OF CLUSTERS IS 9
m=3
M=9
# m/M 
3/9 # 0.3333333 > 0.05 finite correction term should be used
sigma_sq <- (1/m) * (sum((summary_df$mean_age - grand_mean)^2)/(m-1))* ((M-m)/M)
sigma_sq # 5.045185
grand_mean - qnorm(1-0.05/2)*sqrt(sigma_sq) # 35.63096
grand_mean + qnorm(1-0.05/2)*sqrt(sigma_sq) # 44.4357
# mean age was calculated at 95% confidence level in the 3 sampling types. 
#The average age of customers in the data set was 38.9218.  
#Simple random sampling result is between 27.56437 and 42.43563 at the 95% confidence level. 
#Stratified random sampling result is between  38.78539 and 39.05821 at the 95% confidence level.
#Cluster sampling between result is 35,63096 and 44,4357 at the 95% confidence level. 
#The best interval is the narrowest one. We can say that the interval in the Stratified Sampling method gives the best results
