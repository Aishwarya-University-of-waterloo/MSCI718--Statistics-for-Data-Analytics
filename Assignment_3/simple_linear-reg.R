library(dplyr)
library(tidyr)
library(ggplot2)

#dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-reverse_mortgage.csv')
# LOOKS GOOD
dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-Debt-collection.csv')

#dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-1.csv')
#dataset<- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-2.csv')
#dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-city-bank.csv')


count(dataset)
dataset <- na.omit(dataset)
count(dataset)
dataset$Date.received
dataset<-dataset %>% separate(Date.received, c("Month","Day","Year"),remove=FALSE)

#dataset <-data_frame[order(dataset$Year, dataset$Month),]
dataset$Year<- as.integer(dataset$Year)
dataset$Year
dataset$Month <- as.integer(dataset$Month)

count(dataset %>% filter(Month==01 & Year==19))


all_years<-c(12,13,14,15,16,17,18,19,20)
#all_years<-c(2012,2013,2014,2015,2016,2017,2018,2019,2020)
all_months<-c(1,2,3,4,5,6,7,8,9,10,11,12)
trial1=NULL
trial2<-NULL
i<-0
for (y in all_years) {
  for (m in all_months) {
    
    s <- count(dataset %>% filter(Month==m & Year==y))
    if(s!=0){
      i <- i + 1
      
        trial1 <- rbind(trial1, data.frame(y,m,i, s))
    }
  }
}

trial1


i<-0
for (y in all_years) {
  
    
    s <- count(dataset %>% filter(Year==y))
    if(s!=0){
      i <- i + 1
      
      trial2 <- rbind(trial2, data.frame(y,i, s))
    }
  
}


#df2 <- dataset %>% group_by(Year,Month,Day) %>% tally()
#df2 <- dataset %>%  group_by(Year,Month) %>% tally()  #%>%filter(Year != 2021) 

#df2 <-data_frame[order(df2$Year, df2$Month),]

#df2
count(dataset)
sum(trial1$n)
sum(trial2$n)
#df3<-df2 %>% filter(Year!=2021)


#df3



#df3$Year  <- as.integer(df3$Year)
#df3$Month  <- as.integer(df3$Month)
#df3$new<- df3$Month+df3$Year*100
names(dataset)
str(trial1)

#, ,na.action=na.exclude
#na.action = na.omit or na.exclude

cor(trial1$n, trial1$y)
cor(trial2$n, trial2$y)



trial1
trial2
regressor = lm(formula = n ~ i,   data = trial1)
summary(regressor)
summary(regressor1)
regressor1 = lm(formula = n ~ y,   data = trial1)
summary(regressor1)

regressor2 = lm(formula = n ~ y,   data = trial2)
summary(regressor2)

#regressor3 = lm(formula = n ~ i,   data = trial)
#summary(regressor3)


e <- ggplot(trial1, aes(y=n, x=y))
f<-e + geom_point()

f+ geom_smooth(method = lm)

confint(regressor1)

library(ggiraphExtra)
ggPredict(regressor1, se=TRUE)

regressor1$coefficients

newValues=tibble(y=c(2021,2022))
predict.lm(regressor1, newValues)
trial2

library(car)
durbinWatsonTest(regressor)
durbinWatsonTest(regressor1)
durbinWatsonTest(regressor2)
# https://stats.stackexchange.com/questions/326305/i-ran-the-durbinwatsontest-in-r-and-got-p-value-0-for-non-time-series-data-is

plot(regressor2)


# assumption of homogenity of variance
std.residuals <- (regressor1$residuals-mean(regressor1$residuals))/sd(regressor1$residuals)
std.fitted <- (regressor1$fitted.values-mean(regressor1$fitted.values))/sd(regressor1$fitted.values)
plot(std.residuals,std.fitted)


##ERROR
fit<- lm(formula = n ~ y,   data = trial1)

library(boot)
bootReg <- function (formula, data, indices){
  d <- data [i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults<-boot(statistic = bootReg,formula = n ~ y ,data = trial1, R = 2000)



boot.ci(bootResults, type = "bca", index = 1)
boot.ci(bootResults, type = "bca", index = 2)

## error ends


## next try
df<-trial1

fit <- lm(formula = n ~ y,df)  
#install.packages("boot")
library(boot)
bs=function(formula,data,indices){
  d=data[indices,]
  fit=lm(formula,data=d)
  (coef(fit))
}  
results=boot(data=df,statistic=bs, R=2000,formula= n ~y)  
boot.ci(results,type="bca",index=1)  
boot.ci(results,type="bca",index=2)



library(ggfortify);
autoplot(regressor1);


#https://www.isixsigma.com/tools-templates/5s/checking-simple-linear-regression-analysis-using-5s/


#https://www.analyticsvidhya.com/blog/2020/12/predicting-using-linear-regression-in-r/
