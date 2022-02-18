#data source <https://www.consumerfinance.gov/data-research/consumer-complaints/search/?dataNormalization=None&product=Money%20transfers&searchField=all&tab=Map>


# http://www.sthda.com/english/articles/40-regression-analysis/163-regression-with-categorical-variables-dummy-coding-essentials-in-r/

# https://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/



## my data https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/?field=all&format=csv&lens=overview&no_aggs=true&product=Money%20transfer%2C%20virtual%20currency%2C%20or%20money%20service%E2%80%A2Domestic%20%28US%29%20money%20transfer&product=Money%20transfer%2C%20virtual%20currency%2C%20or%20money%20service%E2%80%A2International%20money%20transfer&size=10608&trend_depth=5&trend_interval=year

# international money transfer    https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/?field=all&format=csv&lens=overview&no_aggs=true&product=Money%20transfer%2C%20virtual%20currency%2C%20or%20money%20service%E2%80%A2International%20money%20transfer&size=3759&trend_depth=5&trend_interval=year

# mortgage  https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/?field=all&format=csv&lens=overview&no_aggs=true&product=Mortgage%E2%80%A2Reverse%20mortgage&size=3391&trend_depth=5&trend_interval=year


#library(skimr)
#
#library(pastecs)
#library(psych) 
#library(grid)
#library(gridExtra)
#library(GGally)



library(dplyr)
library(tidyr)
library(ggplot2)

dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-reverse_mortgage.csv')
#dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-Debt-collection.csv')


count(dataset)
dataset <- na.omit(dataset)
count(dataset)
dataset<-dataset %>% separate(Date.received, c("Month","Day","Year"))

#label encoding
dataset$Issue<- factor(dataset$Issue,
                       levels=c('Application, originator, mortgage broker',
                                'Applying for a mortgage or refinancing an existing mortgage',
                                'Closing on a mortgage',
                                'Credit decision / Underwriting',
                               # 'Credit monitoring or identity theft protection services', #strange
                               #Improper use of your report
                                'Incorrect information on your report',
                                'Loan modification,collection,foreclosure',
                                'Loan servicing, payments, escrow account',
                                'Other',
                                'Problem with a credit reporting company\'s investigation into an existing problem',
                                'Settlement process and costs',
                                'Struggling to pay mortgage',
                                'Trouble during payment process'),
                       labels=c(1,2,3,4,5,6,7,8,9,10,11,12))


dataset$Company.response.to.consumer<- factor(dataset$Company.response.to.consumer,
                                              levels=c('Closed',
                                                       'Closed with explanation',
                                                       'Closed with monetary relief',
                                                       'Closed with non-monetary relief',
                                                       'Closed without relief',
                                                       'In progress',
                                                       'Untimely response'),
                                              labels=c(1,2,3,4,5,6,7)
                                              )




dataset$Company.public.response<- factor (dataset$Company.public.response,
                                          levels=c('Company believes complaint is the result of an isolated error',
                                                   'Company believes complaint represents an opportunity for improvement to better serve consumers',
                                                   'Company believes it acted appropriately as authorized by contract or law',
                                                   'Company believes the complaint is the result of a misunderstanding',
                                                   'Company chooses not to provide a public response',
                                                   'Company disputes the facts presented in the complaint',
                                                   'Company has responded to the consumer and the CFPB and chooses not to provide a public response',
                                                   'None'),
                                          labels=c(1,2,3,4,5,6,7,8))
count(dataset)
#dataset<-dataset %>%filter(dataset$State=="CA"| dataset$State=="FL"|dataset$State=="TX"|dataset$State=="NY"|dataset$State=="NJ")

count(dataset)


#product,subproduct,subissue   are highly correlated so dropping them


length(dataset)
#dataset$Product <- NULL
#dataset$Sub.product <- NULL
#dataset$Sub.issue <- NULL

#since timely response gives the same information related to date sent to company and status of complain we can drop the date
#dataset$Date.sent.to.company<- NULL

dataset$Consumer.complaint.narrative<-NULL
#high cardiality
#dataset$Company<-NULL
#dataset$Complaint.ID <- NULL
#dataset$State<-NULL
#dataset$Month<-NULL
#dataset$Day<-NULL
#dataset$ZIP.code<-NULL

#submitted via is highly correlated to consent_provided
#dataset$Consumer.consent.provided.<-NULL
  
  
  
#dataset$State[!(dataset$State=="CA"|| dataset$State=="FL"||dataset$State=="TX"||dataset$State=="NY"||dataset$State=="NJ")] <- "Other"

#str(dataset)






count(dataset)
df2 <- dataset %>% group_by(Year) %>% tally()
sum(df2$n)

for (row in 1:nrow(dataset)) {
  dataset[row,'total_complains'] <- df2$n[df2$Year==dataset[row,'Year']]
}

dataset$Consumer.disputed.
dataset$Consumer.disputed.[dataset$Consumer.disputed.=="N/A"] <- "Other"
dataset$Consumer.disputed.


dataset[c('Year','total_complains')]


#dataset.ordered <- dataset[order(dataset$Day, dataset$Month, dataset$Year),]

#dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\i-3\\complaints-reverse_mortgage.csv')

#dataset<-dataset %>% mutate(dataset, Date.received= as.Date(Date.received, format =  "%m/%d/%Y"))#
#dataset.ordered$Date.received



#dataset.ordered <- dataset[order(dataset$Date.received),]



dataset$Year<-NULL
names(dataset)
regressor = lm(formula = total_complains ~ Sub.product,   data = dataset,na.action=na.exclude)#, 
#na.action = na.omit or na.exclude

summary(regressor)


plot(regressor)

names(dataset)
regressor = lm(formula = total_complains ~ Day+Month,   data = dataset,na.action=na.exclude)
summary(regressor)





#df2 <- dataset %>% group_by(Year,Month,Day) %>% tally()
df2 <- dataset %>% group_by(Year) %>% tally()
df2
df3<-df2 %>% filter(Year!=2021)

str(df3)
df3
df3$Year  <- as.integer(df3$Year)


regressor = lm(formula = n ~ Year,   data = df3,na.action=na.exclude)#, 
#na.action = na.omit or na.exclude

summary(regressor)






e <- ggplot(df2, aes(Year, n))
f<-e + geom_point()

f+ geom_smooth(method = lm)



#install.packages("QuantPsyc")
library(boot) 
library(car) 
library(QuantPsyc)


#DATA REQUIRED FOR PLOTS

album2$residuals<-resid(albumSales.3)
album2$standardized.residuals<- rstandard(albumSales.3)
album2$studentized.residuals<-rstudent(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta<-dfbeta(albumSales.3)
album2$dffit<-dffits(albumSales.3)
album2$leverage<-hatvalues(albumSales.3)
album2$covariance.ratios<-covratio(albumSales.3)

album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2


sum(album2$large.residual) 

album2[album2$large.residual,c("sales", "airplay", "attract", "adverts","standardized.residuals")]
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]

album2$fitted <- albumSales.3$fitted.values

# PLOTS TO BE INCLUDED

histogram<-ggplot(album2, aes(studentized.residuals)) + 
  opts(legend.position ="none") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "Studentized Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)), colour 
                          = "red", size = 1)

qqplot.resid <- qplot(sample = album2$studentized.residuals, stat="qq") + 
  labs(x ="Theoretical Values", y = "Observed Values") 


scatter <- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Blue")+ 
labs(x = "Fitted Values", y = "Studentized Residual")






myModel1<- lm(total_complains ~ Month, data = dataset)
myModel2<- lm(total_complains ~ Day, data = dataset)
myModel3<- lm(total_complains ~ Year, data = dataset)


myModel4<- lm(total_complains ~ Issue, data = dataset)
myModel5<- lm(total_complains ~ Company.public.response, data = dataset)
myModel6<- lm(total_complains ~ Company, data = dataset)
myModel7<- lm(total_complains ~ State, data = dataset)

summary(myModel1)  #not 1
summary(myModel2)#not 1

summary(myModel3) #1
summary(myModel4)
summary(myModel5)
summary(myModel6)
summary(myModel7)



#"ZIP.code"           "Tags"                         "Submitted.via"  "Company.response.to.consumer" "Timely.response."        "Consumer.disputed."           "Complaint.ID"                 "total_complains" 

#regressor = lm(formula = total_complains ~ Year,   data = dataset)


#summary(regressor)


bootReg <- function (formula, data, indices)
{
  d <- data [i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#bootResults<-boot(statistic = bootReg, formula = sales ~ adverts + airplay +attract, data = album2, R = 2000)





#https://towardsdatascience.com/problems-with-multiple-linear-regression-in-r-bef5940518b

#https://www.guru99.com/r-simple-multiple-linear-regression.html

#https://datatofish.com/multiple-linear-regression-in-r/




Look at the graph of the standardized residuals plotted against the fitted (predicted) values. 
If it looks like a random array of dots then this is good. If the dots seem to get more or less 
spread out over the graph (look like a funnel) then this is probably a violation of the assumption 
of homogeneity of variance. If the dots have a pattern to them (i.e., a curved shape) then this 
is probably a violation of the assumption of linearity. If the dots seem to have a pattern and 
are more spread out at some points on the plot than others then this probably reflects violations 
of both homogeneity of variance and linearity. Any of these scenarios puts the validity of your 
model into question. Repeat the above for all partial plots too. 

. Look at a histogram of the residuals too. If the histogram looks like a normal distribution 
(and the Q-Q plot looks like a diagonal line), then all is well. If the histogram looks non-normal, 
then things are less good. Be warned, though: distributions can 
look very non-normal in small samples even when they are normal! 
