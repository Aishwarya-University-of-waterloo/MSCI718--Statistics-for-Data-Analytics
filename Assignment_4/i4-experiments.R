library(dplyr);
library(tidyr);
library(ggplot2);
library(boot); 
library(car); 
library(QuantPsyc);

dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\I4\\melb_data.csv')
count(dataset)
count(dataset)

dataset<-na.omit(dataset)
count(dataset)

#dataset<- dataset %>% filter(Rooms>=3 & Rooms<=5)
count(dataset)


#dataset<- dataset %>%  filter(Distance<=20)
count(dataset)

#dataset<- dataset %>%  filter(Bathroom<=3)
count(dataset)


#dataset<- dataset %>%  filter(Car<=3)
count(dataset)



count(dataset)
dataset <- dataset %>%  filter (Suburb=='Reservoir'|Suburb=='Richmond'| Suburb=='Preston'|Suburb=='Brunswick'|Suburb=='Bentleigh East')

	 
count(dataset)

count(dataset)

#y <- lapply(strsplit(dataset$Address, "(?<=\\d)\\b ", perl=T), function(x) if (length(x)<2) c("", x) else x)
#y <- do.call(rbind, y)
#colnames(y) <- c("StreetNumber", "StreetName")

dataset$Address<- NULL

#dataset$StreetNumber <- y[,1]
#dataset$StreetName <- y[,2]

#dataset$age_of_building<-dataset$YearBuilt - dataset$Year_sold



#dataset <- na.omit(dataset)# ZERO CARS IS STILL VALID SO NO USE OF THIS
count(dataset)


#dataset$Date<- as.Date(dataset$Date, format =  "%d/%m/%Y")
dataset<-dataset %>% separate(Date, c("Day_sold","Month_sold","Year_sold"),remove=FALSE)

dataset$Year_sold<- as.integer(dataset$Year_sold)
dataset$Month_sold<- as.integer(dataset$Month_sold)
dataset$Day_sold<- as.integer(dataset$Day_sold)
dataset$Day_sold<-NULL
#write.csv(dataset,"MEL.csv", row.names = FALSE)



summary(dataset)


describe(dataset)



count(dataset)


length(dataset$Car)



dataset_new<- dataset %>% select_if(is.numeric)

library(corrplot)
res <- cor(dataset_new)
round(res, 2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

dataset$Bedroom2<-NULL #high correlation                 

dataset$Type[dataset$Type=='h']<-'House'
unique(dataset$Type)
dataset$Type[dataset$Type=='t']<-'NonHouse'
dataset$Type[dataset$Type=='u']<-'NonHouse'
unique(dataset$Type)
names (dataset)


#dataset$Month_sold<-NULL

#dataset$YearBuilt<- NULL#18.6%
#dataset$BuildingArea <- NULL #45.1%
count(dataset)
#dataset<- dataset %>%  filter(SellerG=='Nelson'|SellerG=='Jellis'|SellerG=='hockingstuart'|SellerG=='Barry'|SellerG=='Ray')
count(dataset)
names(dataset)

dataset$StreetNumber <-NULL #high correlation
dataset$Day_sold<-NULL


library(ggfortify)
#+Distance+CouncilArea+  Suburb++StreetName  Lattitude++Distance+Regionname+Suburb
#model.1<-lm(formula = Price ~ Longtitude+Distance+Year_sold+Method+CouncilArea,   data = dataset,na.action=na.exclude)#Postcode

names(dataset)
#+Landsize+Longtitude

dataset$Price<- log(dataset$Price)

dataset$Postcode<- log(dataset$Postcode)
dataset$Rooms <- log(dataset$Rooms)
dataset$Bathroom <- log(dataset$Bathroom)
dataset$Distance <- log(dataset$Distance+1)
dataset$BuildingArea <- log(dataset$BuildingArea)



names(dataset)
model.2<-lm(formula = Price ~ BuildingArea+Postcode+Rooms+Bathroom+Distance+Type,   data = dataset,na.action=na.exclude)#Postcode
summary(model.2)
#plot(model.1)+
#+Landsize+Longtitude



dataset$Price<- log(dataset$Price)
dataset$Postcode<- log(dataset$Postcode)
dataset$Rooms <- log(dataset$Rooms)
dataset$Bathroom <- log(dataset$Bathroom)
dataset$Distance <- log(dataset$Distance+1)
dataset$Year_sold <- log(dataset$Year_sold)
dataset$YearBuilt <- log(dataset$YearBuilt)







summary(dataset$Lattitude)
#model.2 = lm(formula = Price ~Rooms+Type +Bathroom+Landsize+BuildingArea+Car+Method, data = dataset,na.action=na.exclude)#, YearBuilt, Method, SellerG,Month_sold,Year_sold

c(1,2,3,6)



dataset$Postcode<- log(dataset$Postcode)
dataset$Rooms <- log(dataset$Rooms)
dataset$Bathroom <- log(dataset$Bathroom)
dataset$Distance <- log(dataset$Distance+1)
dataset$BuildingArea <- log(dataset$BuildingArea)
model.1<-lm(formula = Price ~ BuildingArea+Postcode+Rooms+Bathroom+Distance,   data = dataset,na.action=na.exclude)
summary(model.1)




autoplot(model.1, which = 1:6, label.size = 1, data = dataset, colour = 'Type')


autoplot(model.2, which = 1:6, label.size = 1, data = dataset, colour = 'Type')

#model.3<-lm(formula = Price ~.,   data = dataset,na.action=na.exclude)#Car, Propertycount

#model.4<-lm(formula = Price ~.,   data = dataset,na.action=na.exclude)


#model.4= lm(formula = Price ~ Rooms+Bathroom+Landsize+Distance +Car+Lattitude+Longtitude+Propertycount , data = dataset,na.action=na.exclude)  
lm.beta(model.1)  #page 318
confint(model.1) #page 319
anova(model.1, model.2) #page 321


dataset$residuals<-resid(model.1)  #page 323
dataset$standardized.residuals<- rstandard(model.1) #page 324
dataset$studentized.residuals<-rstudent(model.1)
dataset$cooks.distance<-cooks.distance(model.1)
dataset$dfbeta<-dfbeta(model.1)
dataset$dffit<-dffits(model.1)
dataset$leverage<-hatvalues(model.1)
dataset$covariance.ratios<-covratio(model.1)


e <- ggplot(dataset, aes(y=standardized.residuals, x=leverage))
e+geom_point()
count(dataset)

summary(dataset$cooks.distance)

plot(model.1)
durbinWatsonTest(model.2)
durbinWatsonTest(model.1)
vif(model.1)
mean(vif(model.1))

# For our current model the VIF values are all well below 10 and the tolerance statistics all 
# well above 0.2. Also, the average VIF is very close to 1. Based on these measures we can 
# safely conclude that there is no collinearity within our data. 


1/vif(model.1)

dataset<-dataset %>% filter(cooks.distance < 1 | cooks.distance > -1)
count(dataset)

plot(sort(dataset$cooks, decreasing=TRUE))


autoplot(model.1)
plot(model.1)

summary(model.3)


summary(model.1)







# OPTIONAL
dataset<- dataset %>%  filter(SellerG=='Nelson'|
                                SellerG=='Jellis'|
                                SellerG=='hockingstuart'|
                                SellerG=='Barry'|
                                SellerG=='Ray')
















library(ggfortify)



# COMPARE MODEL


lower_bound <- quantile(dataset_log$Price, 0.025)
upper_bound <- quantile(dataset_log$Price, 0.975)


#dataset_log<- dataset_log %>% filter(dataset_log$Price>=lower_bound &     dataset_log$Price<=upper_bound )





#https://www.kaggle.com/benherbertson/melbourne-housing-market-eda-basic-regression
#https://www.kaggle.com/benherbertson/melbourne-housing-market-eda-basic-regression

#https://www.kaggle.com/gorantlarohan/melbourne-housing-price-prediction



library(tidyverse);
library(ggplot2);
library(dplyr);
library(ggpubr);
library(gridExtra);
library(jtools)

library(magrittr);
library(knitr);
library(summarytools);
library(kableExtra);
library(skimr)

library(pastecs)
library(psych) 
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)