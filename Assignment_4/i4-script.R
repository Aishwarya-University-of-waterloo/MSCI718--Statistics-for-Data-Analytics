library(dplyr)
library(tidyr)
library(ggplot2)

##---------------INSTALLATION--------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")


##------------------END------------------------------------


##---------------DATA READING & ENCODING-----------------------

dataset <- read.csv('C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\I4\\melb_data.csv')
count(dataset)
#
count(dataset)
dataset<-dataset %>% separate(Date, c("Split_Day","Split_Month","Split_Year"),remove=FALSE)
dataset$Split_Day<- as.integer(dataset$Split_Day)
dataset$Split_Month <- as.integer(dataset$Split_Month)
dataset$Split_Year <- as.integer(dataset$Split_Year)
dataset$Date<-NULL
dataset$Suburb<-NULL  #HIGH CARDINALITY
dataset$Address<-NULL #HIGH CARDINALITY



##---------------------CHECK OUTLIERS--------------------
summary(dataset)
f <- ggplot(dataset, aes(y=Price))
f + geom_boxplot()


lower_bound <- quantile(dataset$Price, 0.025)
upper_bound <- quantile(dataset$Price, 0.975)


dataset_without_outlier<- dataset %>% filter(dataset$Price>=lower_bound &
                                               dataset$Price<=upper_bound )



f <- ggplot(dataset_without_outlier, aes(y=Price))
f + geom_boxplot()

count(dataset %>% filter(dataset$Type=='h'))
count(dataset %>% filter(dataset$Type=='u'))
count(dataset %>% filter(dataset$Type=='t'))


dataset$Type<- factor (dataset$Type,levels=c('h','u','t'),labels=c(1,2,3))
dataset$Method<- factor (dataset$Method,levels=c('S','SP','PI','VB','SA'),labels=c(1,2,3,4,5))
count(dataset)
dataset<- dataset %>%  filter(SellerG=='Nelson'|
                                SellerG=='Jellis'|
                                SellerG=='hockingstuart'|
                                SellerG=='Barry'|
                                SellerG=='Ray')
dataset$SellerG<- factor (dataset$SellerG,levels=c('Nelson',
                                                   'Jellis',
                                                   'hockingstuart',
                                                   'Barry',
                                                   'Ray'),
                          labels=c(1,2,3,4,5))



count(dataset)
dataset<- dataset %>% filter(Regionname=='Southern Metropolitan'|
                               Regionname=='Northern Metropolitan'|
                               Regionname=='Western Metropolitan'|
                               Regionname=='Eastern Metropolitan'|
                               Regionname=='South-Eastern Metropolitan')
dataset$Regionname<- factor (dataset$Regionname,levels=c('Southern Metropolitan',
                                                         'Northern Metropolitan',
                                                         'Western Metropolitan',
                                                         'Eastern Metropolitan',
                                                         'South-Eastern Metropolitan'),
                             labels=c(1,2,3,4,5))



##------------------END------------------------------------

#
#dataset$CouncilArea<-NULL



##----------------------EDA-----------

summary(dataset)
str(dataset)

names(dataset)

#https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html

#install.packages("DataExplorer")




library(ggplot2)

library(DataExplorer)
library(nycflights13)

introduce(dataset)
dataset <- na.omit(dataset)  # TOTAL_MISSING_VALUES=4957
sum(is.na (dataset))
sum(is.na (dataset$YearBuilt))
#5375 values are missing from YearBuilt
#




dataset<-dataset %>% filter(Landsize!=0)
dataset<-dataset %>% filter(BuildingArea!=0)
sum(is.na (dataset))


dataset$age_of_building<- 2021-dataset$YearBuilt
      




introduce(dataset)
unique(dataset$Rooms)



plot_histogram(dataset)

plot_correlation(na.omit(dataset), type = c("all", "discrete", "continuous"),maxcat = 5L)
dataset$Bedroom2<- NULL #HIGH CORRELATION WITH NO OF ROOMS





qq_data <- dataset[, c( "Price", "Distance",
                       "Postcode",
                       "Landsize","BuildingArea","age_of_building", #  YearBuilt
                       "Lattitude","Longtitude","Propertycount")]#"Car","Rooms","Bathroom",
plot_qq(qq_data, sampled_rows = 1000L)


log_qq_data <- update_columns(qq_data, 0:9, function(x) log(x))
plot_qq(log_qq_data[, 0:9]) #, sampled_rows = 1000L



library(GGally)
ggscatmat(dataset, columns = 1: ncol(dataset))


ggplot(data=dataset, aes(dataset$Price)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()


library(reshape)
names(dataset)
p <- ggplot(dataset)
p + geom_boxplot() + facet_wrap(~Rooms, scale="free")



qq_data <- final_data[, c("name_origin", "arr_delay", "air_time", "distance", "seats")]
plot_qq(qq_data, by = "name_origin") #, sampled_rows = 1000L





library("PerformanceAnalytics")

chart.Correlation(dataset, histogram=TRUE, pch=19)


res <- cor(dataset)
round(res, 2)
library(corrplot)
corrplot(res, type = "upper", order = "hclust",  tl.col = "black", tl.srt = 45)

library("Hmisc")
res2 <- rcorr(as.matrix(dataset))





corrplot(res2$r, type="upper", order="hclust",          p.mat = res2$P, sig.level = 0.01, insig = "blank")

names(dataset)



model.1<-lm(formula = Price ~ Lattitude+Longtitude,   data = dataset,na.action=na.exclude)
model.2 = lm(formula = Price ~ Rooms+Type+Bathroom+Landsize+BuildingArea+YearBuilt, data = dataset,na.action=na.exclude)#, 
model.3<-lm(formula = Price ~.,   data = dataset,na.action=na.exclude)


library(ggfortify)

autoplot(model.1)

autoplot(model.2)

autoplot(model.3)

# COMPARE MODEL
anova(model.1, model.2, model.3)

