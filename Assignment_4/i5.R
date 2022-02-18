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
library(skimr);

library(pastecs);
library(psych); 
library(grid);
library(gridExtra);
library(readr)
winered<- read_delim("winequality-red.csv", delim = ";",col_types = cols(.default = col_double()));
winered$target<-0
winewhite<-read_delim("winequality-white.csv", delim = ";",  col_types = cols(.default = col_double()));
winewhite$target<-1
wine<- rbind(winered,winewhite) 

winered<-NULL
winewhite<-NULL
unique(wine$target)

a<-describe(wine) #%>% tb(); 
b<- a %>% mutate_if(is.numeric, ~round(.,2)) %>% select(1:7);
kable(b, "pandoc", caption = "summary Statistics")

wine<-na.omit(wine)
describe(wine)




log_wine<- log(wine)
log_wine$target<- wine$target
log_wine$`citric acid`<-log(wine$`citric acid`+1)
# Fit the logistic regression model
model <- glm(target ~., data = wine,     family = binomial)
# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "white", "red")
head(predicted.classes)


# Select only numeric predictors
mydata <- wine %>%  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)




ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")







 
names(winered)
names(winewhite)
ggplot(wine, aes(y=target, x=density)) + geom_jitter(width=0.1, height=0.25)+ facet_grid(~quality)
ggplot(wine, aes(y=target, x=`fixed acidity`)) + geom_jitter(width=0.1, height=0.25)
ggplot(wine, aes(y=target, x=density)) + geom_jitter(width=0.1, height=0.25)
ggplot(wine, aes(y=target, x=density)) + geom_point()

library(car);
library(mlogit)
wine$target<-relevel(wine$target, 0)
wineModel.1 <- glm(target ~ pH, data = wine, family = binomial())
wineModel.2 <- glm(target ~ density+quality+alcohol, data = wine, family = binomial())
wineModel.3 <- glm(target ~ ., data = log_wine, family = binomial())
# The null deviance is the deviance of the model that contains no predictors other 
#than the constant - in other words, -2LL(baseline).5The residual deviance is the deviance 
#for the model - in other words, -2LL(new).
summary(wineModel.1)



logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}


logisticPseudoR2s(wineModel.1)
logisticPseudoR2s(wineModel.2)
logisticPseudoR2s(wineModel.3)
#`volatile acidity`  `residual sugar`  chlorides `free sulfur dioxide` `total sulfur dioxide` density alcohol
exp(wineModel.3$coefficients)
exp(confint(wineModel.3))

# testing for multi-colinearity  
#:These values indicate that there is a problem of 
#collinearity: a VIF over 10 is usually considered problematic

vif(wineModel.3)#378
1/vif(wineModel.3)
wineModel.4 <- glm(target ~pH+sulphates+alcohol+quality+chlorides+`free sulfur dioxide` +`total sulfur dioxide`+`fixed acidity` +`volatile acidity`+`citric acid`+`residual sugar` , data = wine, family = binomial())       

full.model <- glm(target ~., data = wine, family = binomial)
coef(full.model)

library(MASS)
step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)

# independence of errors
durbinWatsonTest(wineModel.3)
summary(wineModel.3)

#both of which do the same thing. As a conservative rule I suggested that values less than 1 
#or greater than 3 should definitely raise alarm bells. The closer to 2 that the value is, the 
#better, and for these data (Output 7.8) the value is 1.950, which is so close to 2 that the 
#assumption has almost certainly been met. The p-value of .7 confirms this conclusion (it is 
#very much bigger than .05 and, therefore, not remotely significant). 
#(The p-value is a little
#strange, because it is bootstrapped, and so, for complex reasons that we don't want to go 
#into here, it is not always the same every time you run the command.


# Testing for linearity of the logit 
##--Output 8.7 shows the part of the output that tests the assumption. We're interested 
##--only in whether the interaction terms are significant. Any interaction that is significant 
##--indicates that the main effect has violated the assumption of linearity of the logit. All three 
##--interactions have significance values (the values in the column Pr(>|z|)) greater than .05, 
##--indicating that the assumption of linearity of the logit has been met for PSWQ, Anxious
##--and Previous.
winered<- read_delim("winequality-red.csv", delim = ";",col_types = cols(.default = col_double()));
winered$target<-0
winewhite<-read_delim("winequality-white.csv", delim = ";",  col_types = cols(.default = col_double()));
winewhite$target<-1
wine<- rbind(winered,winewhite) 

winered<-NULL
winewhite<-NULL
unique(wine$target)

a<-describe(wine) #%>% tb(); 
b<- a %>% mutate_if(is.numeric, ~round(.,2)) %>% select(1:7);
kable(b, "pandoc", caption = "summary Statistics")

wine<-na.omit(wine)








wine$logfixedacidityInt <- log(wine$`fixed acidity`)*wine$`fixed acidity`
wine$logvolatileacidity<- log(wine$`volatile acidity`)*wine$`volatile acidity`
wine$logcitriceacid<- log(wine$`citric acid`+1)*wine$`citric acid`
wine$logresidualsugar<- log(wine$`residual sugar`)*wine$`residual sugar`
wine$logchlorides<- log(wine$chlorides)*wine$chlorides
wine$logfreesulfurdioxide<- log(wine$`free sulfur dioxide`)*wine$`free sulfur dioxide`
wine$logtotalsulfurdioxide<- log(wine$`total sulfur dioxide`)*wine$`total sulfur dioxide`
wine$logdensity<- log(wine$density)*wine$density
wine$logpH<- log(wine$pH)*wine$pH


wine$logsulphates<- log(wine$sulphates)*wine$sulphates
wine$logalcohol<- log(wine$alcohol)*wine$alcohol

wine$logquality<- log(wine$quality)*wine$quality     




#wineModel.log.3 <- glm(target ~ logdensity+logresidualsugar+logfixedacidityInt+
#                         density+`residual sugar`+`citric acid`+`fixed acidity`
#                         , data=wine, family=binomial())


wineModel.log.3 <-boxcox( glm(target ~ density+pH+logdensity+logpH, data=wine, family=binomial()))






summary(boxcox(wineModel.log.3))






wine$sqfixedacidityInt <- (wine$`fixed acidity`**2)*wine$`fixed acidity`
wine$sqvolatileacidity<- (wine$`volatile acidity`**2)*wine$`volatile acidity`
wine$sqcitriceacid<- (wine$`citric acid`**2)*wine$`citric acid`
wine$sqresidualsugar<- (wine$`residual sugar`**2)*wine$`residual sugar`
wine$sqchlorides<- (wine$chlorides**2)*wine$chlorides
wine$sqfreesulfurdioxide<- (wine$`free sulfur dioxide`**2)*wine$`free sulfur dioxide`
wine$sqtotalsulfurdioxide<- (wine$`total sulfur dioxide`**2)*wine$`total sulfur dioxide`
wine$sqdensity<- (wine$density**2)*wine$density
wine$sqpH<- (wine$pH**2)*wine$pH


wine$sqsulphates<- (wine$sulphates**2)*wine$sulphates
wine$sqalcohol<- (wine$alcohol**2)*wine$alcohol

wine$sqquality<- (wine$quality**2)*wine$quality 



wineModel.log.3 <- glm(target ~ ., data=wine, family=binomial())
summary(wineModel.log.3)


names(wine)
model.1 <- glm(target ~ `free sulfur dioxide`+`citric acid`+chlorides, data = wine, family = binomial())
model.2 <- glm(target ~ sulphates+alcohol+quality  , data = wine, family = binomial())
model.3 <- glm(target ~ sulphates+alcohol+quality  , data = wine, family = binomial())
ggplot(wine, aes(x = quality, y = target, colour = sulphates, group = sulphates)) +   geom_line()

#B (SE) 
#95% CI for odds ratio -Lower 
#Odds Ratio 
#95% CI for odds ratio -Upper