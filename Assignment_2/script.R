burdenData <- read.csv("C:\\Users\\SALONI\\OneDrive - University of Waterloo\\MSCI 718\\Individual Assignment1\\Individual assignment2\\TB_burden_countries_2021-03-02.csv", header = TRUE)

burdenData

library(skimr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(psych) 
library(grid)
library(gridExtra)
library(GGally)

df <- as_tibble(burdenData)

describe(df[,c("e_mort_num", "e_mort_num_hi","e_mort_num_lo","e_mort_tbhiv_num",
               "c_cdr",
               "e_inc_num","e_inc_num_lo","e_inc_num_hi","c_cdr_lo","c_cdr_hi","e_inc_tbhiv_num","e_inc_tbhiv_num_lo","e_inc_tbhiv_num_hi")])

new_df<- df%>%select("e_mort_num", "e_mort_num_hi","e_mort_num_lo",
                     "e_mort_tbhiv_num","c_cdr","c_cdr_lo","c_cdr_hi","e_inc_tbhiv_num",
                     "e_inc_tbhiv_num_lo","e_inc_tbhiv_num_hi",
                     
                     "e_inc_num","e_inc_num_lo","e_inc_num_hi"
                     
                     ) 


new_df<-mutate(new_df, log_e_mort_num = log(e_mort_num+1),
              log_e_mort_num_hi=log(e_mort_num_hi+1), 
              log_e_mort_num_lo = log(e_mort_num_lo+1), 
              log_e_mort_tbhiv_num = log(e_mort_tbhiv_num+1) , 
              log_c_cdr = log(c_cdr+1),
               log_c_cdr_lo = log(c_cdr_lo+1),
              log_c_cdr_hi = log(c_cdr_hi+1),
             log_e_inc_tbhiv_num=log(e_inc_tbhiv_num+1),
               log_e_inc_tbhiv_num_lo=log(e_inc_tbhiv_num_lo+1),
              log_e_inc_tbhiv_num_hi=log(e_inc_tbhiv_num_hi+1),
              log_e_inc_num=log(e_inc_num+1),
              log_e_inc_num_lo=log(e_inc_num_lo+1),
              log_e_inc_num_hi=log(e_inc_num_hi+1)
               
               )

describe(x[,c("log_e_mort_num", "log_e_mort_num_hi","log_e_mort_num_lo","log_e_mort_tbhiv_num",
                   "log_c_cdr","log_c_cdr_lo","log_c_cdr_hi",
                   "log_e_inc_tbhiv_num","log_e_inc_tbhiv_num_lo","log_e_inc_tbhiv_num_hi",
                   "log_e_inc_num","log_e_inc_num_lo","log_e_inc_num_hi"
                   
                   )])
x <- na.omit(new_df)
new_df<-na.omit(new_df)

data<-x %>% select("log_e_mort_num","log_c_cdr_lo","log_e_inc_tbhiv_num_lo")

#new_df<-new_df[!sapply(new_df,is.null)]

mortality<- data$log_e_mort_num
#cases_hiv<- new_df$log_e_inc_tbhiv_num
cases<- data$log_e_inc_tbhiv_num_lo
treatment_coverage<-data$log_c_cdr_lo



length(mortality)

c1<-cor.test(mortality, treatment_coverage)
c2<-cor.test(mortality, cases)
c3<-cor.test(cases, treatment_coverage)
c1


c2

c3


cor(data)^2 * 100



library(ggm)

pc<-pcor(c("log_e_mort_num", "log_c_cdr_lo", "log_e_inc_tbhiv_num_lo"), var(new_df))
pc



pc
pc^2

pcor.test(pc, 1, 3591)


pc
c1

#pcor.test(x = data$log_e_mort_num, y = data$log_c_cdr_lo, z = data$log_e_inc_tbhiv_num)


For example, analyses of the treatment coverage data 


showed that mortality was negatively related to treatment coverage, but positively 
related to cases having both hiv and tb, and (tb+hiv)cases itself was negatively related 
to treatment coverage. 
This scenario is complex, but given that we know that mortality is related to both 
tb+hiv cases and treatment coverage, then if we want a pure measure of the relationship 
between moratlity and no of cases we need to take account of the influence of 
treatment coverage. 

Using the values of R2
for these relationships [(refer back to Output 6.4)], we 
know that treatment coverage accounts for 42.89% of the variance in moratlity, that 
cases accounts for 81.74% of the variance in moratlity, 
and that no of cases accounts for 31.78% of the variance in treatment coverage.


If cases accounts for 31.78%  the variance in treatment coverage, then it seems feasible that at least some of the 42.89% of 
variance in moratlity that is accounted for by treatment is the same variance that 
is accounted for by cases. 


data$X<-data$log_e_mort_num
data$Y<-data$log_c_cdr_lo
data$Z<-data$log_e_inc_num

library(ppcor)
#pcor.test("log_e_mort_num"~"log_c_cdr_lo", "log_e_inc_num", var(data))

#"log_e_mort_num", "log_c_cdr_lo", "log_e_inc_num"


Y_resid<-resid(lm(log_c_cdr_lo~log_e_inc_tbhiv_num_lo,data))
X_resid<-resid(lm(log_e_mort_num~log_e_inc_tbhiv_num_lo,data))

library(ggplot2)
m<-ggplot(data, aes(x=X_resid, y=Y_resid)) +
  geom_point() +
  labs(x="X | Z", y = "Y | Z")+
  scale_size_manual(values=c(15))+
  theme_classic()
m +  geom_smooth(method=lm)





##########https://stackoverflow.com/questions/57886951/partial-cor-pcor-test-in-ggpairs

#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("magrittr") 
#install.packages("dplyr")
data

#data_new<-data %>% select("log_e_mort_num","log_c_cdr_lo","log_e_inc_num")





library("dplyr") 
library("magrittr")

data
upper = list(continuous = function(data, mapping) { print(list(data, mapping)) })
upper

library(tidyverse)
pcor_panel <- function(data, mapping, ...) {
  ## remove x, y mapping
  grp_aes <- mapping[setdiff(names(mapping), c("x", "y"))]
  ## extract the columns to which x and y is mapped
  xy <- sapply(mapping[c("x", "y")], rlang::as_name)
  ## calculate pcor per group
  stats <- data %>%
    group_by(!!!unname(unclass(grp_aes))) %>%
    group_modify(function(dat, grp) {
      res <- pcor(dat)$estimate %>%
        as_tibble() %>%
        setNames(names(dat)) ## needed b/c in pcor names are sometimes messed up
      res <- res %>%
        mutate(x = names(res)) %>%
        gather(y, pcor, -x)
      res %>%
        filter(x == xy[1], y == xy[2]) ## look only at the pcors of this panel
    }) %>% 
    ungroup() %>%
    mutate(x = 1, y = seq_along(y))
  ggplot(stats, aes(x, y, label = round(pcor, 3))) +
    geom_text(grp_aes) +
    ylim(range(stats$y) + c(-2, 2))
}

ggpairs(data, columns = c('log_e_mort_num', 'log_c_cdr_lo', 'log_e_inc_num'), title = "All Bivariate analysis", 
        #upper = list(continuous = pcor_panel),
       # lower = list(continuous = wrap("smooth", alpha = 0.6, size = 0.1))#,      mapping = aes(color = D)
        )















##   https://stackoverflow.com/questions/35591033/plot-scatterplot-matrix-with-partial-correlation-coefficients-in-r
##   http://faculty.cas.usf.edu/mbrannick/regression/Part3/Partials.html
##http://bradleyboehmke.github.io/tutorials/correlations

pp<-plot(mortality, treatment_coverage, main = "Main title",
         xlab = "X axis title", ylab = "Y axis title",
         pch = 19, frame = FALSE)
abline(lm(treatment_coverage ~ mortality, data = data), col = "blue")



ggplot(data.frame(
  res.x = lm(log_e_mort_num ~ log_e_inc_tbhiv_num_lo, data)$residuals, 
  res.y = lm(log_c_cdr_lo ~ log_e_inc_tbhiv_num_lo, data)$residuals)) +
  geom_point(aes(res.x, res.y))
