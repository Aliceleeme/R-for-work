

#sampling  
#dealing with imbalanced classification problems in R  
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/ 

# simple sampling 
#sample(, replace = FALSE)

# Stratified sampling 1 - 층화샘플링
require("data.table")
require("sampling")
data_s = as.data.frame(strata(data,size=3000, method="srswor"))
data_s2 = as.data.frame(strata(data,size=3000, method="srswor"))
data_s = as.data.frame(strata(data, c(data$1:data2$43), size=3000, method="srswor"))

# "srswor" = sample random sampling without replacement, basic.
# strata(data, stratanames=NULL, size, method=c("srswor", "srswr", "poisson", "systematic"), pik,description=FALSE)
# ROSE and DMwR
# "srswor" = sample random sampling without replacement, basic.
# strata(data, stratanames=NULL, size, method=c("srswor", "srswr", "poisson", "systematic"), pik,description=FALSE)


# Oversampling 1
library(DMwR)
recommend2 = DMwR::SMOTE(recommend~., data = cx_recover, perc.over=500, perc.under=2500)      #DMwR "out of bounds" issue https://arulvelkumar.wordpress.com/2017/04/30/smote-function-in-r-error-in-ti-subscript-out-of-bounds/
data2 = DMwR::SMOTE(Segments ~., data = cxa2, perc.over=500, perc.under=2500)
data3 = DMwR::SMOTE(Segments ~., data = cxa2, perc.over=1500, perc.under=500)


# Oversampling
library("ROSE")
data_ovrs <- ovun.sample(cls ~ ., data = data, method = "over", N = 3059)

data_balanced_both <- ovun.sample(Segments ~ .,
                                  data = data, method = "both", p=0.5, N=3000, seed = 1)$data

#출처: http://goodtogreate.tistory.com/entry/Handling-Class-Imbalance-with-R-and-Caret-An-introduction [GOOD to GREAT]


# After sampling 
hist() #histogram 
prop.table(table(df_r2$recommend)) #check class distribution 
