
#### Logistic regression modeling test #### 
## written by Jihye Lee, June 2018 ##

data <- "/Alice/Downloads/data.csv"

#yogubul.df <- read.csv(data, header = TRUE, sep = ",")

library(readr)
yogubul.df <- read_csv(data)

class(yogubul.df)
head(yogubul.df)
#tail(yogubul.df)
str(yogubul.df)


#### data wrangling #### 
yogubul.df$D<-as.integer(yogubul.df$D)
yogubul.df$J<-as.integer(yogubul.df$J)
yogubul.df$K<-as.integer(yogubul.df$K)
yogubul.df$L<-as.integer(yogubul.df$L)
yogubul.df$M<-as.integer(yogubul.df$M)
yogubul.df$N<-as.integer(yogubul.df$N)
yogubul.df$O<-as.integer(yogubul.df$O)
yogubul.df$P<-as.integer(yogubul.df$P)
yogubul.df$Q<-as.integer(yogubul.df$Q)
yogubul.df$R<-as.integer(yogubul.df$R)
yogubul.df$U<-as.integer(yogubul.df$U)

yogubul.df$A<-as.factor(yogubul.df$A)
yogubul.df$S<-as.factor(yogubul.df$S)
yogubul.df$B<-as.factor(yogubul.df$B)
yogubul.df$E<-as.factor(yogubul.df$E)
yogubul.df$I<-as.factor(yogubul.df$I)

rdata11$S<-as.factor(rdata11$S)
rdata11$E<-as.factor(rdata11$E)
rdata11$I<-as.factor(rdata11$I)


#### 

# training / test data 분리
indexes <- sample(1:nrow(yogubul.df), size=0.8*nrow(yogubul.df))
train.data <- yogubul.df[indexes,]
test.data <- yogubul.df[-indexes,]


#### Random forest #### 

library(caret)             # 특징 선택 알고리즘 
library(randomForest)      # 랜덤 포레스트 알고리즘  

# Random forest feature selection 
run.feature.selection <- function(num.iters=20, feature.vars, class.var){
   set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv",
                        verbose = FALSE, returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var,
                     sizes = variable.sizes,
                     rfeControl = control)
  return(results.rfe)
}
                                    #num.iters=20 20회 반복 

# 결과 확인 
rfe.results <- run.feature.selection(feature.vars=train.data[,-1],
                                     class.var=train.data[,1])
rfe.results 
varImp(rfe.results) #모든 변수 성능 확인 



#### Logistic regression #### 

library(ROCR)
library(caret)

#data.11 = rdata11[c(2:6)]
#str(data.11)

# 특징character과 클래스 변수를 분리 
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1] 


# 독립변수를 활용한 초기모델 훈련 
#formula.init <- "A ~ ."
formula.init <- "A ~ S + I + E + G + H"
formula.init <- as.formula(formula.init)
lr.model <- glm(formula=formula.init, data=train.data, family="binomial")
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)

summary(lr.model)       #모델 성능 확인 


# formula development 
#formula <- "A ~ S + I + E + G + H"
#formula <- as.formula(formula)
#lr.model <- glm(formula = formula, data = train.data, family = "binomial")
#lr.predictions <- predict(lr.model, test.data, type="response")
#lr.predictions <- round(lr.predictions)

# class check 
class(lr.predictions) # ??
class(test.class.var) # ?? 
test.class.var <- as.data.frame(table(unlist(test.class.var)))

lr.predictions.df <- as.data.frame(table(unlist(lr.predictions)))

# prediction 
predictions = prediction(lr.predictions, test.class.var)
head(lr.predictions, 20)
tail(lr.predictions, 20)
data = lr.predictions$fit # 실행 해볼 필요 있음 / ref: https://stackoverflow.com/questions/44147676/confusion-matrix-x-must-be-atomic-for-sort-list

confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')

# test data 
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)
data=fitted.results$fit
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')

accuracy <- table(lr.predictions, test.data$A)
accuracy

# real data 
lr.predictions.new <- predict(logistic.regression.new, data=data.11, type="response")
lr.predictions.new <- round(lr.predictions.new)
lr.predictions.new

confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')

summary(lr.model)       #모델 성능 확인 


#### ROC Curve ####

#auc 값 확인
lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type="response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="LR ROC Curve")
plot.pr.curve(predictions, title.text="LR Precision/Recall Curve")



library(ROCR)
model_data = glm(A ~ H + I + M + S + V + L + N + K + D + J + G + E + P + R , family=binomial(link='logit'), data=test.data)
p = predict(model_data, newdata=test.data, type="response")
pr = prediction(p, test.data$A)
prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
plot(prf, col='blue', lty = 1, lwd = 3, main = 'ROC curve')


#### save the model 
save(m1, file = "my_model1.rda")

