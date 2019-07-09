#R study
#last updated 2018-02-26 

#### ARIMA time series analysis #### 
#ref: http://www.dodomira.com/2016/04/21/arima_in_r/

#data()           # R dataset list 
data(AirPassengers)           # Call the dataset named "AirPassengers"
str(AirPassengers)            #

plot(AirPassengers)            #plotting
plot(stl(AirPassengers, s.window="periodic"))    #Periodic = Seasonality, Trend, random으로 분리하여 보여줌 

library(tseries)
#data를 로그로 변환한 후 adf.test (Augmented Dickey-Fuller Test)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#install.packages("forecast")
library(forecast)
auto.arima(diff(log(AirPassengers)))        #p, d, q 파라미터 계산 
                                            # ARIMA = AR 모형 + MA 모형 
                                            # ARIMA 모델에서는 세가지 모형을 위한 세가지 파라미터 (p, d, q)가 필요.
                                            # 파라미터를 구하기 위해서는 AR(p)모형의 p차수 MA(q)의 q차수 
                                            # 그리고 트랜드를 제거하여 안정시계열로 만들기 위한 I(d)의 차분 차수 d를 결정하기 위해 KPSS test4 , ACF, PACF를 그려보아야 합니다.
                                            # R에서는 auto.arima를 사용해 자동으로 p, d, q를 결정
tsdiag(auto.arima(diff(log(AirPassengers))))


#ARIMA 모형 만들기 
fit <- arima(log(AirPassengers), c(1, 0, 1), 
             seasonal = list(order = c(0, 1, 1),period = 12))

#미래모델 예측
pred <- predict(fit, n.ahead = 10*12)        #predict = 예측함수 
                                             #n.ahead=10*12 -> 10년간 12개월치 추이 예측 
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

