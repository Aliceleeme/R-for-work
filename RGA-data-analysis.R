
# RGA 분석 
# 2018-07-17 
# Written by Jihye Lee 

# how to use RGA; http://cloud.dbinc.co.kr/2017/08/03/google-analytics-api-r/
# code development based on http://clicknote.tistory.com/31

#### package 설치 ####
install.packages("RGA")
devtools::install_github("googleCausalImpact")

library(RGA)
authorize() #RGA 접속 인증작업 
#rga.open(instance == "ga")
library(CausalImpact) #그래프를 그리기 위한 라이브러리 

## ------------------------------------------------------------------------------- ## 

#ga_profiles = list_profiles()
#id = ga_profiles[grep("https://welcomebank.co.kr", ga_profiles$websiteUrl), "id"]

#list_profiles() #ga 안에 있는 profiles ID 및 정보 확인 가능 

## ------------------------------------------------------------------------------- ## 

viewID <- "120448724" #보고자하는 viewID 설정 

# 저희의 경우 별도의 비교군이 있는 것이 아니기 때문에 DATA를 불러올 때 From-to를 맥스로 설정하면 됩니다
gaData <- get_ga(viewID, start.date = as.Date("2017-11-01"),
end.date = as.Date("2018-04-30"), metrics = "ga:sessions", dimensions = "ga:date")

---# 비활성 코드 #---
#gaDataIntervention <- get_ga(viewID, start.date = as.Date("2017-11-01"),
#end.date = as.Date("2017-12-31 "), metrics = "ga:sessions", dimensions = "ga:date")
#gaDataNonIntervention <- get_ga(viewID, start.date = as.Date("2018-03-01"),
#end.date = as.Date("2018-04-30"), metrics = "ga:sessions", dimensions = "ga:date")
---------------------

# 데이터 정리  
workingData <- gaData
str(workingData) 

---# 비활성 코드 #---
#combinedData <- cbind(gaDataIntervention, gaDataNonIntervention)
#workingData <- combinedData[-c(3)]
#combinedData
#str(combinedData) 
#str(workingData) 
---------------------

# 시간 데이터로 변환 
workingData <- zoo(workingData[, -1], workingData$date)

# 비교를 위한 시간대아토 터압 설정 
pre.period = c(as.POSIXct("2017-11-01"), as.POSIXct("2017-12-31"))
post.period = c(as.POSIXct("2018-03-01"), as.POSIXct("2018-04-30"))

# 분석 및 그래프 그리기 
im <- CausalImpact(workingData, pre.period, post.period)
plot(im)

# 해석을 위한 추가적 통계값 요약 그래프 
summary(im)
