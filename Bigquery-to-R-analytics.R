### Bigquery data extraction #### 

library(bigrquery)

# project, dataset 이름 정의 
projectName <- "ga360-export" 
datasetName <- "ga_sessions_web_view"

ds <- bq_dataset(projectName, datasetName)

# 예시용 
sql <- "
  SELECT fullVisitorId , visitId 
  FROM `v_visit`
  WHERE TABLE_DATE BETWEEN @startDate AND @endDate;"


# 실전용; device 데이터 추출 

#sql <- " #sql문 입력 


#tb=table  
tb <- bq_dataset_query(ds,
                       query = sql,
                       parameters = list(startDate = "20180501" , endDate = "20180531"),
                       billing = NULL,
                       quiet = NA)

result <- bq_table_download(tb, 
                            page_size = 200 , 
                            start_index = 1 , 
                            max_connections = 5, 
                            max_results = 20000)

print(result)

#### Data wrangling #### 
#ref https://rpubs.com/jmhome/R_data_processing

data <- result

# variables
str(data)
head(data)

# data backup
dt <- data

# data 결측치 전부 제거 (신중함 필요)
sum(is.na(dt))
dt <- na.omit(dt)

# operatingSystem이 NA인 데이터만 출력
#library(dplyr)
#dt %>% filter(is.na(dt$iv1))   # operatingSystem이 NA인 데이터만 출력
#df_nomiss <- df %>% filter(!is.na(dt$iv1))   # operatingSystem 결측치 제거한 데이터 만들기 


# chr to num 
dt$isRegister <- as.character(dt$dv1)
dt$isApply <- as.character(dt$dv1)


# data distribution (EDA)
hist(dt$iv1)
hist(dt$dv1)

# 
dimnames(dt$mobileDeviceBranding)

# 접수율과 승인율 갯수 알아보기 
length(which(dt$iv1==1))
length(which(dt$iv1==0))
length(which(dt$dv1==1))
length(which(dt$dv1==0))

length(which(dt$TABLE_DATE=="20180531"))
length(which(dt$TABLE_DATE=="20180515"))

# 카테고리 데이터로 바꾸기 (1,2,3,4...)

test$type <- 
  ifelse(test$bank %in% sob, 1, 
  ifelse(test$bank %in% fob, 2, 
  ifelse(test$bank %in% jov, 3,
    4)))


# use quantiles for cut https://stackoverflow.com/questions/40380112/categorize-continuous-variable-with-dplyr

# Chi-suqare test 

#https://stackoverflow.com/questions/25350618/run-chi-squared-test-on-a-data-frame
chisq.test(dt[,-1])
chisq.test(as.matrix(df[,-1]))

table(survey$W.Hnd)
chisq.test(table(dt$iv1))


#chisq.test(table(survey$W.Hnd), p=c(.3, .7))

# csv exporting 
write.csv(data, file="ga360-data-20180621-2.csv", row.names = TRUE)
