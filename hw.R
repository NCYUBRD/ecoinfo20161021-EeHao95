setwd('/Users/user/Desktop/生態資訊/homework1028/1021data/raw')
##ecoinfo20161028 - EeHao
## don't know what i'm doing zzzz

##由於把全部c0M530的資料全部調出來combine在一個檔案裡面了，所以就直接用那個.txt
library(data.table)
M530 <- fread("/Users/user/Desktop/生態資訊/homework1028/1021data/raw/c0M530.txt" ,skip = 74, na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'), stringsAsFactors=FALSE)



## set頂端的命名
colum <- c("stno", "yyyymmddhr","PS01", "TX01", "RH01","WD01","WD02", "PP01","SS01")
setnames(M530 , 1:9 , colum)

## 先把時間欄位換成正確且有意義的時間格式
# 時間的格式(strptime(目標,'格式'))
strptime(M530$yyyymmddhr,'%Y%m%d%H')
##新增欄位用法  M530[, timestamp:=as.POSIXct(strptime(yyyymmddhr,'%Y%m%d%H'))]
##增加一個欄位叫checktime
M530[, checktime:=as.POSIXct(strptime(yyyymmddhr-1, '%Y%m%d%H'))]

# 需要用“日”來做aggregate的集合來計算
# 所以用format.Date的功能將hr去掉format.Date(data$checktime, '%Y-%m-%d')

M530[, day:= format.Date(checktime, '%Y-%m-%d')]

----
  

######### 1.計算C0M530(奮起湖)從2006~2015十年的：
####  a.每年每日日均溫daliy temp (DT)
####     ANS:mean_DailyTemp
  
{
#先將題目所需要用到的function設計出來
#將na值去除
mean_na <- function(x){
  x <- as.numeric(x)
  mean(x, na.rm = TRUE)

}

#用aggregate在TX01的欄位下用mean且在依day做集合排序
mean_DailyTemp <- aggregate(TX01 ~ day, data = M530, FUN = mean_na)
}


mean_DailyTemp


#### b.c.每年每日最低溫、最高溫Daliy Max/ min tmper (DMaxT/DminT)
####     ANS: DayHTemp & DayLTemp
{
#設計function主要是去除na值
high <- function (x){
  max(x, na.rm = TRUE)
}
low <- function (x){
    min(x, na.rm = TRUE)
}
  
DayHTemp<- aggregate(TX01 ~ day,data = M530, FUN = high)
DayLTemp<- aggregate(TX01 ~ day,data = M530, FUN = low)

}

DayHTemp
DayLTemp


#### d.每年每月均溫monthly temp (MT)
####   ANS:mean_MonthlyTemp
{
  #新增多一個欄位“月” month
  M530[, month:= format.Date(checktime, '%Y-%m')]
  mean_MonthlyTemp <- aggregate(TX01 ~ month, data = M530, FUN = mean)
  
}

mean_MonthlyTemp

#### e.每年每月累積降水Sum of Monthly Rain(SMR)
#### ANS:SMR
{
  sum_na <- function(x){
    x <- as.numeric(x)
    sum(x, na.rm = TRUE)
  }
  # Sum of Monthly Rain(SMR)
  SMR <- aggregate(PP01 ~ month,data = M530 , FUN = sum_na)
  
}

SMR





###ANS
##Q1
#a.mean_DailyTemp
#b.DayHTemp
#c.DayLTemp
#d.mean_MonthlyTemp
#e.SMR

