setwd("D:/데이터분석/프로젝트/rr")
getwd()
#lubridate는 tidyverse 계열이기는 하지만 날짜와 시간 데이터를 처리할 때만 필요하기 때문에 tidyverse를 불러올 때 같이 따라오지 않습니다.
install.packages('lubridate')
install.packages('tidyverse')
install.packages('dplyr')

library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(tidyverse)

##############
#정보

##### 1. 데이터 불러오기
#1.1 따릉이 월별 시간별 이용대수
final_xx=data.frame()

for (i in 1:12){
  if (i %in% 1:6){
    xldata<-read_excel(paste0('공공자전거 이용정보(시간대별)_21.',i,'.xlsx'))
  }
  else{
    xldata<-read_excel(paste0('공공자전거 이용정보(시간대별)_20.',i,'.xlsx'))
  }
    
  # 불러온 파일의 columns명 확인
  df<-as.data.frame(xldata)
  df1<-df[which(df$대여소번호=='502'),]
  day_time<-aggregate(이용건수~대여일자+대여시간, data=df1, sum, drop=FALSE) #drop=FALSE:빈값 NA처리
  day_time[is.na(day_time)] <- 0   #NA 값 0으로 대체
  # nrow(day_time)
  
  day_time$일시 = paste0(as.character(day_time$대여일자)," ", day_time$대여시간,":","00:00")
  row.names(day_time) = NULL
  
  #1.2 기후데이터
  xl_weather<-read_excel(paste0(i,'_weather.xlsx'))
  weather_df<-as.data.frame(xl_weather)
  weather_df<-weather_df[,3:8]
  
  
  #1.3 미세먼지 데이터
  xl_dust<-read_excel(paste0(i,'_dust.xlsx'))
  dust_df<-as.data.frame(xl_dust)
  dust_df<-dust_df[,3:4]
  
  # 기후데이터+미세먼지 데이터 병합 merge
  final_x<-merge(weather_df,dust_df,by='일시',all=T) 
  final_x$시간<-hour(final_x$일시)
  row.names(final_x)=NULL
  
  final_df<-merge(day_time,final_x, by='일시')
  
  day_time$일시 = strptime(day_time$일시, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  # data merge For phase
  
  for (i1 in 1:length(final_x[,1])){
    for(i2 in 1:length(day_time[,1])){
      if(as.numeric(final_x$일시[i1]) == as.numeric(day_time$일시[i2])){
        final_x$이용건수[i1] = day_time$이용건수[i2]
      }
    }
  }
  
  final_xx<-rbind(final_x,final_xx)
}

############################################################################
final_xx<-na.omit(final_xx)


#시각화
install.packages('corrplot')
library(corrplot)
tmp<-final_xx_nn[,-c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16)]
co_plot<-cor(tmp)
round(co_plot, 2) 
corrplot(co_plot, method='shade', shade.col=NA, tl.col='black', tl.srt=45)  #,addCoef.col="black"


library(lubridate)
library(tidyverse)
#wday() 함수는 기본적으로 일요일~토요일을 1~7로 표시

final_xx$요일<-wday(final_xx$일시)
#library(writexl)
#write_xlsx(final_xx,"final_xx.xlsx")

##########################################################################


names(final_xx)<-c("일시","기온","풍향","풍속","강수량","습도","미세먼지","시간","이용건수","요일")
final_xx$요일<-as.factor(final_xx$요일)   #요일을 factor
final_xx$시간<-as.factor(final_xx$시간)   #시간을 factor

#df<-transform(df,
#              월요일=ifelse(요일=='월',1,0),
#              화요일=ifelse(요일=='화',1,0),
#              수요일=ifelse(요일=='수',1,0),
#              목요일=ifelse(요일=='목',1,0),
#              금요일=ifelse(요일=='금',1,0),
#              토요일=ifelse(요일=='토',1,0),
#              일요일=ifelse(요일=='일',1,0))

#df$월요일=as.factor(df$월요일)
#df$화요일=as.factor(df$화요일)
#df$수요일=as.factor(df$수요일)
#df$목요일=as.factor(df$목요일)
#df$금요일=as.factor(df$금요일)
#df$토요일=as.factor(df$토요일)
#df$일요일=as.factor(df$일요일)


#+기온+풍향+풍속+강수량+습도+미세먼지+
#for(i in c('월','화','수','목','금','토','일')){
#  lm_result<-lm(이용건수~시간, data=final_xx[which(final_xx$요일==i),])  #요일:7-1개
#  print(summary(lm_result))
#}


#####################################


#final_xx<-final_xx[,-1]
#lm_result1<-lm(이용건수~시간+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx)
#lm_result2<-lm(이용건수~요일+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx)
#lm_result3<-lm(이용건수~시간+요일+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx)
#lm_result4<-lm(이용건수~시간*요일+시간+요일+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx)
   # 모두 10을 넘지 않음
summary(lm_result1<-step(lm_result1, direction='backward'))

install.packages('writexl')
library(writexl)
a<-summary(lm(이용건수~., data=final_xx))

install.packages("xlsx")
library(xlsx)


all_result<-step(lm(이용건수~.,data=final_xx))
capture.output(summary(all_result),file='./lm_result.txt')


all_result_multi<-step(lm(이용건수~시간*요일+시간+요일+기온+풍향+풍속+강수량+습도,data=final_xx))
capture.output(summary(all_result_multi),file='./lm_result_multi.txt')

install.packages('car')
library(car)
vif(all_result)

summary(all_result)

summary(lm_result1<-step(all_result), direction=backward)

glm_result<-glm(이용건수~시간+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx, family=poisson)

glm_result<-glm(이용건수~시간*요일+시간+요일+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx, family=poisson)

glm_result<-glm(이용건수~시간+요일+기온+풍향+풍속+강수량+습도+미세먼지, data=final_xx, family=poisson)
summary(all_result<-step(glm(이용건수~.,data=final_xx)))

SSE=sum((final_xx$이용건수-glm_result$fitted.values)^2)
SST=sum((final_xx$이용건수-mean(final_xx$이용건수))^2)
R_2=1-SSE/SST
R_2

all_result<-step(glm(이용건수~.,data=final_xx))
capture.output(summary(all_result),file='./glm_aov_result.txt')


#선형->포아송

###################################



library(neuralnet)
library(caret)
set.seed(1)
#final_xx_matrix<-as.matrix(final_xx)


final_xx_nn<-final_xx
final_xx_nn$이용건수<-(final_xx_nn$이용건수-min(final_xx_nn$이용건수,na.rm=T))/(max(final_xx_nn$이용건수,na.rm=T)-min(final_xx_nn$이용건수,na.rm=T))
final_xx_nn$풍향<-(final_xx_nn$풍향-min(final_xx_nn$풍향,na.rm=T))/(max(final_xx_nn$풍향,na.rm=T)-min(final_xx_nn$풍향,na.rm=T))
final_xx_nn$풍속<-(final_xx_nn$풍속-min(final_xx_nn$풍속,na.rm=T))/(max(final_xx_nn$풍속,na.rm=T)-min(final_xx_nn$풍속,na.rm=T))
final_xx_nn$습도<-(final_xx_nn$습도-min(final_xx_nn$습도,na.rm=T))/(max(final_xx_nn$습도,na.rm=T)-min(final_xx_nn$습도,na.rm=T))
final_xx_nn$미세먼지<-(final_xx_nn$미세먼지-min(final_xx_nn$미세먼지,na.rm=T))/(max(final_xx_nn$미세먼지,na.rm=T)-min(final_xx_nn$미세먼지,na.rm=T))
final_xx_nn$기온<-(final_xx_nn$기온-min(final_xx_nn$기온,na.rm=T))/(max(final_xx_nn$기온,na.rm=T)-min(final_xx_nn$기온,na.rm=T))

final_xx_nn$요일1=0
final_xx_nn$요일1[which(final_xx$요일==1)]=1
  
final_xx_nn$요일2=0
final_xx_nn$요일2[which(final_xx$요일==2)]=1

final_xx_nn$요일3=0
final_xx_nn$요일3[which(final_xx$요일==3)]=1

final_xx_nn$요일4=0
final_xx_nn$요일4[which(final_xx$요일==4)]=1

final_xx_nn$요일5=0
final_xx_nn$요일5[which(final_xx$요일==5)]=1

final_xx_nn$요일6=0
final_xx_nn$요일6[which(final_xx$요일==6)]=1

final_xx_nn$요일7=0
final_xx_nn$요일7[which(final_xx$요일==7)]=1


#시간

final_xx_nn$시간0=0
final_xx_nn$시간0[which(final_xx$시간==0)]=1

final_xx_nn$시간1=0
final_xx_nn$시간1[which(final_xx$시간==1)]=1

final_xx_nn$시간2=0
final_xx_nn$시간2[which(final_xx$시간==2)]=1

final_xx_nn$시간3=0
final_xx_nn$시간3[which(final_xx$시간==3)]=1

final_xx_nn$시간4=0
final_xx_nn$시간4[which(final_xx$시간==4)]=1

final_xx_nn$시간5=0
final_xx_nn$시간5[which(final_xx$시간==5)]=1

final_xx_nn$시간6=0
final_xx_nn$시간6[which(final_xx$시간==6)]=1

final_xx_nn$시간7=0
final_xx_nn$시간7[which(final_xx$시간==7)]=1

final_xx_nn$시간8=0
final_xx_nn$시간8[which(final_xx$시간==8)]=1

final_xx_nn$시간9=0
final_xx_nn$시간9[which(final_xx$시간==9)]=1

final_xx_nn$시간10=0
final_xx_nn$시간10[which(final_xx$시간==10)]=1

final_xx_nn$시간11=0
final_xx_nn$시간11[which(final_xx$시간==11)]=1

final_xx_nn$시간12=0
final_xx_nn$시간12[which(final_xx$시간==12)]=1

final_xx_nn$시간13=0
final_xx_nn$시간13[which(final_xx$시간==13)]=1

final_xx_nn$시간14=0
final_xx_nn$시간14[which(final_xx$시간==14)]=1

final_xx_nn$시간15=0
final_xx_nn$시간15[which(final_xx$시간==15)]=1

final_xx_nn$시간16=0
final_xx_nn$시간16[which(final_xx$시간==16)]=1

final_xx_nn$시간17=0
final_xx_nn$시간17[which(final_xx$시간==17)]=1

final_xx_nn$시간18=0
final_xx_nn$시간18[which(final_xx$시간==18)]=1

final_xx_nn$시간19=0
final_xx_nn$시간19[which(final_xx$시간==19)]=1

final_xx_nn$시간20=0
final_xx_nn$시간20[which(final_xx$시간==20)]=1

final_xx_nn$시간21=0
final_xx_nn$시간21[which(final_xx$시간==21)]=1

final_xx_nn$시간22=0
final_xx_nn$시간22[which(final_xx$시간==22)]=1

final_xx_nn$시간23=0
final_xx_nn$시간23[which(final_xx$시간==23)]=1

#final_xx_nn<-final_xx_nn[,-c(8,10)]
#final_xx_nn<-final_xx_nn[,-1]
library(dplyr)
train=sample_frac(final_xx_nn, size=0.7)
test=setdiff(final_xx_nn, train)

nn<-neuralnet(이용건수~시간0+시간1+시간2+시간3+시간4+시간5+시간6+시간7+시간8+시간9+시간10+시간11+시간12+시간13+시간14+시간15+시간16+시간17+시간18+시간19+시간20+시간21+시간22+시간23+일+월+화+수+목+금+토+기온+풍향+풍속+강수량+습도+미세먼지,linear.output=F, data=train, hidden=c(5,5) ) #linear.output=F, #error 발생
#train

prediction(nn)
plot(nn,rep='best')
predict<-compute(nn, train)
y_mean=mean(train$이용건수)
SSE=sum((train$이용건수-predict$net.result)^2)
SST=sum((train$이용건수-y_mean)^2)
R_2=1-SSE/SST
R_2

#test
prediction(nn)
plot(nn,rep='best')
predict<-compute(nn, test)
y_mean=mean(test$이용건수)
SSE=sum((test$이용건수-predict$net.result)^2)
SST=sum((test$이용건수-y_mean)^2)
R_2=1-SSE/SST
R_2
