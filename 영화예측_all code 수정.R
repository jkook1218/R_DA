setwd("C:/Users/JINKOOK/Desktop/bigcon2/bigcon2")
#불러오기 
library(readxl)
xlsx = dir(pattern="xlsx")
dat = c()
for (i in xlsx) {
  d = read_excel(i, col_names = F)
  dat = rbind(dat, d)
}

#변수명 설정 
colnames(dat) = c(
  'ranking', 'name', 'release_date',                     # 순위       영화명   개봉일
  'sales', 'sales_p', 'sales_change',                    # 매출액    매출액_점유율   매출액증감   
  'sales_change_p', 'sales_accumulated',                 # 매출액증감율   누적매출액 
  'audience', 'audience_change', 'audience_change_p',    # 관객수    관객수증감   관객수증감율   
  'audience_accumulated', 'Nof_screen', 'Nof_played' ,   # 누적관객수 스크린수    상영횟수    
  'nation_main', 'nation', 'maker',                      # 대표국적    국적    제작사 
  'distributor', 'filmRatings', 'Genre',                 # 배급사    등급      장르    
  'Director', 'actor', 'date')                           # 감독      배우    날짜  

#개봉일 처리
dat$release_date = as.Date(as.numeric(dat$release_date), origin="1899-12-30", na.pass=T)
#table(unique(dat$name))
#dat[dat$name == '산',]

#년월일 나누기
library(dplyr)
dat = dat %>% mutate(year = substr(date,1,4),
                     month = substr(date,7,8),
                     day = substr(date,11,12),
                     day_of_week = substr(date,15,15))


#리얼 날짜 만들기
dat=dat %>% mutate(date2=paste(dat$year,dat$month,dat$day,sep="-"))
dat$date2=as.Date(dat$date2)

z=dat %>% group_by(name) %>% arrange(date2) 

#개봉일 NA인 data 제거 , nation_main 한국,미국인애들만.
dat=dat%>%filter(!is.na(dat$release_date))

dat=dat%>%filter(nation_main=='한국'| nation_main=='미국')



#상영 첫 주 동시 개봉영화 수
first=as.Date('1954-12-13')

t=c()
for(i in 1:3220){t[i]= 7*i}

week=t+first
week=week+3

dat2=dat %>% group_by(name,Director) %>% summarise(unique_rle = min(release_date))

aaa=c()
for( i in 1:NROW(dat2)){
  a=which.min(abs(week-dat2$unique_rle[i]))
  aaa=rbind(aaa,a)
}


dat2$Director= strsplit((dat2$Director), ',' )
dat2$Director=as.factor(as.character(dat2$Director))

dat2$t=paste0(dat2$name,':',dat2$Director)

row.names(aaa)=c(1:NROW(aaa))
aaa=as.data.frame(aaa)
aaa=cbind(dat2$t,aaa$V1)
aaa=as.data.frame(aaa)

colnames(aaa)=c('t','same_release_week')

dat$t= strsplit((dat$Director), ',' )


dat$t=paste0(dat$name,':',dat$t)
x=merge(dat,aaa,all=TRUE)

#merge에서 all=TRUE해주면 공통되는 열 기준으로 병합
dat=x %>% arrange(date2,ranking)


dat3=dat %>% group_by(name,t) %>% summarise(unique_week = unique(same_release_week))

bbb=c()
for( i in 1:NROW(dat3)){
  b=table(dat3$unique_week==dat3$unique_week[i])
  bbb=rbind(bbb,b)
}

rownames(bbb)=c(1:NROW(dat3))




bbb=cbind(dat3$t,bbb[,2])
bbb=as.data.frame(bbb)


colnames(bbb)=c('t','same_week_table')
rownames(bbb)=c(1:NROW(bbb))

x=merge(dat,bbb,all=TRUE)
dat=x %>% arrange(date2,ranking)


##### 개봉일 기준으로 데이터 추가 자르기
ffirst=as.Date('2014-08-01')
end=as.Date('2016-06-14')
dat=dat%>%filter(release_date>=ffirst)
dat=dat%>%filter(release_date<=end)


#공휴일여부
holi = as.Date(c('2014-08-15'
                 ,'2014-09-07','2014-09-08','2014-09-09','2014-09-10','2014-10-03','2014-10-09','2014-12-25','2015-01-01'
                 ,'2015-02-18','2015-02-19','2015-02-20','2015-03-01','2015-05-05','2015-05-25','2015-06-06','2015-08-15'
                 ,'2015-09-26','2015-09-27','2015-09-28','2015-09-29','2015-10-03','2015-10-09','2015-12-25','2016-01-01'
                 ,'2016-02-07','2016-02-08','2016-02-09','2016-02-10','2016-03-01','2016-04-13','2016-05-05','2016-05-06'
                 ,'2016-05-14','2016-06-06','2016-08-15','2016-09-14','2016-09-15','2016-09-16'))
dat =  dat %>% mutate(holiday = ifelse(day_of_week == '토' | day_of_week == '일' | date2 %in% holi , 1,0))

#문화의날 여부>% arrange(date2,ranking)

culture = as.Date(c('2014-08-27','2014-09-24','2014-10-29','2014-11-26','2014-12-31','2015-01-28'
                    ,'2015-02-25','2015-03-25','2015-04-29','2015-05-27','2015-06-24','2015-07-29'
                    ,'2015-08-26','2015-09-30','2015-10-28','2015-11-25','2015-12-30','2016-01-27'
                    ,'2016-02-24','2016-03-30','2016-04-27','2016-05-25','2016-06-29','2016-07-27'
                    ,'2016-08-31','2016-09-28'))
dat =  dat %>% mutate(culture = ifelse(date2 %in% culture , 1,0))



#제작사,배급사 na처리
k=which(is.na(dat$maker))
k2=which(is.na(dat$distributor))
dat[k,18]='없음'
dat[k2,19]='없음'


#top 배급사 더미
dstr= strsplit((dat$distributor), ',' )
for (i in 1:NROW(dat)) {
  dat$sum.distr[i] <- sum(dstr[[i]] %in% c('월트디즈니컴퍼니코리아(주)','(주)쇼박스','씨제이엔엠(주)','워너브러더스 코리아(주)','이십세기폭스코리아(주)'))}

#top 제작사 더미
maker= strsplit((dat$maker), ',' )
for (i in 1:NROW(dat)) {
  dat$sum.maker[i] <- sum(maker[[i]] %in% c('유한회사 아가씨에프에스','사이드미러','(주)빅스톤픽쳐스','(주)케이퍼필름','(주)외유내강'))}



#한국 갤럽 배우 순위에 드는 배우(흥행배우 가중치 방법1)
s = strsplit((dat$actor), ',' )
#dat$s = s

for (i in 1:NROW(dat)) {
  dat$koreanactor[i] <- sum(s[[i]] %in% c('유아인','송강호','황정민','전지현','하정우','최민식','오달수','이정재','강동원','유해진','류승룡','이병헌','설경구','정우성','현빈','김수현','김혜수','장동건','송중기','김윤석','박보영','안성기','조민수','원빈','박해일','김하늘','공유','하지원','한석규','전도연','윤정희','정지훈','손예진','문근영','배용준'))}

for (i in 1:NROW(dat)) {
  dat$globalactor[i] <- sum(s[[i]] %in% c('제니퍼 로렌스','로버트 다우니 주니어','레오나르도 디카프리오','산드라 블록','덴젤 워싱턴','안젤리나 졸리','톰 행크스','조니 뎁','브래드 피트','브래들리 쿠퍼','채닝 테이텀','맷 데이먼','조지 클루니','휴 잭맨','윌 스미스','벤 에플렉','매튜 맥커너히','크리스찬 베일','톰 크루즈','에이미 아담스','크리스 햄스워스','리암 니슨','마크 월버그','쉐일린 우들리','빈 디젤','메릴 스트립','엠마 스톤','앤 해서웨이','드웨인 존슨','멜리사 맥카시','스칼렛 요한슨','다니엘 래드클리프','크리스 프랫','샤를리즈 테론','케빈 하트','제이크 질렌할','크리스 에반스'))}

dat = dat %>% mutate(num_actor = koreanactor + globalactor)



#영화의 등급
filmRatings <- as.data.frame(dat$filmRatings)
colnames(filmRatings) <- c("filmRatings")

filmRatings$filmRatings <- as.character(filmRatings$filmRatings)


filmRatings$filmRatings[filmRatings$filmRatings == "12세 미만인 자는 관람할 수 없는 등급"] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings == "12세관람가" ] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings == "12세이상관람가,12세관람가" ] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings == "12세이상관람가,국민학생관람불가" ] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings == "12세이상관람가,연소자관람가" ] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings == "12세이상관람가,연소자관람가,전체관람가" ] <- "12세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "12세이상관람가,중학생이상관람가"  ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세 미만인 자는 관람할 수 없는 등급"   ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세 미만인 자는 관람할 수 없는 등급 ,15세이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세관람가"] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세관람가,15세이상관람가"  ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세이상관람가,18세 미만인 자는 관람할 수 없는 등급" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세이상관람가,전체관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "15세이상관람가,중학생이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "18세 미만인 자는 관람할 수 없는 등급"] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "18세관람가" ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "18세관람가,청소년관람불가"] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "고등학생이상관람가"  ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "고등학생이상관람가,15세이상관람가"  ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "고등학생이상관람가,청소년관람불가" ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "국민학생관람불가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "국민학생관람불가,15세이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "국민학생관람불가,청소년관람불가" ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "기타" ] <- "미정"
filmRatings$filmRatings[filmRatings$filmRatings ==  "모든 관람객이 관람할 수 있는 등급"  ] <- "전체관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "모든 관람객이 관람할 수 있는 등급,전체관람가"] <- "전체관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "미성년자관람불가" ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "미정" ] <- "미정"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람가" ] <- "전체관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람가,15세이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람가,전체관람가" ] <- "전체관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람불가" ] <- "청소년관람불가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람불가,15세이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "연소자관람불가,청소년관람불가" ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "중학생이상관람가" ] <- "15세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "청소년관람불가,12세관람가"  ] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "청소년관람불가,15세이상관람가"] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "청소년관람불가,고등학생이상관람가"] <- "18세이상관람가"
filmRatings$filmRatings[filmRatings$filmRatings ==  "청소년관람불가,전체관람가" ] <- "18세이상관람가"
filmRatings$filmRatings[is.na(filmRatings)] <- "미정"
filmRatings$filmRatings[filmRatings$filmRatings ==  "청소년관람불가" ] <- "18세이상관람가"


r = c("전체관람가","12세이상관람가","15세이상관람가","18세이상관람가","미정")

#dd = c()
#for(h in r){
#  a=c()
#  for (x in 1:NROW(dat)) {
#    b = sum(filmRatings$filmRatings[x] %in% h)
#    a = rbind(a, b)
#  }
#  dd = cbind(dd,a)
#}

#rownames(dd) = 1:NROW(dat)
#colnames(dd) = c("전체관람가","12세이상관람가","15세이상관람가","18세이상관람가","미정")

#write.csv(dd,'dd.csv',row.names = FALSE)

dd = read.csv('dd.csv',header = TRUE)
colnames(dd) = c("전체관람가","a12세이상관람가","a15세이상관람가","a18세이상관람가","미정")
dat = cbind(dat, dd)


#장르 더미변수화 


#g = strsplit((dat$Genre), ',' )
#f = c("사극",  "액션", "어드벤처",
#      "애니메이션",  "SF",  "스릴러",
#      "드라마",  "코미디",  "멜로/로맨스",
#      "공포(호러)",  "가족",  "판타지",
#      "다큐멘터리",  "뮤지컬",  "미스터리",
#      "공연",  "전쟁",  "범죄", "기타",  "성인물(에로)",
#      "서부극(웨스턴)")


#aa = c()
#for(h in f){
#a=c()
#for (x in 1:NROW(dat)) {
#  b = sum(g[[x]] %in% h)
#  a = rbind(a, b)
#}
#  aa = cbind(aa,a)
#}
#rownames(aa) = 1:NROW(dat)
#colnames(aa) = c("사극",  "액션", "어드벤처",
#                 "애니메이션",  "SF",  "스릴러",
#                 "드라마",  "코미디",  "멜로/로맨스",
#                 "공포(호러)",  "가족",  "판타지",
#                 "다큐멘터리",  "뮤지컬",  "미스터리",
#                 "공연",  "전쟁",  "범죄", "기타",  "성인물(에로)",
#                 "서부극(웨스턴)")
#write.csv(aa,'aa.csv', row.names=FALSE)

aa = read.csv('aa.csv',header = TRUE)
colnames(aa)=c("사극",  "액션", "어드벤처",
                                "애니메이션",  "SF",  "스릴러",
                                "드라마",  "코미디",  "멜로로맨스",
                                "공포호러",  "가족",  "판타지",
                               "다큐멘터리",  "뮤지컬",  "미스터리",
                                "공연",  "전쟁",  "범죄", "기타",  "성인물에로",
                                "서부극웨스턴")

dat = cbind(dat, aa)



# 감독, 배우NA 처리

dat=dat%>%filter(!is.na(actor)|!is.na(Director))


# 개봉 몇일 째 인지: nthday

dat = dat %>% mutate(nthday = date2 - release_date + 1)
dat$nthday = as.factor(dat3$nthday)


# 시사회_누적_관객수 변수 만들고 개봉일>date2 인 애들 제거


yy=dat%>%mutate(pre_release_audience_accumulated=ifelse(release_date==date2,audience_accumulated- audience,0))

yy=yy%>%group_by(t)%>%mutate(pre_release_audience=max(pre_release_audience_accumulated))
yy=yy%>%arrange(t,nthday)

dat=yy%>%filter(date2>=release_date)


# 사용할 데이터 컬럼만..

dat2=dat[,c(3,4,10,14,16,26,28,31:35,38:65,67)]

# 겹치는 장르 : 사극 ,서부, 액션, 코미디, 멜로/로맨스, 드라마
# dat2[,c(17,18,23,24,25,37)]

dat2$sss=rowSums(dat2[,c(19,20,25:27,39)])

dat3=dat2%>%filter(sss>=1)

####################
dat3=as.data.frame(dat3)
for(i in c(5,6,7,9:40)){
  dat3[,i]=as.factor(dat3[,i])
}



# 시사회 대비 첫날 고객수 변수 생성( 입소문의 영향) : numeric

o=dat3%>%group_by(name)%>%summarise(p=min(release_date))
oo=as.data.frame(o$name)


library(doBy)
n=sampleBy(~1,frac=.8,data=oo,systematic = TRUE)

o=as.data.frame(o)
ooo=o[n,]

oo$`o$name`=as.character(oo$`o$name`)

bbb=c()
for( i in 1:NROW(oo)){
  b=table(oo$`o$name`[i]==ooo$name)
  bbb=rbind(bbb,b)
}


oo$v=bbb[,2]
oo=oo%>%mutate(v=ifelse(v>1,0,1))
colnames(oo)=c('name','v')

##### 사용할녀석들만.##########################################################

d=merge(dat3,oo,all=TRUE)
dd=d%>%arrange(name,nthday)

######

final=dd[,c(3:41,43)]

train=final%>%filter(v==1)
test=final%>%filter(v==0)

train=train[,-40]
test=test[,-40]

####################모델 여러개 만들기 #############


train_listDF = split(train, train$nthday)
test_listDF = split(test,test$nthday)


foreach(r = 1:24) %do% {
train_listDF[[r]]$same_week_table = as.numeric(train_listDF[[r]]$same_week_table)
}
  
library(RWeka)
library(foreach)
foreach ( r = 1:24) %do% {
  assign( paste0('m',r), M5P(audience ~ . -nthday , data = train_listDF[[r]]) )
}


plot(test_listDF$`1`$audience - predict(m1, test_listDF$`1`[-1]) )

require(hydroGOF)
rmse(test_listDF$`24`$audience , predict(m1, test_listDF$`24`[-1]))
rmse(test_listDF$`17`$audience , predict(m1, test_listDF$`17`[-1]))


# 데이터 불러와서 예측하기 
M7 = read.csv('abcde.csv',header = T)
M7$month=substr(M7$date2,6,7)
M7$month=as.factor(M7$month)

M77= M7[,c(3:40)]


for(i in c(2:4,6:36,38)){
  M77[,i]=as.factor(M77[,i])
}

for(i in c(1,5,37)){
  M77[,i]=as.numeric(M77[,i])
}

M77$same_week_table = as.numeric(M77$same_week_table)

predict(m1, M77[1,])

fitted(m1)

str(M77[1,])


str(train_listDF[[1]])
