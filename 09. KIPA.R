#.패키지 설치 및 구동 
library(dplyr) 
library(ggplot2)

install.packages("foreign") 
library(foreign)

mental <- read.spss("KIPA_DATA_2022.SAV")

class(mental)
mental <- as.data.frame(mental) 
class(mental)

#.변수 추출 후 이름 변경
mental <-mental %>% 
  select(q32_2,q1_4,q32_1,q34_1,q55,d17,d1,d2,ara)%>% 
  rename(suicide=q32_2, satisfaction=q1_4, loneliness=q32_1, family_belief=q34_1, wealth=q55, health=d17, sex=d1, age=d2, area=ara)

#변수가 어떤 유형인지 확인합니다. 
str(mental)

table(mental$suicide)
table(mental$health)
table(mental$satisfaction)

#분석을 위해서 범주형 데이터를 모두 정수형으로 변환합니다. 
mental$suicide <- as.integer(mental$suicide) 
mental$satisfaction <- as.integer(mental$satisfaction) 
mental$loneliness <- as.integer(mental$loneliness) 
mental$family_belief <- as.integer(mental$family_belief) 
mental$wealth <- as.integer(mental$wealth) 
mental$health <- as.integer(mental$health)

#변경된 변수를 확인합니다. 
table(mental$suicide) 
table(mental$health) 
table(mental$wealth) 
table(mental$satisfaction)

#.변수 정리 
mental$satisfaction<- 
  mental$satisfaction-1 
mental$wealth<- mental$wealth-1

table(mental$wealth) 
table(mental$satisfaction)

#변경한 3개 변수의 빈도를 확인합니다. 
table(mental$sex) 
table(mental$age) 
table(mental$area)

mental$age<- ifelse(mental$age=="19~29세"," 20대",mental$age)
table(mental$age)

#.결측치와 이상치 확인 
summary(mental)

#1.(빈도 분석) 성별
mental%>%
  group_by(sex)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),
         pct=round(n/total*100,1))

#2.(빈도 분석) 연령대 
mental%>%
  group_by(age)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),
         pct=round(n/total*100,1))
#3.(교차 분석) 성별과 연령대의 교차분석 – chisq.test 
table(mental$sex, mental$age)
round(prop.table(table(mental$ sex,  mental$age),1)*100,1)
chisq.test(mental$ sex,  mental$age)

#4.(평균 분석) 6개 분석 변수의 평균 분석 
mental%>%
  summarise(m1=mean(suicide),m2=mean(satisfaction),m3=mean(loneliness),m4=mean(family_belief),m5=mean(health))

#5.(회귀 분석)삶의 만족도와 외로움이 자살충동에 미치는 영향 
RA<-lm(data=mental,suicide~satisfaction+loneliness)
summary(RA)

#6.(상관 분석) 삶의 만족도와 외로움의 상관관계 
cor.test(mental$satisfaction,mental$loneliness)

#7.(회귀 분석) 가족신뢰도, 경제안정도, 건강상태가 삶의 만족도에 미치는 영향
install.packages("ztable")
library(ztable)
RA <-lm(data=mental,satisfaction~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)

#8.(회귀 분석) 가족신뢰도, 경제안정도, 건강상태가 외로움에 미치는 영향
RA <-lm(data=mental,loneliness~family_belief+wealth+health)
summary(RA)
options(ztable.type="viewer")
ztable(RA)

#9.(독립표본t검정) 성별 삶의 만족도 차이 
t.test(data=mental,satisfaction~sex)

#10.(평균 분석) 연령대별 삶의 만족도 차이 
mental%>%
  group_by(age)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))

#11.(평균 분석-그래프 작성) 지역별 삶의 만족도 분석과 그래프 그리기
area_satisfaction <-mental%>%
  group_by(area)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))

ggplot(data=area_satisfaction,aes(x=reorder(area,m),y=m))+
  geom_col()+
  ggtitle("지역별 만족도")+
  xlab("지역")+
  ylab("만족도")+
  coord_flip() 
