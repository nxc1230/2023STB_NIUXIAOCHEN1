#.패키지 설치 및 구동
library(dplyr)
library(ggplot2)
library(foreign)

#데이터를 R로 불러 옵시다
koweps23 <- read.spss("Koweps_h17_2022_beta1.sav")
koweps23 <- as.data.frame(koweps23)

#.변수 추출 후 이름 변경
welfare23 <-koweps23 %>%  
  select(h1701_4,h1701_5,h1701_6,h17_reg5,h1708_114,h1708_122)

welfare23 <-welfare23 %>%  
  rename(sex=h1701_4,         
         birth=h1701_5,         
         edu=h1701_6,         
         reg=h17_reg5,         
         p_salary=h1708_114,        
         t_salary=h1708_122)

str(welfare23)

# 결측치와 이상치 확인
summary(welfare23)
table(is.na(welfare23$p_salary))
welfare23$p_salary<-ifelse(welfare23$p_salary==0,NA,welfare23$p_salary)
table(is.na(welfare23$p_salary))

table(is.na(welfare23$p_salary))
welfare23$p_salary<-ifelse(welfare23$p_salary==64800,NA,welfare23$p_salary)
table(is.na(welfare23$p_salary))

#변수 기초 정리 
#sex 
table(welfare23$sex)
welfare23$sex<-ifelse(welfare23$sex==1, "male","female")
table(welfare23$sex)

#age
welfare23$age<-2023-welfare23$birth+1
range(welfare23$age)

#edu
welfare23$edu_grade<-ifelse(welfare23$edu%in%c(1,2,3,4),"중학이하",                        ifelse(welfare23$edu==5, "고교",                               ifelse(welfare23$edu==6,"전문대","대학 이상")))
table(welfare23$edu_grade)

#총 9가지 항목을 분석
#1.상용직과 일용직의 평균 총급여 비교
mean(welfare23$p_salary,na.rm=T)
mean(welfare23$t_salary,na.rm=T)

#2.성별 평균 총급여 차이 검정
t.test(data=welfare23,p_salary~sex)

#3.성별 최대 총급여 상용직 근로자 찾기
welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(sex)%>%
  filter(p_salary==max(p_salary))%>%
  select(sex,age,edu,edu_grade,reg,p_salary)

#4.연령별 평균 총급여
age_salary1<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(age)%>%
  summarise(m=mean(p_salary))

age_salary1%>%
  arrange(desc(m))%>%
  head(3)

#5.연령별 평균 총급여 그래프 작성
ggplot(data=age_salary1,aes(x=age,y=m))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

#6.연령별 남녀 평균 총급여 그래프 작성
age_salary2<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(age,sex)%>%
  summarise(m=mean(p_salary))

ggplot(data=age_salary2,aes(x=age,y=m,col=sex))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

#7.교육수준별 상용직 평균 총급여 비교
edu_salary1<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(edu_grade)%>%
  summarise(m=mean(p_salary))

edu_salary1%>%
  arrange(desc(m))

ggplot(data=edu_salary1, aes(x=reorder(edu_grade,m),y=m))+
  geom_col()

#8.상용직 근로자의 교육수준과 성별 총급여 분석
edu_salary2<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(edu_grade,sex)%>%
  summarise(m=mean(p_salary))

edu_salary2%>%
  arrange(desc(m))

ggplot(data=edu_salary2,aes (x=edu_grade,y=m,fill=sex))+ 
  geom_col(position="dodge")+ 
  scale_x_discrete(limits=c("중학이하", "고교", "전문대" ,"대학이상"))

#9.권역별 평균 총급여 비교
region_salary<-welfare23%>%
  filter(!is.na(p_salary))%>%
  group_by(reg)%>%
  summarise(m=mean(p_salary))

region_salary%>%
  arrange (desc(m))

ggplot(data=region_salary, aes (x=reorder(reg,-m),y=m))+
  geom_col()