#실습에 필요한 packages를 라이브러리에 등록 
library(dplyr) 
library(ggplot2)

#CSV형식의 파일 불러와서 subway객체에 입력하고 구조 확인 
str(congestion)

#변수의 이상치와 결측치 확인하고 처리 
summary(congestion)

#결측치 개수 확인 
is.na(congestion) 
sum(is.na(congestion)) 
colSums(is.na(congestion))

#결측치 개수 확인 
is.na(congestion) 
sum(is.na(congestion)) 
colSums(is.na(congestion)) 

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성 #6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),] 
colSums(is.na(congestion1))

#23시 30분 출발기차의 결측치를 제거 
congestion1 <- congestion1[!is.na(congestion1$s2330),] 
colSums(is.na(congestion1))

#남은 결측치를 0으로 대체 
congestion1[is.na(congestion1)] <- 0 
colSums(is.na(congestion1))

#이상치 확인 
ggplot(congestion1, aes(y=s0530))+  
  geom_boxplot()
summary(congestion1$s0530)

#1.지하철역의 하루 평균 혼잡도 
congestion1$day_mean <- 
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#2.지하철 호선별 하루 평균혼잡도
congestion%>% group_by(line) %>%  
  summarise(total=sum(line)) %>% 
  arrange(desc(total)) %>% 
  head(10)

#3.지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도
#3-1. 호선별로 07시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s07_grade=ifelse(s0700<=80, "good", ifelse(s0700<=130, "normal", ifelse(s0700<=150, "caution",  "bad"))))%>%  
  group_by(line, s07_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s07_grade=="caution")%>%  
  select(line, s07_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)
#3-2. 호선별로 08시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s08_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution",  "bad"))))%>%  
  group_by(line, s08_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s08_grade=="caution")%>%  
  select(line, s08_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)
ggplot(data=congestion1,aes(x=reorder(congestion1, n),  y=pct))+ geom_col()+ coord_flip()
#3-3. 호선별로 09시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s09_grade=ifelse(s0900<=80, "good", ifelse(s0900<=130, "normal", ifelse(s0900<=150, "caution",  "bad"))))%>%  
  group_by(line, s09_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s09_grade=="caution")%>%  
  select(line, s09_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)

#4.출발시간 8시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%  
  mutate(s08_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution",  "bad"))))%>%  
  group_by(line,s80_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s80_grade,n,pct)%>%  
  arrange(desc(n))

#5.지하철 호선별 퇴근시간(18:00~20:00)대의 평균혼잡도
#5-1. 호선별로 18시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution",  "bad"))))%>%  
  group_by(line,s18_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s18_grade,n,pct)%>%  
  arrange(desc(n))
congestion1 %>%  
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution",  "bad"))))%>%  
  group_by(line, s18_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s18_grade=="caution")%>%  
  select(line, s18_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)

#5-2. 호선별로 19시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s19_grade=ifelse(s1900<=80, "good", ifelse(s1900<=130, "normal", ifelse(s1900<=150, "caution",  "bad"))))%>%  
  group_by(line,s19_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s19_grade,n,pct)%>%  
  arrange(desc(n))
congestion1 %>%  
  mutate(s19_grade=ifelse(s1900<=80, "good", ifelse(s1900<=130, "normal", ifelse(s1900<=150, "caution",  "bad"))))%>%  
  group_by(line, s19_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s19_grade=="caution")%>%  
  select(line, s19_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)
#5-3. 호선별로 20시 지하철 혼잡도 범주화 
congestion1 %>%  
  mutate(s20_grade=ifelse(s2000<=80, "good", ifelse(s2000<=130, "normal", ifelse(s2000<=150, "caution",  "bad"))))%>%  
  group_by(line,s20_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s20_grade,n,pct)%>%  
  arrange(desc(n))
congestion1 %>%  
  mutate(s20_grade=ifelse(s2000<=80, "good", ifelse(s2000<=130, "normal", ifelse(s2000<=150, "caution",  "bad"))))%>%  
  group_by(line, s20_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s20_grade=="caution")%>%  
  select(line, s20_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(4)
#6.출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%  
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution",  "bad"))))%>%  
  group_by(s18_grade) %>%  
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s18_grade,n,pct)%>%  
  arrange(desc(n))
