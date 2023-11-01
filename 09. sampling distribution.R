salary=c(45,50,50,55,55,60,60,65,70,90) #연봉데이터
com2=combn(salary,2) #연봉데이터에서 2개씩 선택해서 데이터로 생성, com2에 저장
print(com2)

com2t=t(com2)#데이터의 전치치
z=rowMeans(com2t)#전치된 데이터의 행평균 계산
barplot(table(z),xlab="연봉평균",ylab="도수",main="연봉평균의 분포")

meanofz=mean(z) #z의 평균
print(meanofz)

data50=c(1:50)#데이터
com2=combn(data50,2)#데이터에서 2개씩 선택하여 데이터 생성
com2t=t(com2)#데이터의 전치
z2=rowMeans(com2t)#전치된 데이터의 행평균 계산
meanofz2=mean(z2)#기댓값을 계산하여 저장
print(meanofz2)
barplot(table(z2),xlab="평균2",ylab="도수",main="표본크기2의 도수분포포")

com3=combn(data50,3)#데이터에서 3개씩 선택하여 데이터 생성
com3t=t(com3)#데이터의 전치
z3=rowMeans(com3t)#전치된 데이터의 행평균 계산
meanofz3=mean(z3)#기댓값을 계산하여 저장
print(meanofz3)
barplot(table(z3),xlab="평균3",ylab="도수",main="표본크기3의 도수분포포")

com5=combn(data50,5)#데이터에서 5개씩 선택하여 데이터 생성
com5t=t(com5)#데이터의 전치
z5=rowMeans(com5t)#전치된 데이터의 행평균 계산
meanofz5=mean(z5)#기댓값을 계산하여 저장
print(meanofz5)
barplot(table(z5),xlab="평균3",ylab="도수",main="표본크기5의 도수분포포")
sampling=c(1:100)
print(sampling)
sample(x=sampling, size=10)

install.packages("sampling")
library(sampling)
sam2=strata(data=iris,stratanames=c("Species"),size=c(3,3,3),method='srswor') #iris데이터 사용하여 표본추출, Species층을 기준으로
sample2=getdata(data=iris,m=sam2) #추출된데이터 저장
print(sample2)