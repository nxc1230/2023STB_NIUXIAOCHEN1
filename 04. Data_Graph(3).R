# table()함수/구분 1개의 인자를 가지고 도수분포표 작성
table(X2023_STB_survey$Gender)

# table()함수/2개의 인자를 가지고 교차표를 작성
table(X2023_STB_survey$Gender,X2023_STB_survey$Age)
table(X2023_STB_survey$Gender,X2023_STB_survey$Grade)

#막대그래프
barplot(table(X2023_STB_survey$Gender))
barplot(table(X2023_STB_survey$Age))
barplot(table(X2023_STB_survey$Grade))
barplot(table(X2023_STB_survey$Nationality))
barplot(table(X2023_STB_survey$`residential area`))

entry <- table(X2023_STB_survey$Gender,X2023_STB_survey$Grade)
barplot(entry, legend = TRUE)

#  상대도수 계산
ECN <- table(X2023_STB_survey$Gender)
prop.table(ECN)

#파이차트 
pie(table(X2023_STB_survey$Gender))
pie(table(X2023_STB_survey$Age))
pie(table(X2023_STB_survey$Grade))
pie(table(X2023_STB_survey$Nationality))
pie(table(X2023_STB_survey$`residential area`))
#Age and grade
boxplot(X2023_STB_survey$Age,  main="Age", col="yellow")
boxplot(X2023_STB_survey$Grade,  main="Grade", col="green")

# Grade를 X값으로 Age를 Y값
plot(x=X2023_STB_survey$"Grade",y=X2023_STB_survey$"Age",  xlab="Grade", ylab="Age",  main="Grade and Age")

