# table()함수/구분 1개의 인자를 가지고 도수분포표 작성
table(X2023_STB_survey $Gender)

#  상대도수 계산
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)

# table()함수/2개의 인자를 가지고 교차표를 작성
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

#막대그래프
barplot(table(X2023_STB_survey $Nationality))


#차트회전
barplot(table(X2023_STB_survey $`residential area`), horiz=TRUE)

#막대그래프
barplot(table(X2023_STB_survey $Gender, X2023_STB_survey $Grade))

#파이차트
pie(table(X2023_STB_survey $Grade))


#히스토그램
hist(X2023_STB_survey $`Age`,main="Age",col=terrain.colors(12))

#박스플롯비교
boxplot(grade_age$`Grade2`, grade_age$`Grade3`, grade_age$`Grade4`,main="Grade별Age비교", col="yellow", names = c("Grade2","Grade3","Grade4"))

#산점도 작성.
plot(x=X2023_STB_survey$`Grade`, y=X2023_STB_survey$`Age`, xlab="Grade", ylab="Age")
