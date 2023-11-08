
#전처리 작업
installed.packages("dplyr")
installed.packages("ggplot2")
library(dplyr)
library(ggplot2)

#csv 형식의 파일을 불러와서 subway객체에 입력하고 구조확인
str(congestion)

#변수의이상치와결측치확인하고처리
summary(congestion)

#결측치개수확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가있는행을제거한새로운데이터프레임생성
#6시 출발기차의 결측치를제거
congestion1 <-congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))

#23시30분출발기차의결측치를제거
congestion1 <-congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))

#다음으로남은시간대는결측치를0으로대체합니다.
#남은결측치를0으로대체
congestion1[is.na(congestion1)] <-0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1,aes(y=s0530))+
  geom_boxplot()

summary(congestion1$s0530)

#1. 지하철역의 하루평균 혼잡도
congestion1$day_mean <-
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#2. 지하철 호선별 하루 평균 혼잡도
mean_day_congestion <- congestion1 %>%
  group_by(line) %>%
  summarize(mean_day_congestion = mean(day_mean))

print(mean_day_congestion)

#3. 지하철 호선별 출근시간(07:00~09:00)대의 평균 혼잡도
# 07:00~09:00 대의 열을 선택
mean_day_on <- congestion1 %>%
  select(station, line, s0700, s0730, s0800, s0830)

# 호선 및 방향으로 그룹화하고, 출근 시간대의 평균 혼잡도를 계산합니다.

mean_day_onavg <- congestion1 %>%
  group_by(line) %>%
  summarise(
    s0700_mean = mean(s0700),
    s0730_mean = mean(s0730),
    s0800_mean = mean(s0800),
    s0830_mean = mean(s0830)
  )
# 결과를 출력합니다.
print(mean_day_onavg)

#3-1. 기초통계분석 
summary_stats <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_s0700 = mean(s0700),
    mean_s0730 = mean(s0730),
    mean_s0800 = mean(s0800),
    mean_s0830 = mean(s0830),
    median_s0700 = median(s0700),
    median_s0730 = median(s0730),
    median_s0800 = median(s0800),
    median_s0830 = median(s0830),
    max_s0700 = max(s0700),
    max_s0730 = max(s0730),
    max_s0800 = max(s0800),
    max_s0830 = max(s0830),
    min_s0700 = min(s0700),
    min_s0730 = min(s0730),
    min_s0800 = min(s0800),
    min_s0830 = min(s0830),
    sd_s0700 = sd(s0700),
    sd_s0730 = sd(s0730),
    sd_s0800 = sd(s0800),
    sd_s0830 = sd(s0830)
  )

# 결과를 출력합니다.
print(summary_stats)

#3-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
mean_day_onavg$max_on <-apply(mean_day_onavg,1,max)

# MaxCommute의 최대값을 가진 열의 이름을 새로운 열로 추가
mean_day_onavg <- mean_day_onavg %>%
  mutate(MaxCommute_Column = colnames(mean_day_onavg)[max.col(select(mean_day_onavg, starts_with("s0")))])

#막대그래프 만들기  
ggplot(mean_day_onavg, aes(x = as.factor(line), y = max_on)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "각 호선별로 평균 혼잡도가 가장 높은 시간대(출근시간)", x = "호선", y = "평균 혼잡도") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  geom_text(aes(label = MaxCommute_Column), vjust = -0.5, color = "black", size = 3)
str(mean_day_onavg)



#3-3. 평균혼잡도 상위4개 호선의 역별 기여도
mean_dayon_congestion <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_congestion = mean(s0700 + s0730 + s0800 + s0830)
  ) %>%
  arrange(desc(mean_congestion))

# 평균 혼잡도가 가장 높은 4개 호선을 선택합니다.
top_4_lines <- mean_dayon_congestion %>% top_n(4, mean_congestion)

# 선택된 4개 호선에서 역별로 혼잡도의 기여도를 계산합니다.
contribution_by_station <- congestion1 %>%
  filter(line %in% top_4_lines$line) %>%
  select(station, line, s0700, s0730, s0800, s0830) %>%
  group_by(station,line) %>%
  summarise(
    total_congestion = sum(s0700 + s0730 + s0800 + s0830),
    contribution_percentage = total_congestion / sum(total_congestion) * 100
  ) 

# 결과를 출력합니다.
print(contribution_by_station)


#4. 08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>% 
  mutate(s08_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>% 
  group_by(line,s08_grade) %>% summarise(n=n())%>% 
  group_by(line) %>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s08_grade =="caution")%>%
  select(line,s08_grade,n,pct)%>% 
  arrange(desc(n))
head(5)

#5. 지하철 호선별 퇴근시간(18:00~20:00)대의 평균 혼잡도
# 07:00~09:00 대의 열을 선택
mean_day_off <- congestion1 %>%
  select(station, line, s1800, s1830, s1900, s1930)

# 호선 및 방향으로 그룹화하고, 퇴근 시간대의 평균 혼잡도를 계산합니다.
mean_day_offavg <- congestion1 %>%
  group_by(line) %>%
  summarise(
    avgs1800_maen = mean(s1800),
    avgs1830_mean = mean(s1830),
    avgs1900_mean = mean(s1900),
    avgs1930_mean = mean(s1930)
  )
# 결과를 출력합니다.
print(mean_day_offavg)

#5-1. 기술통계분석 결과 
summary_stats2 <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean_s1800 = mean(s1800),
    mean_s1830 = mean(s1830),
    mean_s1900 = mean(s1900),
    mean_s1930 = mean(s1930),
    median_s1800 = median(s1800),
    median_s1830 = median(s1830),
    median_s1900 = median(s1900),
    median_s1930 = median(s1930),
    max_s1800 = max(s1800),
    max_s1830 = max(s1830),
    max_s1900 = max(s1900),
    max_s1930 = max(s1930),
    min_s1800 = min(s1800),
    min_s1830 = min(s1830),
    min_s1900 = min(s1900),
    min_s1930 = min(s1930),
    sd_s1800 = sd(s1800),
    sd_s1830 = sd(s1830),
    sd_s1900 = sd(s1900),
    sd_s1930 = sd(s1930)
  )

# 결과를 출력합니다.
print(summary_stats2)

#5-2. 막대그래프로 그리기

mean_day_offavg$max_off <-apply(mean_day_offavg,1,max)

# MaxCommute의 최대값을 가진 열의 이름을 새로운 열로 추가
mean_day_offavg <- mean_day_offavg %>%
  mutate(MaxCommute_Column1 = colnames(mean_day_offavg)[max.col(select(mean_day_offavg, starts_with(" ")))])

#막대그래프 만들기  
ggplot(mean_day_offavg, aes(x = as.factor(line), y = max_off)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "각 호선별로 평균 혼잡도가 가장 높은 시간대(퇴근시간)", x = "호선", y = "평균 혼잡도") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  geom_text(aes(label = MaxCommute_Column1), vjust = -0.5, color = "black", size = 3)
str(mean_day_offavg)


#5-3. 평균 혼잡도 상위 4개 호선의 역별 기여도
mean_dayoff_congestion <- congestion1 %>%
  group_by(station,line) %>%
  summarise(
    mean_congestion = mean(s1800 + s1830 + s1900 + s1930)
  ) %>%
  arrange(desc(mean_congestion))

# 평균 혼잡도가 가장 높은 4개 호선을 선택합니다.
top_4_lines2 <- mean_dayoff_congestion %>% top_n(4, mean_congestion)

# 선택된 4개 호선에서 역별로 혼잡도의 기여도를 계산합니다.
contribution_by_station2 <- congestion1 %>%
  filter(line %in% top_4_lines2$line) %>%
  select(station, line, s1800, s1830, s1900, s1930) %>%
  group_by(station,line) %>%
  summarise(
    total_congestion = sum(s1800 + s1830 + s1900 + s1930),
    contribution_percentage = total_congestion / sum(total_congestion) * 100
  )

# 결과를 출력합니다.
print(contribution_by_station2)

#6. 출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>% 
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>% 
  group_by(line,s18_grade) %>% summarise(n=n())%>% 
  group_by(line) %>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s18_grade =="bad")%>%
  select(line,s18_grade,n,pct)%>% 
  arrange(desc(n))
  head(5)

