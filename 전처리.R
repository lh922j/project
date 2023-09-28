#1.	 성별에 따른 급여 차이가 있는지 분석하시오
custom <- read.csv("custom.csv")
custom
# A.	변수 전처리
library(dplyr)
summary(custom)
table(custom$sex)
summary(custom$salary)
table(custom$salary)
custom <- custom %>% filter(!is.na(salary)) 
custom  
table(is.na(custom$salary))
# B.	성별 급여를 확인할 수 있는 비교표와 그래프 만들기
custom$sex <- ifelse(custom$sex == 1, "male","female") 

sex_salary <- custom %>% 
  group_by(sex) %>% 
  summarise(mean_salary = mean(salary))

sex_salary

install.packages("ggplot2")
library(ggplot2)
ggplot(data = sex_salary, aes(x = sex, y = mean_salary)) + geom_col()
##################################################################################
#2.	연령대별 급여 차이가 있는지 분석하시오.
#A.	변수 전처리
custom$birth
custom <- custom %>%
  mutate(birth = ifelse(birth > 1980, "young",
                         ifelse(birth >= 1950,
                                "middle",
                                "old")))
custom
table(custom$birth)
#B.	연령대별 급여를 확인할 수 있는 비교표와 그래프 만들기 
birth_salary <- custom %>% 
  filter(!is.na(salary)) %>%
  group_by(birth) %>% 
  summarise(mean_salary = mean(salary))
birth_salary
ggplot(data = birth_salary, aes(x = birth, y = mean_salary)) + geom_col()
#이 자료에서는 청년층은 205만원, 중년층은 275만원 노년층은 78.1만원으로 노년층<청년층<중년층의 순서로 급여 차이가 있다.

##########################################################################
#3.	연령대별 성별 급여 차이가 있는지 분석하시오
#A.	변수 전처리
table(custom$birth)

#B.	연령대별 성별 급여 비교표와 그래프 만들기
sex_salary <- custom %>%
  group_by(birth, sex) %>% 
  summarise(mean_salary = mean(salary))
sex_salary
ggplot(data = sex_salary, aes(x = birth, y = mean_salary, fill = sex)) + geom_col(position = "dodge")


#############################################################################
#4.	어느 직업의 급여가 많은지 알아보시오
#A.	변수 전처리
table(custom$jobcode)
library(readxl)
job_code <- read_excel("job_code.xlsx")
job_code
job_list <- left_join(custom , job_code, id="jobcode")
job_list %>% 
  filter(!is.na(jobcode)) %>% 
  select(jobcode, job) %>% 
  head(5)
#B.	직업별 급여 비교표와 그래프 만들기( 여러 관점으로 자유롭게 분석해 보기 )
job_salary <- job_list %>%
  group_by(job) %>% 
  summarise(mean_salary = mean(salary))
head(job_salary)  
top7 <- job_salary %>% 
  arrange(desc(mean_salary)) %>% 
  head(7)
top7

ggplot(data = top7, aes(x = reorder(job, mean_salary), y = mean_salary)) + geom_col() + coord_flip()
#C.	분석결과에 따른 insight 요약 정리
#이 자료에서는 금속 재료 공학 기술자 및 시험원이 845만원으로 가장 많은 급여를 받고 있고 의료진료 전문가가 844만원으로 그 뒤를 잇고 있다. 그 다음으로 의회의원 고위공무원 및 공공단체임원, 보험 및 금융 관리자 등등으로 순위를 나열할 수 있다.

bottom7 <- job_salary %>% 
  arrange(mean_salary) %>% 
  head(7)
bottom7

ggplot(data = bottom7, aes(x = reorder(job, -mean_salary), y = mean_salary)) + geom_col() + coord_flip() + ylim(0, 850)

#이 자료에서는 가사 및 육아 도우미가 80만원으로 가장 낮은 급여를 받고 있고 임업관련 종사자가 83만원으로 그 뒤를 잇고 있다. 그 다음으로는 기타 서비스관련 단순 종사원, 청소원 및 환경 미화원, 약사 및 한약사 등등으로 순위를 나열할 수 있다. 가장 많은 급여를 받고 있는 금속 재료 공학 기술자 및 시험원과 가장 낮은 급여를 받고 있는 가사 및 육아 도우미의 평균 급여 차이는 약 765만원정도 차이가 나고 있다.

###############################################################
#5.	종교유무에 따른 이혼율을 분석하시오
#A.	변수 전처리
table(custom$religion)
custom$religion <- ifelse(custom$religion == 1, "Y", "N")
table(custom$religion)

table(custom$marriage)
custom$marriage <- ifelse(custom$marriage == 1, "marriage",
                          ifelse(custom$marriage == 3, "divorce", NA))
table(is.na(custom$marriage))

#B.	종교유무에 따른 이혼율을 비교표와 그래프 만들기( 여러 관점으로 자유롭게 분석해 보기 )
religion_marriage <- custom %>% 
  filter(!is.na(marriage)) %>% 
  group_by(religion, marriage) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n / total*100, 1))
religion_marriage  

divorce <- religion_marriage %>% 
  filter(marriage == "divorce") %>% 
  select(religion, pct)
divorce
ggplot(data = divorce, aes(x = religion, y= pct)) +geom_col()  
#C.	분석결과에 따른 insight 요약 정리
#이 자료에서는 종교의 유무에 따라 이혼율을 나타내었을 때 종교가 있을 경우 7.2% 종교가 없을 경우 8.3%로 종교가 없는 사람들이 이혼을 더 많이 한다고 볼 수 있다.


#######################################################################
#6.	지역별 연령대 분포를 요약 정리하시오
#A.	변수 전처리
table(custom$region)
list_region <- data.frame(region = c(1:7),
                          list = c ("서울",
                                      "수도권(인천/경기)",
                                      "부산/경남/울산",
                                      "대구/경북",
                                      "대전/충남",
                                      "강원/충북",
                                      "광주/전남/전북/제주도"))
list_region
custom
custom <- left_join(custom, list_region, id = "region")
region_birth <- custom %>%
  group_by(list, birth) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n / total * 100, 2))
region_birth
head(region_birth)
ggplot(data = region_birth, aes(x = list, y = pct, fill = birth)) + geom_col() + coord_flip()
#B.	분석결과에 따른 insight 요약 정리
#이 자료에서 청년층(young)비율은 높은순으로 서울,대전/충남, 부산/경남/울산 등등으로  형성되어있고 중년층(middle)비율은 부산/경남/울산, 대전/충남 등등 으로 노년층(old)비율은 강원/충북,대구/경북, 광주/전남/전북/제주도 등등으로 순위가 형성되어있습니다.
###############################################################################################
#7.	교육정도에 따른 이혼율을 분석하시오
#A.	변수 전처리
table(custom$educate)
custom$educate <- ifelse(custom$educate == 0, "미취학",
                          ifelse(custom$educate == 1, "초졸",
                                 ifelse(custom$educate == 2, "중졸",
                                        ifelse(custom$educate == 3, "고졸",
                                               ifelse(custom$educate == 4, "전문대졸",
                                                      ifelse(custom$educate == 5, "대졸이상",
                                                             "기타"))))))
custom
custom$educate
head(custom$educate)
table(is.na(custom$educate))

#B.	교육정도에 따른 이혼율 비교표와 그래프 만들기( 여러 관점으로 자유롭게 분석해 보기 )
edu_marriage <- custom %>%
   filter(!is.na(marriage)) %>% 
   group_by(educate, marriage) %>% 
   summarise(n = n()) %>% 
   mutate(total = sum(n)) %>% 
   mutate(pct = round(n / total * 100, 1))
edu_marriage

divorce1 <- edu_marriage %>% 
  filter(marriage == "divorce") %>% 
  select(educate, pct)
divorce1
ggplot(data = divorce1, aes(x = educate, y = pct)) + geom_col()

#C.	분석결과에 따른 insight 요약 정리
#이 자료에선 교육정도가 고졸인 경우 이혼율이 100%이다.  

##################################################################################
#8.	자신만의 분석 주제를 가지고 분석해 보기
#주거환경에 따른 급여차이 분석하기
# 변수 전처리
table(custom$house_type)
custom$house_type <- ifelse(custom$house_type == 0, "해당사항없음",
                        ifelse(custom$house_type == 1, "다가구주택",
                               ifelse(custom$house_type == 2, "연립주택",
                                      ifelse(custom$house_type == 3, "오피스텔",
                                             ifelse(custom$house_type == 4, "다세대주택",
                                                    ifelse(custom$house_type == 5, "아파트",
                                                           "기타"))))))
custom
#주거환경에 따른 급여차이를 비교표와 그래프 만들기
ht_salary <- custom %>% 
  filter(!is.na(salary)) %>%
  group_by(house_type) %>% 
  summarise(mean_salary = mean(salary))
ht_salary
ggplot(data = ht_salary, aes(x = house_type, y = mean_salary)) + geom_col()


#분석결과에 따른 insight 요약 정리
#이 자료를 분석했을때 다가구주택의 경우 273만원, 다세대주택 139만원, 아파트 201만원, 연립주택 94.9만원, 오피스텔 174만원으로, 연립주택<다세대주택<오피스텔<아파트<다가구주택 순으로 급여차이가 났습니다.



