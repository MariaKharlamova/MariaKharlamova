
# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

library("sandwich")
library("memisc")# удобное сравнение двух и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof") # множественная проверка гипотез
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car") #проверка линейных ограничений (гипотезы о равенстве коэффициентов)
library("hexbin") # графики
library("devtools") # установка пакетов неофициального репозитория
devtools::install_github("bdemeshev/rlms") #установим пакет для обрабокти данных rlms с github
library("rlms") # загрузка данных в формате rlms (spss)

h <- rlms_read("r28hall32.sav")
individuals <- rlms_read("r28h_os32.sav")
h_2018 <- rlms_read("r27i_os_32.sav")
h_2019 <- rlms_read("r28i_os_32.sav")
describe(individuals)

# отбор данных (переменных)

h_2019_select <- select(h_2019, xredid_i, xj322, xj72.171, xj72.172, xj72.173, xh5,  xm1, xm2, xm2.1,  xi1, xi4, status, region, x_occup08,
             x_educ, x_diplom, xj72.18a, x_age,  xj81,  xj90, xj161.3y, xj1.1.1, xj2cod08,
             xj4.1, xj6.1a, xj6.2, xj10, xj11.1,   xj14, xj21a, xj32,  
               xj60, xj60.5a, xj65, xj72.19, xj72.18, xj206, xl5.0, xm3,
             xm71, xm80.0)

describe(h_2019_select)
###  
###

# можно переименовать переменные для большей наглядности
h_2019_main <- rename(h_2019_select, id=xredid_i, marital_status=xj322, children=xj72.171, n_children=xj72.172, n_underage=xj72.173, 
             sex=xh5, weight=xm1, height=xm2, weight_changed=xm2.1,   b_settl_shift=xi1, nationality=xi4, 
             settl_type=status, region=region, prof_group=x_occup08, edu=x_educ, degree=x_diplom, 
             max_degree=xj72.18a , age = x_age,  want_change_job=xj81, 
              main_occupation=xj90, off_work_experience=xj161.3y, work_satisf=xj1.1.1, occupation_code=xj2cod08,
             work_industry1=xj4.1, working_hours_per_day1=xj6.1a, working_hours_per_week1=xj6.2,
             salary_main=xj10, off_employment1=xj11.1,  
             wages_non_payment=xj14, vacation=xj21a, moonlighting_job=xj32, 
               tot_incom=xj60, 
             fin_situation_dynam=xj60.5a, life_satisf=xj65, confession=xj72.19, belief=xj72.18, trust= xj206, health_doctor=xl5.0,
             health_state=xm3, smoke=xm71, drink=xm80.0)
#library(Hmisc)
#impute(h_2019_main$n_children, 0)
#impute(h_2019_main$n_underage, 0)
h_2019_main$n_children[is.na(h_2019_main$n_children)] <- 0
h_2019_main$n_underage[is.na(h_2019_main$n_underage)] <- 0
colSums(is.na(h_2019_main))

h_2019_main <- na.omit(h_2019_main)
describe(h_non_NA)
h_2019_main$children


# от переменной год рождения перейти к переменной возраст
h3 <- mutate(h3, vozrast = 2013 - b_year)

# кроме отбора переменных, может понадобиться отбор наблюдений

#Отобрать только два типа населённого пункта:
#город и областной центр (должно остаться 10692 записи).
#город=2,областной центр=1

h4 <- filter(h3, from <= "2")

# в задании указано, что пропуски нужно оставить! это было задание на размышленние (то есть на гугление и использование справки R)
#Отобрать только две категории степени удовлетворенности работой в целом:
#полностью удовлетворен и скорее удовлетворен(должно остаться 9089 записей).
#1--полностью удовлетворен,2-скорее удовлетворен
h5 <- filter(h4, satisf <= "2"|is.na(satisf))

#отобрать категории образования:
#Категория 1: окончил 0 - 6 классов;
#Категория 2: незаконченное среднее образование (7 - 8 кл);
#Категория 3: незаконченное среднее образование (7 - 8 кл) + что-то еще;
#Категория 4: законченное среднее образование;
#Категория 5: законченное среднее специальное образование;
#Категория 6: законченное высшее образование и выше;
h6 <- filter(h5, degree <= "6"|is.na(degree))

#Из переменной тип населенного пункта сделать дамми-переменную, 
#равную 1 для города и 0 для областного центра ((было город - 2, область - 1)).
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "1=0")
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "4=0")
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "5=0")
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "2=1")
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "3=1")
h_2019_main$marital_status <- recode(h_2019_main$marital_status, "6=1")
h_2019_main$marital_status


write.table(h_2019_main, file="h_2019_main.csv", sep=",")

colSums(is.na(h_2019_main))
#  wages_non_payment, why_not_off_employment1, salary_add, working_hours_per_day2, hunt_for_job_time, b_settl_type, nationality, 
##n_underage, n_children, 
######b_settl_type, exercise, off_employment2, working_hours_per_week2, work_industry2, working_hours_per_day2, salary_add, why_not_off_employment2,hunt_for_job_time, ever_work
###

h6$from <- recode(h6$from, "1=0")
h6$from <- recode(h6$from, "2=1")

#Из переменной удовлетворённость работой сделать дамми-переменную,
#равную 1 для полностью удовлетворен и 0 для скорее удовлетворен ((было 1 - полностью удов, 2 - скорее удовл)).
h6$satisf <- recode(h6$satisf, "2=0")

#Из переменной пол сделать дамми-переменную, равную 1 мужчин и 0 для женщин (было 1- муж, 2 - жен)
h6$sex <- recode(h6$sex, "2=0")

#Использовать будем следующие категории:
#Незаконченное среднее образование (категории от 1 до 3 из пункта 6)
#Законченное среднее образование (категория 4 из пункта 6)
#Законченное среднее специальное образование (категория 5 из пункта 6)
#Законченное высшее образование (категория 6 из пункта 6)
#В итоге мы получаем 4 фиктивные переменные, 
#отвечающие за принадлежность респондента к одной из этих категорий.

h_2019_main1 <- within(h_2019_main, matrial_status_dummy <-ifelse(matrial_status ="2"|"3"|"6", 1, 0))

h7 <- within(h7,  <-ifelse(degree =="4", 1, 0))
h7 <- within(h7, d_3 <-ifelse(degree =="5", 1, 0))
h7 <- within(h7, d_4 <-ifelse(degree =="6", 1, 0))

#1
describe(h_2019_main)

#2
colSums(is.na(h7))

#3
h_non_NA <- na.omit(h7)
y <- h_non_NA$earning
hist(y)

#4
g1<-h8$earning/1000
ggplot(h8, aes(g1)) + 
  geom_histogram( aes(fill=sex), binwidth = 3.43, color ="black",fill="lightpink") +
  scale_x_continuous(limits =c(0, 100))+  facet_grid(~sex)
#4.2
ggplot(h8, aes(x=vozrast)) + 
  geom_histogram( aes(fill=sex),position="dodge",binwidth = 10, color="white") + 
  facet_wrap(~sex)
#В первой --- доход (в тысячах рублей) , во второй --- возраст (в годах)

#5
model <- lm(data = h_non_NA,earning~vozrast+sex+d_2+d_3+d_4+from+satisf)
h_non_NA
summary(model)
x <- model.matrix(model)
x
#F=59.08

#6
# -- 4

#7
summary(model)
# 14.948

#8
# -- 4 

#9
vcovHC(model)
conftable <- coeftest(model, vcov. = vcovHC(model))
conftable
# 602.689
363234.2864^0.5


