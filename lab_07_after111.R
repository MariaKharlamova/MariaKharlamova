

# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 07
library("memisc")
library("lmtest")
library("broom")
library("psych") # описательные статистики
library("dplyr")  # манипуляции с данными
library("erer")  # расчет предельных эффектов
library("vcd")  # графики для качественных данных
library("ggplot2")  # графики
library("reshape2")  # манипуляции с данными
library("AUC")  # для ROC кривой


# при загрузке файлов R автоматом переделывает все строковые переменные в
# факторные эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- read.csv("h_2019_main.csv", sep = ';')

# смотрим на набор данных
glimpse(t)
t

t <- mutate(t,  weight_changed=as.factor(weight_changed))
t$working_hours_per_day1
summary(t$weight_changed)

# объясняем R, какие переменные считать факторными
t <- mutate(t, marital_status=as.factor(marital_status), sex = as.factor(sex), children = as.factor(children), weight_changed=as.factor(weight_changed), b_settl_shift = as.factor(b_settl_shift), settl_type = as.factor(settl_type),
            settl_type = as.factor(settl_type), region =  as.factor(region), prof_group =  as.factor(prof_group), edu = as.factor(edu),
            degree = as.factor(degree),  want_change_job=as.factor(want_change_job), main_occupation = as.factor(main_occupation), work_satisf = as.factor(work_satisf),
            work_industry1 = as.factor(work_industry1), off_employment1 = as.factor(off_employment1),
          fin_situation_dynam=as.factor(fin_situation_dynam), trust=as.factor(trust), 
             smoke=as.factor(smoke), drink=as.factor(drink))

t$region <- recode(t$region, '141'='1')
t$region <- recode(t$region, '138'='2')
t$region <- recode(t$region, '140'='2')
t$region <- recode(t$region, '142'='2')
t$region <- recode(t$region, '9'='3')
t$region <- recode(t$region, '39'='3')
t$region <- recode(t$region, '129'='3')
t$region <- recode(t$region, '137'='3')
t$region <- recode(t$region, '12'='4')
t$region <- recode(t$region, '10'='4')
t$region <- recode(t$region, '45'='4')
t$region <- recode(t$region, '47'='4')
t$region <- recode(t$region, '48'='4')
t$region <- recode(t$region, '70'='4')
t$region <- recode(t$region, '100'='4')
t$region <- recode(t$region, '116'='4')
t$region <- recode(t$region, '117'='4')
t$region <- recode(t$region, '14'='5')
t$region <- recode(t$region, '33'='5')
t$region <- recode(t$region, '67'='5')
t$region <- recode(t$region, '72'='5')
t$region <- recode(t$region, '135'='5')
t$region <- recode(t$region, '136'='5')
t$region <- recode(t$region, '46'='6')
t$region <- recode(t$region, '86'='6')
t$region <- recode(t$region, '106'='6')
t$region <- recode(t$region, '107'='6')
t$region <- recode(t$region, '52'='7')
t$region <- recode(t$region, '77'='7')
t$region <- recode(t$region, '58'='8')
t$region <- recode(t$region, '66'='8')
t$region <- recode(t$region, '71'='8')
t$region <- recode(t$region, '73'='8')
t$region <- recode(t$region, '84'='8')
t$region <- recode(t$region, '161'='8')
t$region <- recode(t$region, '89'='9')
t$region <- recode(t$region, '105'='9')
t$region <- recode(t$region, '92'='10')
t$region <- recode(t$region, '93'='10')

t$region <- recode(t$region, '2'='1')
t$region <- recode(t$region, '1'='5')
summary(t$region)

#wages_non_payment           
summary(t)
t$work_industry1
t1
# мозаичный график (sex = 1- муж, 2 - жен; children= 1-есть, 2-нет; marital_status=1-сост.в зарег.браке, 0-не сост.в зарег браке )
mosaic(data = t, ~ marital_status + sex + children, shade = TRUE)

# график-виолончель
qplot(data = t, x = marital_status, y = age, geom = "violin")
#видно, что распределения по возрасту среди людей в браке и вне сильно различается
qplot(data = t, x = marital_status, y = tot_incom, geom = "violin")


# 'ящик с усами'
qplot(data = t, x = marital_status, y = age, geom = "boxplot")


# два варианта сглаженной функции плотности

ggplot(data = t, aes(x = age, y = ..count.., fill = marital_status)) + geom_density(position = "stack")
ggplot(data = t, aes(x = age, y = ..count.., fill = marital_status)) + geom_density(position = "fill")

# !!!! отличие от исходника !!!!
# вместо qplot() в исходнике используется ggplot()
# опция `position` исчезла из команды qplot() при обновлении пакета ggplot2 до версии 2.0


# Оценивание логит и пробит моделей
region_d <- model.matrix(~t$region+0 )
region_d 
region_d <- region_d[ , -10] 
region_d
m_logit00 <- glm(data = t, marital_status ~ sex + age +height+weight + weight_changed + tot_incom 
                , family = binomial(link = "logit"), x = TRUE)

m_logit1 <- glm(data = t, marital_status ~ region_d + sex + age +height+weight + weight_changed + tot_incom + n_children + 
                  settl_type + main_occupation
                , family = binomial(link = "logit"),
                x = TRUE)

m_logit0 <- glm(data = t, marital_status ~ region_d + sex + age + tot_incom + children + settl_type
               , family = binomial(link = "logit"),
  x = TRUE)

m_logit0 <- glm(data = t, marital_status ~ sex + age +height+weight + weight_changed + n_children + 
                  settl_type 
                , family = binomial(link = "logit"),
                x = TRUE)


m_logit1 <- glm(data = t, marital_status ~ region_d + sex + age +height+weight + weight_changed + n_children + 
                  settl_type 
               , family = binomial(link = "logit"),
               x = TRUE)
lrtest(m_logit0, m_logit1)

m_logit2 <- glm(data = t, marital_status ~ region_d + sex + age +height+weight + weight_changed + tot_incom + n_children + 
                  settl_type + main_occupation +  working_hours_per_week1 + salary_main + off_employment1 + off_work_experience + work_satisf + want_change_job
                , family = binomial(link = "logit"),
                x = TRUE)
lrtest(m_logit1, m_logit2)

m_logit3 <- glm(data = t, marital_status ~ region_d + sex + age +height+weight + weight_changed + tot_incom + n_children + 
                  settl_type + main_occupation +  working_hours_per_week1 + salary_main + off_employment1 + off_work_experience + work_satisf + want_change_job
                + fin_situation_dynam + life_satisf + belief +health_doctor + health_state + smoke + drink  , family = binomial(link = "logit"),
                x = TRUE)
lrtest(m_logit2, m_logit3)
m_logit4 <- glm(data = t, marital_status ~ region_d + sex + age +height+weight + weight_changed + tot_incom + n_children + 
                  settl_type + main_occupation +  working_hours_per_1 + salary_main + off_employment1 + off_work_experience + work_satisf + want_change_job
                + fin_situation_dynam + life_satisf + belief +health_doctor + health_state + smoke + drink  , family = binomial(link = "logit"),
                x = TRUE)

# проводим LR тест
lrtest(m_logit, m_logit2)
# гипотеза о том, что финансовые переменные не влияют на вероятность выжить отвергается

m_probit <- glm(data = t, marital_status ~ sex + age +height+weight + weight_changed + tot_incom + children + settl_type + occupation_code, family = binomial(link = "probit"),
  x = TRUE)
# отчеты об оценке моделей
summary(m_logit3)
summary(m_probit)
#мы моделируем логорифм отношения шансов, то есть логарифм дроби P(y = 1)/ P(y = 0). 
#Рассмотрим коэффициент перед непрерывной переменной — возраст. И перед переменной дискретной — пол (1 — мужчина, 0 — женщина)
#Коэффициент β₂ с крышкой при age = -0,03 Это означает, что с ростом переменной «возраст» на 1, то есть на 1 год, 
#отношение шансов падает, раз коэффициент отрицательный, процентное падение на 3%. 
#Аналогично можно проинтерпретировать коэффициент при любой непрерывной переменной, например, при переменной «тариф». 
#β с крышкой fare = 0.0003, значит с ростом тарифа на одну единицу (он измерялся в фунтах) логарифмическое отношение шансов, растет на 0,0003, а само отношение шансов растет на 0.03%.

#для пола, здесь величина коэффициента большая, поэтому интерпретировать производную не получится, лучше посчитать изменения аккуратно, с учетом экспоненты.
# Если логарифм отношения шансов = ... + -2,49 умножить на дамми-переменную, обозначающую пол участника +...,
#то это означает, что само отношение шансов, P(y = 1)/ P(y = 0), это есть экспонента, от -2,49, помножить на дамми-переменную, обозначающую пол участника, помножить на остальные составляющие. 
#Соответственно, при сравнении двух пассажиров, женщины и мужчины, мы получим, что отношение шансов меняется в экспоненту от -2,49 раз. 
#То есть, если переменная sex male меняется от 0 до 1 (сравниваем женщину и мужчину), то получается, что это отношение шансов делится на экспоненту от 2.49, это примерно 12. 
#То есть мы можем сказать, что отношение шансов, шансы у женщин примерно в 12 раз больше, чем у мужчин. 
#Раз коэффициент отрицательный, значит при переходе от 0 к 1 отношение шансов падает. И отношение шансов падает примерно в 12 раз.


# оценка ковариационной матрицы оценок коэффициентов
vcov(m_logit)

# создаём новый массив данных для прогнозирования
newdata <- data.frame(age = seq(from = 18, to = 80, length = 50), sex = "1",
                      tot_incom=40000, children='1', settl_type='2', work_industry1='31')
# посмотрим на начало этой таблички
head(newdata)

# прогнозируем по логит модели
pr_logit <- predict(m_logit, newdata, se = TRUE)
# соединим прогнозы и новый массив данных в единую табличку:
newdata_pr <- cbind(newdata, pr_logit)
head(newdata_pr)  # смотрим на начало таблички

# применив логистическую функцию распределения получим границы доверительного интервала
newdata_pr <- mutate(newdata_pr, prob = plogis(fit), 
                     left_ci = plogis(fit - 1.96 *se.fit), 
                     right_ci = plogis(fit + 1.96 * se.fit))
head(newdata_pr)  # смотрим на результат

# посмотрим на графике как меняется доверительный интервал для вероятности
qplot(data = newdata_pr, x = age, y = prob, geom = "line") + geom_ribbon(aes(ymin = left_ci,
  ymax = right_ci), alpha = 0.2)


# проведем LR тест R при построении разных моделей автоматом использует
# максимальное количество полных наблюдений поэтому часто выходит, что
# ограниченная и неограниченная модель оцениваются на разном наборе данных но в
# таком случае их нельзя сравнивать с помощью LR теста поэтому мы сначала
# создадим набор данных t2 без пропущенных значений и на нем оценим короткую и
# длинную модели H0: beta(pclass)=0, beta(fare)=0
t2 <- select(t, sex, age, pclass, survived, fare) %>% na.omit()
# оцениваем ограниченную модель
m_logit2 <- glm(data = t2, survived ~ sex + age, family = binomial(link = "logit"),
  x = TRUE)
# проводим LR тест
lrtest(m_logit, m_logit2)
# гипотеза о том, что финансовые переменные не влияют на вероятность выжить отвергается

maBina( m_logit)  # предельные эффекты для среднестатистического респондента

# усредненные предельные эффекты по всем пассажирам
maBina(m_logit3, x.mean = FALSE)

# обычный МНК #для шага с ROC нужно опустить или перекодировать формат 
m_ols <- lm(data = t, as.numeric(marital_status) ~ sex + age + tot_incom + children + settl_type + work_industry1)

summary(m_ols)

# прогнозы по обычному МНК
pr_ols <- predict(m_ols, newdata)
head(pr_ols)

# ROC кривая спрогнозируем скрытую переменную для исходного набора данных
pr_t <- predict(m_logit00, t, se = TRUE)

# соединим прогнозы с исходным набором данных
t <- cbind(t, pr_t)
t
# применим логистическую функцию распределения, чтобы получить вероятности
t <- mutate(t, prob = plogis(fit))
# глянем выборочно на результат:
select(t, age, marital_status, prob)
t<-t[ , -42]
t
describe(t)
# переберем различные пороги отсечения и посмотрим как они влияют на вероятность неверной классификации пассажиров
# получим все данные для ROC кривой:

roc.data <- AUC::roc(t$prob, t$marital_status)
str(roc.data)
AUC::auc(roc.data)
plot(roc.data, col = "blue")
# Calculate the area under the curve (AUC)
auc(roc.data)

# три графика для выбора порога отсечения по горизонтали --- пороги, по вертикали
# --- чувствительность чувствительность = число выживших верно предсказанных
# выжившими / общее количество выживших
qplot(x = roc.data$cutoffs, y = roc.data$tpr, geom = "line")
#доля верно класс-х выживших

# по горизонтали --- пороги, по вертикали --- процент ложноположительных
# прогнозов процент ложно положительных прогнозов = число погибших ошибочно
# предсказанных выжившими/общее число погибших
qplot(x = roc.data$cutoffs, y = roc.data$fpr, geom = "line")
#доля неверно классиф-х невыживших

#qplot(x = roc.data$cutoffs, y = roc.data$fpr, geom = "line") +geom_line(aes(x = roc.data$cutoffs, y = roc.data$tpr))

# по горизонтали --- процент ложноположительных прогнозов по вертикали ---
# чувствительность
qplot(x = roc.data$fpr, y = roc.data$tpr, geom = "line")

######################################
#from datacamp
# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(roc.data, col = "blue")

# Calculate the area under the curve (AUC)
auc(roc.data)

# ROC кривая спрогнозируем скрытую переменную для исходного набора данных
pr_t <- predict(m_logit00, t, se = TRUE)
# соединим прогнозы с исходным набором данных
t <- cbind(t, pr_t)
# применим логистическую функцию распределения, чтобы получить вероятности
t <- mutate(t, prob = plogis(fit))
# глянем выборочно на результат:
select(t, age, marital_status, prob)
describe(t)
t <- t[ ,-42]
t
# переберем различные пороги отсечения и посмотрим как они влияют на вероятность неверной классификации пассажиров
# получим все данные для ROC кривой:

roc.data <- roc(t$prob, t$marital_status)
roc.data$
str(roc.data)

