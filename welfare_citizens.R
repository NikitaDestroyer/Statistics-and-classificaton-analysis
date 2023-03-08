install.packages("foreign")
install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
library("lmtest")
library("car")
library("rlms")
library("GGally")
library("dplyr")

#read the file
data1<-rlms_read("D:\\r12i_os26b.sav")
#glimpse(data)


#Выбираем столбцы данных с которыми будем работать(8 характеристик)
#з/п,пол, семейное положение, образование,возраст, тип населенного пункта,рабочая неделя,
#условия труда,владееет ли предприятем иностранные лица,
data= select(data1,hj13.2,hh5,h_marst,h_diplom,h_age,status,hj6.2,hj1.1.2,hj24)


# Избавляемся от пустых ячеек
data=na.omit(data)
#glimpse(data)

#Предварительная нормализация данных с большим разбросом
#зарплата c элементами нормализации
sal1 = as.character(data$hj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data["salary"] = (sal - mean(sal)) / sqrt(var(sal))

#нормализация возраста
age1 = as.character(data$h_age)
age2 = lapply(age1,as.integer)
age=as.numeric(unlist(age2))
data["age"] = (age - mean(age)) / sqrt(var(age))

#нормализация количества часов в рабочей неделе
wtime1 = as.character(data$hj6.2)
wtime2 = lapply(wtime1,as.integer)
wtime = as.numeric(unlist(wtime2))
data["weekly_hours"] = (wtime - mean(wtime)) / sqrt(var(wtime))



# пол
data["sex"]=data$hh5
data["sex"] = lapply(data["sex"], as.character)
data$sex[which(data$sex=='2')] <- 0
data$sex[which(data$sex=='1')] <- 1
data$sex = as.numeric(data$sex)


# владеют ли иностарнные лица вашей компанией, где вы рабатаете
data["foreign1"] = data$hj24
data["foreign1"] = lapply(data["foreign1"],as.character)
data["foreign"] = data$foreign1
data["foreign"] = 0
data$foreign[which(data$foreign1=="1")] <- 1
data$foreign= as.numeric(data$foreign)

# Законченное образовние 
data["h_educ"] = data$h_diplom
data["h_educ"] = lapply(data["h_educ"], as.character)
data["high_edu"] = data$h_educ
data["high_edu"] = 0
data$high_edu[which(data$h_educ=='5')] <- 1
data$high_edu[which(data$h_educ=='6')] <- 1
data$high_edu = as.numeric(data$high_edu)

# статус населенного пункта
data["status_pgt"] = data$status
data["status_pgt"] = lapply(data["status_pgt"],as.character)
data["status"] = data$status_pgt
data["status"] = 0
data$status[which(data$status_pgt=="1")] <-1
data$status[which(data$status_pgt=="2")] <-1
data$status = as.numeric(data$status)

# семейное положение
data["wed"] = data$h_marst
data["wed"] = lapply(data["wed"], as.character)
data["wed1"] = data$wed # Разведены, либо потеряли супруга
data$wed1 = 0
data$wed1[which(data$wed=="4")] <- 1
data$wed1[which(data$wed=="5")] <- 1
data$wed1 = as.numeric(data$wed1)

data["wed2"] = data$wed #парочки ;)
data$wed2 = 0
data$wed2[which(data$wed=="2")] <- 1
data$wed2[which(data$wed=="3")] <- 1
data$wed2 = as.numeric(data$wed2)

data["wed3"] = data$wed # Never get married
data$wed3 = 0
data$wed3[which(data$wed=="1")] <- 1
data$wed3 = as.numeric(data$wed3)


# Условия труда
data["sat"] = data$hj1.1.2
data["sat"] = lapply(data["sat"], as.character)
data['satisfaction'] = data$sat
data$satisfaction = 0
data$satisfaction[which(data$sat=="1")] <-1
data$satisfaction[which(data$sat=="5")] <-1
data$satisfaction = as.numeric(data$satisfaction)

data=na.omit(data)


data2 = select(data,salary,sex,wed1,wed2,wed3,high_edu,age,status,weekly_hours,satisfaction,foreign)


#1) Построение зависимостей

#ggpairs(data2)

model1 = lm(data = data2,salary ~ sex + wed1 + wed2 +wed3 + high_edu + age + status + weekly_hours + satisfaction + foreign)
summary(model1)
# Выкинули из анализа wed3. R^2=13%

model2 = lm(data = data2,salary ~ sex + wed1  +wed2 + high_edu + age + status + weekly_hours + satisfaction + foreign)
summary(model2)
vif(model2)
# R^2=13%,попробуем выкинуть переменные у которых маленькие значения р-статистики. И по значению вздутия дисперсии
# нужно обратить внимание на переменные wed1,wed2 значения примерно 2

model3 = lm(data = data2,salary ~ sex + wed1  +wed2 + high_edu + age  + weekly_hours  + foreign)
summary(model3)
vif(model3)

model_help= lm(data =data2,wed1 ~ wed2)
summary(model_help)
# Вздутие дисперсии у переменных по преженему не большие,кроме wed1,wed2, они линейно зависимы стоит одну выкинуть.Также  Мы выкинули две переменные из нашего анализа: Satisfaction,status, причем R^2=13%
# остался прежним, значит они были не сильно важны для нашего анализа. Остальные переменные хорошо описывают нашу зависиомтсь
# можно попробовать выкинуть переменные, которые имеют большие значения р -статистики по сравнению с остальными

model4 = lm(data = data2,salary ~ sex + wed1 + high_edu   + weekly_hours  + foreign)
summary(model4)
vif(model4)
# R^2=13% изменился не сильно, убрали переменную age, р -статистика, низкая у wed1

model20 = lm(data = data2,salary ~ sex +wed2+  wed1 + high_edu   + weekly_hours  +satisfaction+ foreign)
summary(model20)
vif(model20)

model5  = lm(data = data2,salary ~ sex  + high_edu   + weekly_hours  + foreign)
summary(model5)
vif(model5)
# Переменные хорошо описывают нашу зависимость, попробуем поэксперементировать с добавлением новых регрессоров


model6  = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^0.5)  + foreign)
summary(model6)
vif(model6)
# Увелиичли степент регрессора, причем R^2=12%,он упал, значит нужно попробовать что нибудь другое

model6.1 = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^1.5)  + foreign)
summary(model6.1)
vif(model6.1)
# R^2 = 12%, упал,  не стоит использовать weekly_hours в степени в дальнейшем анализе

model6.2 = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^2)  + foreign)
summary(model6.2)
vif(model6.2)
# R^2 = 11.5 упал еще больше, хоть мы и имеем много звезд рядом с этим регрессором, но R^2 сильно снижается

model6.2 = lm(data = data2,salary ~ sex + I(age^0.1)  + high_edu   + weekly_hours  + foreign)
summary(model6.2)
vif(model6.2)
# R^2 =  13.79%, попробуем возведение в степень


model6.3 = lm(data = data2,salary ~ sex + I(age^0.5)  + high_edu   + weekly_hours  + foreign)
summary(model6.3)
vif(model6.3)
# R^2 = 13.95 %

model6.4 = lm(data = data2,salary ~ sex + I(age^1.5)  + high_edu   + weekly_hours  + foreign)
summary(model6.4)
vif(model6.4)
# R^2 = 14.14, увеличиваем степень переменной age, она хорошо описывает нашу модель


model7  = lm(data = data2,salary ~ sex + I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model7)
vif(model7)
# Я добавил переменную age, в степени она хорошо описывает нашу завсисимость, увеличился R^2=14.25%,  пока лучшая модель

model8  = lm(data = data2,salary ~ sex  + high_edu   + I(age*weekly_hours)  + foreign)
summary(model8)
vif(model8)
# Плохая переменная age*weekly_hours, упал R^2, p-statistics тоже низкий

model9  = lm(data = data2,I(salary^0.1) ~ sex  + high_edu   + I(age^2) + weekly_hours  + foreign)
summary(model9)
vif(model9)
# R^2=5% fell dramatically.

model10  = lm(data = data2,I(salary^0.5) ~ sex  + high_edu   + I(age^2) + weekly_hours  + foreign)
summary(model10)
vif(model10)
# R^2 = 5 % still

model10.1  = lm(data = data2,I(salary^1.5) ~ sex  + high_edu   + I(age^2) + weekly_hours  + foreign)
summary(model10.1)
vif(model10.1)
# R^2 = 5 % still. Возведение salary в квадарат, плохо описывает нашу моедель

model11 = lm(data = data2, salary ~  log(abs(age)+1) + sex + high_edu    + weekly_hours  + foreign)
summary(model11)
vif(model11)
# С добавлением логарифма увеличивается R^2=14.06. Модель, где регрессор квадрат от age, имеет больший R^2

model12 = lm(data = data2,salary ~  log(abs(weekly_hours)+1) + sex + high_edu  + foreign)
summary(model12)
vif(model12)
# R^2=11.5%,
# Пронализируя модели 6-12, age^2 лучше описывает нашу модель чем логарифм от этой переменной

model13 = lm(data = data2, log(abs(salary)+1) ~  I(age^2)  +  high_edu    + weekly_hours  + foreign)
summary(model13)
vif(model13)
# R^2 < 1%, очень сильно упал

model14  = lm(data = data2,salary ~ sex + wed1+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model14)
vif(model14)
# R^2 = 14.24, добавленная переменная wed1 имеет мало звездочек, тем не менее, она    не сильно снижает наш R^2
# значит она может иметь значимость в нашем анализе

model15  = lm(data = data2,salary ~ sex + wed2+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model15)
vif(model15)
# R^2 = 14.22. Точно такая же ситуация как и с переменной wed1

model16  = lm(data = data2,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model16)
vif(model16)
# R^2 = 14.26, стал больше, теперь эта модель лучше чем model7 

# ЛУчшая модель из перечисленных model16
# Какие индивиды получают наибольшую зарплату?
# Ответ: Большу зарплату получают индивиды мужского пола, не состоящие в браке, молодого возраста, с высшим образованием,которые больше работают
# и их предприятием владеет иностранная компания

# Нужно оценить зарплату для конкретного подмножества: 
# 1) Не состоявшие в браке мужчины, с высшим образованием; 
data4 = subset(data2, wed3==1)
data4 = subset(data2,high_edu==1)

# Create a model for this subset #1
model_subset1  = lm(data = data4,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model_subset1)
vif(model_subset1)
# R^2 = 13.14, упал по сравнению на всей выборке. Количество звездочек стало меньше у таких коэф, как high edu, и у свобожного коэф

#2) Городские жители, состоящие в браке, женщины
data5 = subset(data2, wed2==1)
data5 = subset(data2, sex==0)
data5 = subset(data2, status ==1 )
# Create a model for this subset #2
model_subset2 = lm(data=data5,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model_subset2)
vif(model_subset2)
# R^2  = 14.26 % для данного подмножества получилась хорошая лин зависимость

