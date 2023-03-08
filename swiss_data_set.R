library("lmtest")
library("GGally")

data=swiss

data
summary(data)

#ggpairs(data)

#Я в списке 12й,пробую 12й вариант 
#Объясняемая переменная Examination,регрессоры Agriculture Fertility

summary(data)
# Срднее для Fertility 70.14
# Среднее для Agriculture 50.66
dispers_fertility=var(data$Fertility) # вычисление дисперсии
dispers_agriculture=var(data$Agriculture)
print(var(data$Examination))
print(dispers_agriculture)
print(dispers_fertility)
sko1=sqrt(dispers_fertility)          # Вычисление СКО
sko2=sqrt(dispers_agriculture)

print(sko1)
print(sko2)
print(sqrt(var(data$Examination)))


model1=lm(Examination~Agriculture,data)
summary(model1)
#plot(model1)
#R^2 46% нет линейной зависимотсти, у коэф. альфа и у свободного коэф.
# стоит по три звездочки,значит между ними есть  отрицательная  зависимость
# так же коэф. "p" маленький, значит наша модель достаточно точная,
# и отклонения от среднего маловероятны


model2=lm(Examination~Fertility,data)
summary(model2)

# Значение R^2 40% линейной заисимоти нет. Так как коэф. p очень мал, можно сказать
# что отклонения от среднего маловероятны. У коэф. альфа и свободного коэф.
# по три звездочки, значит есть отрицательная зависимость


#Проверим на наличие зависимости между регрессорами
test_model1 = lm(Agriculture~Fertility,data)
test_model2 = lm(Fertility~Agriculture,data)
summary(test_model1)
summary(test_model2)
# Для model1: Нет существенной зависимости между переменными, высокий коэф р, R^2 11%
# Для model2: Линейной зависимоти нет:  R^2 < 15%, их можно использовать в паре


#Теперь расмотрим множественную регрессию

model12=lm(Examination~Agriculture+Fertility,data)
summary(model12)
# Неплохая линейная зависимость R^2 65%. Есть зависимость от обоих регрессоров