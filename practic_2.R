library("lmtest")


data=Seatbelts

help(data)
data
summary(data)

# 1)Проверьте, что в наборе данных нет линейной зависимости (построить зависимости 
#между переменными, указанными в варианте, и проверить, что R2 в каждой из них  невысокий). В случае, если R2
#большой, один из таких столбцов можно исключить 
#из рассмотрения.  Набор данных Seatbelts Ваиант 12 
model1_test=lm(law~kms,data)
model1_test
summary(model1_test)
# R^2=24% что означет, что с линенйо зависимотси между параметрами нет, и их можно использовать вместе



model2_test=lm(law~PetrolPrice,data)
model2_test
summary(model2_test)
# R^2=15% , что означает линейной зависимости между параметрами нет, эти параматеры можно использовать вместе



model3_test=lm(kms~PetrolPrice,data)
model2_test
summary(model3_test)
# R^2=15%, линейной зависимости между параметрами нет, значит их можно использовать вместе

# 2)  Неободимо построить модель линейной зависимости от переменных указанны в варианте.
# Оценить качество модели по R^2, p -statisisc
model_1=lm(front~kms+law+PetrolPrice,data)
model_1
summary(model_1)
# R^2=43%, у параметров law и PetrolPrice, малые значения p - статистики, они хорошо объясняют нашу зависимость, у 
# kms наоборот ни одной звезды, знаит лучше избавиться от этой переменной

model_1_2=lm(front~law+PetrolPrice,data)
model_1_2
summary(model_1_2)
#
#3) Введем в расмотрение логарифмы регрессоров
data=Seatbelts
data=as.data.frame(data)
data["law"]=data["law"]+1
data["law_log"]=log(data["law"])


data["PetrolPrice"]=data["PetrolPrice"]+1
#Прибаляем к столбцу PetrolPrice 1, что логарифмы вычислялись

data["PetrolPrice_log"]=log(data["PetrolPrice"])
data["kms_log"]=log(data["kms"])

data

#4)Необходимо ввести в модель все возможные произведения пар регрессоров, в том числе квадраты
# Найти лучшую из моделей
model_2=lm(front~+PetrolPrice+I(law^2),data)
model_2
summary(model_2)
# R^2=43% линейно зависисмоти нет, но между регрессорами есть некоторая зависимость

model_2_1=lm(front~law+I(PetrolPrice^2),data)
model_2_1
summary(model_2_1)
# R^2=43%,  все те же показатели

model_2_6=lm(front~law+PetrolPrice_log,data)
model_2_6
summary(model_2_6)
# R^2=43%, логарифмы пока не приносят существенного выйгрыша

model_2_7=lm(front~ PetrolPrice+law_log, data)
model_2_7
summary(model_2_7)
# R^2=43%, те же показатели что и у предыдущих моделей

# Изначальная модель front~law+PetrolPrice оказалась наилучшей из всех протестированных моделей