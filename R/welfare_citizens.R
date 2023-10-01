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


#Âûáèðàåì ñòîëáöû äàííûõ ñ êîòîðûìè áóäåì ðàáîòàòü(8 õàðàêòåðèñòèê)
#ç/ï,ïîë, ñåìåéíîå ïîëîæåíèå, îáðàçîâàíèå,âîçðàñò, òèï íàñåëåííîãî ïóíêòà,ðàáî÷àÿ íåäåëÿ,
#óñëîâèÿ òðóäà,âëàäåååò ëè ïðåäïðèÿòåì èíîñòðàííûå ëèöà,
data= select(data1,hj13.2,hh5,h_marst,h_diplom,h_age,status,hj6.2,hj1.1.2,hj24)


# Èçáàâëÿåìñÿ îò ïóñòûõ ÿ÷ååê
data=na.omit(data)
#glimpse(data)

#Ïðåäâàðèòåëüíàÿ íîðìàëèçàöèÿ äàííûõ ñ áîëüøèì ðàçáðîñîì
#çàðïëàòà c ýëåìåíòàìè íîðìàëèçàöèè
sal1 = as.character(data$hj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data["salary"] = (sal - mean(sal)) / sqrt(var(sal))

#íîðìàëèçàöèÿ âîçðàñòà
age1 = as.character(data$h_age)
age2 = lapply(age1,as.integer)
age=as.numeric(unlist(age2))
data["age"] = (age - mean(age)) / sqrt(var(age))

#íîðìàëèçàöèÿ êîëè÷åñòâà ÷àñîâ â ðàáî÷åé íåäåëå
wtime1 = as.character(data$hj6.2)
wtime2 = lapply(wtime1,as.integer)
wtime = as.numeric(unlist(wtime2))
data["weekly_hours"] = (wtime - mean(wtime)) / sqrt(var(wtime))



# ïîë
data["sex"]=data$hh5
data["sex"] = lapply(data["sex"], as.character)
data$sex[which(data$sex=='2')] <- 0
data$sex[which(data$sex=='1')] <- 1
data$sex = as.numeric(data$sex)


# âëàäåþò ëè èíîñòàðííûå ëèöà âàøåé êîìïàíèåé, ãäå âû ðàáàòàåòå
data["foreign1"] = data$hj24
data["foreign1"] = lapply(data["foreign1"],as.character)
data["foreign"] = data$foreign1
data["foreign"] = 0
data$foreign[which(data$foreign1=="1")] <- 1
data$foreign= as.numeric(data$foreign)

# Çàêîí÷åííîå îáðàçîâíèå 
data["h_educ"] = data$h_diplom
data["h_educ"] = lapply(data["h_educ"], as.character)
data["high_edu"] = data$h_educ
data["high_edu"] = 0
data$high_edu[which(data$h_educ=='5')] <- 1
data$high_edu[which(data$h_educ=='6')] <- 1
data$high_edu = as.numeric(data$high_edu)

# ñòàòóñ íàñåëåííîãî ïóíêòà
data["status_pgt"] = data$status
data["status_pgt"] = lapply(data["status_pgt"],as.character)
data["status"] = data$status_pgt
data["status"] = 0
data$status[which(data$status_pgt=="1")] <-1
data$status[which(data$status_pgt=="2")] <-1
data$status = as.numeric(data$status)

# ñåìåéíîå ïîëîæåíèå
data["wed"] = data$h_marst
data["wed"] = lapply(data["wed"], as.character)
data["wed1"] = data$wed # Ðàçâåäåíû, ëèáî ïîòåðÿëè ñóïðóãà
data$wed1 = 0
data$wed1[which(data$wed=="4")] <- 1
data$wed1[which(data$wed=="5")] <- 1
data$wed1 = as.numeric(data$wed1)

data["wed2"] = data$wed #ïàðî÷êè ;)
data$wed2 = 0
data$wed2[which(data$wed=="2")] <- 1
data$wed2[which(data$wed=="3")] <- 1
data$wed2 = as.numeric(data$wed2)

data["wed3"] = data$wed # Never get married
data$wed3 = 0
data$wed3[which(data$wed=="1")] <- 1
data$wed3 = as.numeric(data$wed3)


# Óñëîâèÿ òðóäà
data["sat"] = data$hj1.1.2
data["sat"] = lapply(data["sat"], as.character)
data['satisfaction'] = data$sat
data$satisfaction = 0
data$satisfaction[which(data$sat=="1")] <-1
data$satisfaction[which(data$sat=="5")] <-1
data$satisfaction = as.numeric(data$satisfaction)

data=na.omit(data)


data2 = select(data,salary,sex,wed1,wed2,wed3,high_edu,age,status,weekly_hours,satisfaction,foreign)


#1) Ïîñòðîåíèå çàâèñèìîñòåé

#ggpairs(data2)

model1 = lm(data = data2,salary ~ sex + wed1 + wed2 +wed3 + high_edu + age + status + weekly_hours + satisfaction + foreign)
summary(model1)
# Âûêèíóëè èç àíàëèçà wed3. R^2=13%

model2 = lm(data = data2,salary ~ sex + wed1  +wed2 + high_edu + age + status + weekly_hours + satisfaction + foreign)
summary(model2)
vif(model2)
# R^2=13%,ïîïðîáóåì âûêèíóòü ïåðåìåííûå ó êîòîðûõ ìàëåíüêèå çíà÷åíèÿ ð-ñòàòèñòèêè. È ïî çíà÷åíèþ âçäóòèÿ äèñïåðñèè
# íóæíî îáðàòèòü âíèìàíèå íà ïåðåìåííûå wed1,wed2 çíà÷åíèÿ ïðèìåðíî 2

model3 = lm(data = data2,salary ~ sex + wed1  +wed2 + high_edu + age  + weekly_hours  + foreign)
summary(model3)
vif(model3)

model_help= lm(data =data2,wed1 ~ wed2)
summary(model_help)
# Âçäóòèå äèñïåðñèè ó ïåðåìåííûõ ïî ïðåæåíåìó íå áîëüøèå,êðîìå wed1,wed2, îíè ëèíåéíî çàâèñèìû ñòîèò îäíó âûêèíóòü.Òàêæå  Ìû âûêèíóëè äâå ïåðåìåííûå èç íàøåãî àíàëèçà: Satisfaction,status, ïðè÷åì R^2=13%
# îñòàëñÿ ïðåæíèì, çíà÷èò îíè áûëè íå ñèëüíî âàæíû äëÿ íàøåãî àíàëèçà. Îñòàëüíûå ïåðåìåííûå õîðîøî îïèñûâàþò íàøó çàâèñèîìòñü
# ìîæíî ïîïðîáîâàòü âûêèíóòü ïåðåìåííûå, êîòîðûå èìåþò áîëüøèå çíà÷åíèÿ ð -ñòàòèñòèêè ïî ñðàâíåíèþ ñ îñòàëüíûìè

model4 = lm(data = data2,salary ~ sex + wed1 + high_edu   + weekly_hours  + foreign)
summary(model4)
vif(model4)
# R^2=13% èçìåíèëñÿ íå ñèëüíî, óáðàëè ïåðåìåííóþ age, ð -ñòàòèñòèêà, íèçêàÿ ó wed1

model20 = lm(data = data2,salary ~ sex +wed2+  wed1 + high_edu   + weekly_hours  +satisfaction+ foreign)
summary(model20)
vif(model20)

model5  = lm(data = data2,salary ~ sex  + high_edu   + weekly_hours  + foreign)
summary(model5)
vif(model5)
# Ïåðåìåííûå õîðîøî îïèñûâàþò íàøó çàâèñèìîñòü, ïîïðîáóåì ïîýêñïåðåìåíòèðîâàòü ñ äîáàâëåíèåì íîâûõ ðåãðåññîðîâ


model6  = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^0.5)  + foreign)
summary(model6)
vif(model6)
# Óâåëèè÷ëè ñòåïåíò ðåãðåññîðà, ïðè÷åì R^2=12%,îí óïàë, çíà÷èò íóæíî ïîïðîáîâàòü ÷òî íèáóäü äðóãîå

model6.1 = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^1.5)  + foreign)
summary(model6.1)
vif(model6.1)
# R^2 = 12%, óïàë,  íå ñòîèò èñïîëüçîâàòü weekly_hours â ñòåïåíè â äàëüíåéøåì àíàëèçå

model6.2 = lm(data = data2,salary ~ sex  + high_edu   + I(weekly_hours^2)  + foreign)
summary(model6.2)
vif(model6.2)
# R^2 = 11.5 óïàë åùå áîëüøå, õîòü ìû è èìååì ìíîãî çâåçä ðÿäîì ñ ýòèì ðåãðåññîðîì, íî R^2 ñèëüíî ñíèæàåòñÿ

model6.2 = lm(data = data2,salary ~ sex + I(age^0.1)  + high_edu   + weekly_hours  + foreign)
summary(model6.2)
vif(model6.2)
# R^2 =  13.79%, ïîïðîáóåì âîçâåäåíèå â ñòåïåíü


model6.3 = lm(data = data2,salary ~ sex + I(age^0.5)  + high_edu   + weekly_hours  + foreign)
summary(model6.3)
vif(model6.3)
# R^2 = 13.95 %

model6.4 = lm(data = data2,salary ~ sex + I(age^1.5)  + high_edu   + weekly_hours  + foreign)
summary(model6.4)
vif(model6.4)
# R^2 = 14.14, óâåëè÷èâàåì ñòåïåíü ïåðåìåííîé age, îíà õîðîøî îïèñûâàåò íàøó ìîäåëü


model7  = lm(data = data2,salary ~ sex + I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model7)
vif(model7)
# ß äîáàâèë ïåðåìåííóþ age, â ñòåïåíè îíà õîðîøî îïèñûâàåò íàøó çàâñèñèìîñòü, óâåëè÷èëñÿ R^2=14.25%,  ïîêà ëó÷øàÿ ìîäåëü

model8  = lm(data = data2,salary ~ sex  + high_edu   + I(age*weekly_hours)  + foreign)
summary(model8)
vif(model8)
# Ïëîõàÿ ïåðåìåííàÿ age*weekly_hours, óïàë R^2, p-statistics òîæå íèçêèé

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
# R^2 = 5 % still. Âîçâåäåíèå salary â êâàäàðàò, ïëîõî îïèñûâàåò íàøó ìîåäåëü

model11 = lm(data = data2, salary ~  log(abs(age)+1) + sex + high_edu    + weekly_hours  + foreign)
summary(model11)
vif(model11)
# Ñ äîáàâëåíèåì ëîãàðèôìà óâåëè÷èâàåòñÿ R^2=14.06. Ìîäåëü, ãäå ðåãðåññîð êâàäðàò îò age, èìååò áîëüøèé R^2

model12 = lm(data = data2,salary ~  log(abs(weekly_hours)+1) + sex + high_edu  + foreign)
summary(model12)
vif(model12)
# R^2=11.5%,
# Ïðîíàëèçèðóÿ ìîäåëè 6-12, age^2 ëó÷øå îïèñûâàåò íàøó ìîäåëü ÷åì ëîãàðèôì îò ýòîé ïåðåìåííîé

model13 = lm(data = data2, log(abs(salary)+1) ~  I(age^2)  +  high_edu    + weekly_hours  + foreign)
summary(model13)
vif(model13)
# R^2 < 1%, î÷åíü ñèëüíî óïàë

model14  = lm(data = data2,salary ~ sex + wed1+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model14)
vif(model14)
# R^2 = 14.24, äîáàâëåííàÿ ïåðåìåííàÿ wed1 èìååò ìàëî çâåçäî÷åê, òåì íå ìåíåå, îíà    íå ñèëüíî ñíèæàåò íàø R^2
# çíà÷èò îíà ìîæåò èìåòü çíà÷èìîñòü â íàøåì àíàëèçå

model15  = lm(data = data2,salary ~ sex + wed2+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model15)
vif(model15)
# R^2 = 14.22. Òî÷íî òàêàÿ æå ñèòóàöèÿ êàê è ñ ïåðåìåííîé wed1

model16  = lm(data = data2,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model16)
vif(model16)
# R^2 = 14.26, ñòàë áîëüøå, òåïåðü ýòà ìîäåëü ëó÷øå ÷åì model7 

# ËÓ÷øàÿ ìîäåëü èç ïåðå÷èñëåííûõ model16
# Êàêèå èíäèâèäû ïîëó÷àþò íàèáîëüøóþ çàðïëàòó?
# Îòâåò: Áîëüøó çàðïëàòó ïîëó÷àþò èíäèâèäû ìóæñêîãî ïîëà, íå ñîñòîÿùèå â áðàêå, ìîëîäîãî âîçðàñòà, ñ âûñøèì îáðàçîâàíèåì,êîòîðûå áîëüøå ðàáîòàþò
# è èõ ïðåäïðèÿòèåì âëàäååò èíîñòðàííàÿ êîìïàíèÿ

# Íóæíî îöåíèòü çàðïëàòó äëÿ êîíêðåòíîãî ïîäìíîæåñòâà: 
# 1) Íå ñîñòîÿâøèå â áðàêå ìóæ÷èíû, ñ âûñøèì îáðàçîâàíèåì; 
data4 = subset(data2, wed3==1)
data4 = subset(data2,high_edu==1)

# Create a model for this subset #1
model_subset1  = lm(data = data4,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model_subset1)
vif(model_subset1)
# R^2 = 13.14, óïàë ïî ñðàâíåíèþ íà âñåé âûáîðêå. Êîëè÷åñòâî çâåçäî÷åê ñòàëî ìåíüøå ó òàêèõ êîýô, êàê high edu, è ó ñâîáîæíîãî êîýô

#2) Ãîðîäñêèå æèòåëè, ñîñòîÿùèå â áðàêå, æåíùèíû
data5 = subset(data2, wed2==1)
data5 = subset(data2, sex==0)
data5 = subset(data2, status ==1 )
# Create a model for this subset #2
model_subset2 = lm(data=data5,salary ~ sex + wed3+ I(age^2)  + high_edu   + weekly_hours  + foreign)
summary(model_subset2)
vif(model_subset2)
# R^2  = 14.26 % äëÿ äàííîãî ïîäìíîæåñòâà ïîëó÷èëàñü õîðîøàÿ ëèí çàâèñèìîñòü

