# Федоров Олег БЭК-172
# В этом скрипте краткое решение домашки, а более подробное - в rmd файле, который почему-то не работает
library("psych")
library("dplyr")
library("ggplot2")
library("GGally")
library("knitr")
library("jtools")
library("car")
library("mctest")
library("tsoutliers")
library("lmtest")
library("sandwich")
#считываем данные
fireinitial <- read.csv("forestfires.csv")

#оставляем только наблюдения по выбранным признакам, где площадь пожара была ненулевой

dffires <- subset(fireinitial[c(6, 9:11, 13)], area > 0)
synergy <- dffires['temp']*log1p(dffires['wind'])*((100-dffires['RH'])/10)^2
dffires <- data.frame(dffires, synergy)
colnames(dffires)[6] <- "synergy"
# Диаграммы рассеяния и корреляции, дескриптивные статистики, гистограммы
ggpairs(dffires)
describe(dffires)
qplot(data = dffires, DMC, ylab="Показатель индекса", main = "Распределение DMC")
qplot(data = dffires, temp, ylab="Градусов Цельсия", main = "Распределение температуры")
qplot(data = dffires, RH, ylab="Относительная влажность,%", main = "Распределение влажности воздуха")
qplot(data = dffires, wind, ylab="Скорость ветра,км/ч", main = "Распределение силы ветра")
qplot(data = dffires, area, ylab="Площадь лесных пожаров", main = "Распределение площади лесных пожаров")
qplot(data = dffires, synergy, ylab="показатель индекса", main = "Распределение синергии")



# Модель: линейная регрессия
model_fires <- lm(data = dffires, area ~ DMC+temp+RH+wind+synergy)
summary(model_fires)

#Проверяем модель на мультиколлинеарность
vif(model_fires)
omcdiag(model_fires,cn = 30)


# Проверяем остатки на нормальность с помощью теста Харке-Бера
JarqueBera.test(residuals(model_fires))

# Находим медианные значения для признаков
median(dffires$DMC)
median(dffires$temp)
median(dffires$RH)
median(dffires$wind)
median(dffires$synergy)

nw <- data.frame( DMC = c(median(dffires$DMC)),temp = c(median(dffires$temp)), 
                RH = c(median(dffires$RH)), wind = c(median(dffires$wind)), 
                synergy =median(dffires$synergy))

#Точечный прогноз
predict(model_fires, newdata = nw)
#Прогноз для среднего
predict(model_fires, newdata = nw,interval = "confidence")
#Индивидуальный прогноз
predict(model_fires, newdata = nw,interval = "prediction")

# Выявляем гетероскедастичность графически
qplot(data = dffires, synergy, sqrt(residuals(model_fires)^2), ylab = "модуль остатков") + stat_smooth(method = "lm")

#Тест Голдфельда-Квандта
gqtest(model_fires, order.by = ~synergy, data = dffires, fraction = 0.3)

#Тест Уайта
bptest(model_fires, data = dffires, varformula = ~poly((lm(data = dffires, abs((residuals(model_fires))) ~ model_fires$fitted.values)$fitted.values), 2))

#Поборемся с гетероскедастичностью с помощью WLS. Сначала найдем веса.
#(нашел код здесь:https://rpubs.com/mpfoley73/500818)
fires.weights <- 1/(lm(data = dffires, abs((residuals(model_fires))) ~ model_fires$fitted.values)$fitted.values^2)
model_fires_wls <- lm(data = dffires,
                      area ~ synergy+DMC+temp+RH+wind, 
                      weights = fires.weights)
summary(model_fires_wls)
vif(model_fires_wls)

#Робастные ошибке в форме Уайта
coeftest(model_fires, vcov = vcovHC(model_fires, type = "HC0"))
coeftest(model_fires, vcov = vcovHC(model_fires, type = "HC3"))
