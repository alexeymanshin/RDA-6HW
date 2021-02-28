# установка пакета
install.packages("dplyr")
# установка пакета
install.packages("data.table")
# ответ на запрос установщика для продолжения оперции
yes
# установка пакета
install.packages("tidyr")
#проверяем подключение устанленного пакета
require(tidyr)

# 2 Добавить датафрейм. Добавить столбец с новым параметром машины
library(datasets)
cars<-mtcars
head(cars)
tail(cars)
summary(cars)
colnames(cars)

# новый столбец содержит инфу по количеству лошадей на цилиндр двигателя
class(cars$hp)
class(cars$cyl)
cars$hp_per_cyl <- cars$hp/cars$cyl
head(cars)
colnames(cars)

# 3 функция определения цены машины на основе параметров

car_price_for_parameters <- function(hp, gear, cyl)
{
  (cars$hp*10000 + cars$gear*3000 + cars$cyl*15000)*1.18
}

cars$prices<-car_price_for_parameters()

colnames(cars)

# 4 исследовать зависимость параматров автомобилей
head(cars)
tail(cars)
summary(cars)

cor(mtcars)
pairs(mtcars) #вопрос как можно интепретировать полученный результат?

# 5 Создать временной ряд со значениями продаж

sales <- round(rnorm(60, 150, 15), 0)
salests <- ts(sales, start = c(1990, 1), frequency = 12)

print(salests)

plot(salests)
plot(sales)

diffts <- ts(seq(10000, 90000, 1000), start = c(1990, 1), frequency = 12)
result <- diff(diffts, 1)

print(diffts)
plot(diffts)

print(result)
plot(result)