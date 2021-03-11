library(ggplot2)
library(tidyverse)

library(datasets)
library(nycflights13)

#Создаем выборку из 1000 записей как указано в задании
flights2<-sample_n(flights,1000)

# 1 Каких задержек больше всего

ggplot(flights2) + geom_histogram(mapping = aes(x = arr_delay), binwidth = 5, color = "white") #смотрим гистонрамму встречающихся значений
nrow(filter(flights2, dep_delay > 200)) # смотрим сколько значений >300
nrow(filter(flights2, dep_delay < -30)) # смотрим сколько значений >-15
flights2 %>% filter(dep_delay < 200) %>% nrow()

ggplot(filter(flights2, dep_delay > -15 & dep_delay < 200)) + geom_boxplot(mapping = aes(x = dep_delay)) # проверяем есть ли выбросы
ggplot(filter(flights2, dep_delay > -15 & dep_delay < 200)) + geom_histogram(mapping = aes(x = arr_delay), binwidth = 5, color = "white") #смотрим гистонрамму наиболее часто встречающихся значений

# 2 Зависимость задержек от различных переменных

ggplot(flights2 %>% group_by(dep_time) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = dep_time, y = dep_delay, fill = dep_time)) #зависимость между задержкой и временем вылета. Задержки появляются в интервале от 07.00 до 23.00

ggplot(filter(flights2, dep_delay > -15 & dep_delay < 200)) + geom_point(mapping = aes(x = carrier, y = dep_delay)) # смотрим какие значения встречаются у авиалиний

ggplot(flights2 %>% group_by(carrier) %>% summarise(dep_delay = dep_delay)) + geom_jitter(mapping = aes(x = carrier, y = dep_delay, fill = carrier)) #зависимость между задержкой и авиалинией

ggplot(filter(flights2, distance<3000)) + geom_point(mapping = aes(x = distance, y = dep_delay), alpha=0.5) #зависимость между задержкой и дистанцией полета
ggplot(filter(flights2, distance<3000)) + geom_jitter(mapping = aes(x = distance, y = dep_delay)) #зависимость между задержкой и дистанцией полета

ggplot(flights2 %>% group_by(month) %>% summarise(delay = dep_delay)) + geom_point(mapping = aes(x = month, y = delay, fill = month)) # диаграмма значений по месяцам
ggplot(flights2 %>% group_by(month) %>% summarise(av_delay = mean(dep_delay))) + geom_col(mapping = aes(x = month, y = av_delay, fill = month)) # диаграмма средних значений по месяцам

ggplot(flights2) + geom_boxplot(mapping = aes(x = as.factor(month), y = dep_delay)) # разброс значений по месецам, значения больше 200 похоже на выбросы или случаются крайне редко

# 3 Зависимость задержки от погодных условий

flights_x_weather <- flights %>% right_join(weather, by=c("year","month","day","hour","origin")) # Объединяем датасеты по нескольким столбцам

ggplot(sample_n(flights_x_weather,10000) %>% group_by(visib) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = visib, y = dep_delay, fill = visib))
ggplot(sample_n(flights_x_weather,10000) %>% group_by(temp) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = temp, y = dep_delay, fill = temp))
ggplot(sample_n(flights_x_weather,10000) %>% group_by(pressure) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = pressure, y = dep_delay, fill = pressure))
ggplot(sample_n(flights_x_weather,10000) %>% group_by(precip) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = precip, y = dep_delay, fill = precip))
ggplot(sample_n(flights_x_weather,10000) %>% group_by(wind_gust) %>% summarise(dep_delay = dep_delay)) + geom_point(mapping = aes(x = wind_gust, y = dep_delay, fill = wind_gust))

# Построанные диаграммы не выявляют явной зависимости от погодных условий. Зависимость прослеживается скорее от сезонности: летом - отпуски, а зимой - Рождество.

# 4 Столбчатая диаграмма, отражающую количество задержек в разрезе месяца...

flights3 <- flights %>% mutate(dep_type = ifelse(dep_delay < 10, "on time", "delayed"))
qplot(x = month, fill = dep_type, data = flights, geom = "bar", main = "Frequency of Delayed vs On Time Arrivals by Month")
ggplot(flights2 %>% group_by(month) %>% summarise(avg_delay = mean(dep_delay))) + geom_col(mapping = aes(x = month, y = avg_delay, fill = month)) # диаграмма средних значений по месяцам
# Самые долгие задержки в декабре, июне и июле. Предположительно потому, что в декабре Рождество и зима: задержки могут быть связаны с Рождественскими праздниками.
#Июнь - середина августа - месяцы летние и здесь предполагаю наплыв туристов-отпускников.


# 5 Повторить диаграмму температур в разрезе месяца
df<-select(weather, month, day, temp, wind_speed, hour)
temp_wind_may<-filter(df, month==5, hour >6 & hour < 19)

temp_grouped <- group_by(temp_wind_may, day, hour)
temp_summary <- summarise(temp_grouped, max_temp_C = max(temp))

fin_df<-temp_wind_may %>% group_by(day) %>% summarise(max_temp = (max(temp)/1.8-32), min_temp = (min(temp)/1.8-32), wind_speed = mean(wind_speed)/2.237)

ggplot(fin_df) + geom_col(mapping = aes(x = day, y = wind_speed)) + geom_smooth(aes(x=day, y= max_temp))
#как добить geom и повесить лейблы над столбцами не разобрался


#ggplot(fin_df) + geom_col(mapping = aes(x = day, y = wind_speed)) + geom_smooth()
#geom_smooth()` using method = 'loess' and formula 'y ~ x'
#Ошибка: stat_smooth requires the following missing aesthetics: x and y.