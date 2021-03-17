library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(plotly)
library(lubridate)

prices<-read_excel('prices_for_years.xls', sheet='Цены') # загружаем данные 
colnames(prices) #проверяем названия столбцов

clean_prices<-prices # записываем df в виде новой переменной
colnames(clean_prices)<-as.vector(clean_prices[2,]) # корректируем названия столбцов
clean_prices<-clean_prices[4:89,] # отсекаем лишние строки
clean_prices<-gather(clean_prices, 'year', 'value', 2:ncol(clean_prices)) #собираем данные по годам в виде одной переменной

sapply(clean_prices, class) #проверяем тип данных
clean_prices<-clean_prices %>% group_by(year, ПОКАЗАТЕЛИ) %>% mutate(year = as.integer(year)) %>% mutate(value = as.numeric(value)) #преобразовываем данные

sapply(clean_prices, class) #проверяем изменения типа данных

clean_prices_no_na<-drop_na(clean_prices)
is.na(clean_prices_no_na) #проверяем есть ли пропуски в столбце value
is.null(clean_prices_no_na) #проверяем есть ли пустые значения в столбце value

clean_prices_no_na2<-na.approx(zoo(clean_prices$value)) # вариант заполнения пустых значений через ф-ю zoo
is.na(clean_prices_no_na2)
clean_prices_no_na2_grouped<-clean_prices_no_na2 %>% group_by(year, ПОКАЗАТЕЛИ) #при группировке возникает ошибка нет подходящего метода для 'group_by' применяемого к объекту класса "zoo"
#подскажите, как сгруппировать такой датафрейм?

df1<-clean_prices_no_na %>% filter(ПОКАЗАТЕЛИ=="Сахар-песок" | ПОКАЗАТЕЛИ=="Рис шлифованный") %>% group_by(year, ПОКАЗАТЕЛИ)

plot <- ggplot(df1, aes(x=year, y=value, fill=ПОКАЗАТЕЛИ))+geom_col()+
  labs(title='Динамика Цен по Годам',
       x='Год',
       y='Цена в рублях'
       )

boxplot(clean_prices_no_na$value) #диаграмма построилась
#ggplot(clean_prices_no_na) + geom_histogram(mapping = aes(x = value), binwidth = 5, color = "black") #попробовал построить диаграмму на выбросы,
#но диаграмма не появляется, почему? Хотя ошибка не появляется, есть белое поле плота с координатной сеткой, а  значений нет (подождал долго)

sber<-read.csv("opendata_sberbank.csv", fileEncoding = "Windows-1251")

colnames(sber) #проверяем названия столбцов: все в порядке

#проверяем na значения
is.na(sber) #проверяем есть ли пропуске в столбце value: [ достигнута getOption("max.print") -- пропущено 74592 строки ]
filter(sber, value==na) #проверяем есть ли пропуске в столбце value: значений не найдено
filter(sber, region==na) #проверяем есть ли пропуске в столбце value: значений не найдено
filter(sber, date=na) #проверяем есть ли пропуске в столбце value: значений не найдено
summary(sber) #проверяем датасет

#преобразовываем данные и извлекаем из даты год
clean_sber<-sber %>% mutate(apps = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d'))
class(clean_sber$date)
class(clean_sber$apps)
clean_sber_year<- clean_sber %>% mutate(year=year(date))

#отбираем данные
clean_sber_year_sl<-select(clean_sber_year, name, region, apps, year)
clean_sber_grouped <- group_by(clean_sber_year_sl, region, year)
clean_sber_summary <- summarise(clean_sber_grouped, apps=sum(apps))
clean_sber_summary

# Я долго шел к тому, чтобы написать все через pipe:
master_sber<-sber %>% mutate(apps = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d')) %>% mutate(year=year(date)) %>%
select(region, apps, year) %>% group_by(region, year) %>%
summarise(apps=sum(apps))
master_sber

# Объединяем таблицы
df_fin <- inner_join(master_sber, clean_prices_no_na, by = 'year')
pairs(df_fin)#Ошибка в pairs.default(df_fin) :у 'pairs' нечисловой аргумент
cor(df_fin) #выдает: Ошибка в cor(df_fin) :'x' должен быть числом
prices_Bread<-filter(df_fin, ПОКАЗАТЕЛИ=='Хлеб и булочные изделия из пшеничной муки высшего сорта') 

ggplot(df_fin) + geom_point(mapping = aes(x = apps, y = value), alpha=0.5) #исходя и этого плота: когда цены низкия - заявок на кредиты мало, а когда высокие - наоборот
ggplot(prices_Bread) + geom_point(mapping = aes(x = apps, y = value), alpha=0.5) # в разрезе цен на хлеб - тоже самое: цены высокие - идем в банк

# В ДЗ указано рассчитать покупательную способность и зависимости с ценами, но в датасете сбера не данные о доходах или зп, а заявки на кредиты. Возможно, я что-то пропустил:)
