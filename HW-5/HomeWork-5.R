library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(plotly)
library(lubridate)
library(tibble)
library(corrplot)
library(stringr)

prices<-read_excel('prices_for_years.xls', sheet='Цены') # загружаем данные 
colnames(prices) #проверяем названия столбцов

clean_prices<-prices # записываем df в виде новой переменной
colnames(clean_prices)<-as.vector(clean_prices[2,]) # корректируем названия столбцов
clean_prices<-clean_prices[4:89,] # отсекаем лишние строки
clean_prices<-gather(clean_prices, 'year', 'value', 2:ncol(clean_prices)) #собираем данные по годам в виде одной переменной
colnames(clean_prices)[colnames(clean_prices) == 'ПОКАЗАТЕЛИ'] <-'items'

sapply(clean_prices, class) #проверяем тип данных
clean_prices<-clean_prices %>% group_by(year, items) %>% mutate(year = as.integer(year)) %>% mutate(value = as.numeric(value)) #преобразовываем данные
colnames(clean_prices)[colnames(clean_prices) == 'value'] <-'price'

sapply(clean_prices, class) #проверяем изменения типа данных

df_clean_prices<-spread(clean_prices, items, price)

is.na(df_clean_prices)

for (i in 1:ncol(df_clean_prices)) {
  df_clean_prices[,i] <- as.vector(na.approx(zoo(df_clean_prices[,i]), rule = 2))
}


is.na(df_clean_prices) #проверяем есть ли пропуски в столбце value

df1<-clean_prices %>% filter(items=="Сахар-песок" | items=="Рис шлифованный")

#df_clean_prices<-spread(df1, items, price)

plot <- ggplot(df1, aes(x=year, y=price, fill=items))+geom_col()+
  labs(title='Динамика Цен по Годам',
       x='Год',
       y='Цена в рублях'
       ) # как прописать оси, если строить диаграмму из развернутого спредо датасета? items станут названием столбцов?

sber<-read.csv("opendata_sberbank.csv", fileEncoding = "Windows-1251")

colnames(sber) #проверяем названия столбцов: все в порядке

#проверяем na значения
is.na(sber) #проверяем есть ли пропуске в столбце value: [ достигнута getOption("max.print") -- пропущено 74592 строки ]
filter(sber, value==na) #проверяем есть ли пропуске в столбце value: значений не найдено
filter(sber, region==na) #проверяем есть ли пропуске в столбце value: значений не найдено
filter(sber, date=na) #проверяем есть ли пропуске в столбце value: значений не найдено
summary(sber) #проверяем датасет

#отбираем данные через pipe:
master_sber<-sber %>% mutate(value = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d')) %>% mutate(year=year(date)) %>% 
group_by(name, region, year) %>%
summarise(value=mean(value))
master_sber

df_master_sber<-spread(clean_prices, items, price)

# Объединяем таблицы
df_fin <- inner_join(df_master_sber, df_clean_prices, by = 'year')

for (i in 1:ncol(df_fin)) {
  df_fin[,i] <- as.vector(na.approx(zoo(df_fin[,i]), rule = 2))
}

rownames(df_fin) <- as.vector(as.character(df_fin[,2])) #Ошибка в `.rowNamesDF<-`(x, value = value) :
#неправильная длина 'row.names'
#Вдобавок: Предупреждение:
#  Setting row names on a tibble is deprecated.
# ошибка актуальна


#после разварота датафрейма не понятно как переписать синтакс
prices_Bread_salary<-filter(Средняя зарплата=='Средняя зарплата' & items=='Хлеб и булочные изделия из пшеничной муки высшего сорта' & region=='Россия') 
df_prices_Bread_salary<-spread(prices_Bread_salary, items, price)
colnames(df_prices_Bread_salary)[colnames(df_prices_Bread_salary) =='Хлеб и булочные изделия из пшеничной муки высшего сорта'] <-'Хлеб'
ggplot(df_prices_Bread_salary) + geom_point(mapping = aes(x = value, y = Хлеб), alpha=0.5) # в разрезе цен на хлеб и средней зп - наблюдаем линейную зависимость

df_prices_Bread_salary_tr<-df_prices_Bread_salary[,3:ncol(df_prices_Bread_salary)]
cor(df_prices_Bread_salary_tr)