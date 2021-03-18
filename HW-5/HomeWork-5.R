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
colnames(clean_prices)[colnames(clean_prices) == 'ПОКАЗАТЕЛИ'] <-'items'

sapply(clean_prices, class) #проверяем тип данных
clean_prices<-clean_prices %>% group_by(year, items) %>% mutate(year = as.integer(year)) %>% mutate(value = as.numeric(value)) #преобразовываем данные
colnames(clean_prices)[colnames(clean_prices) == 'value'] <-'price'

sapply(clean_prices, class) #проверяем изменения типа данных

clean_prices_no_na<-drop_na(clean_prices)
is.na(clean_prices_no_na) #проверяем есть ли пропуски в столбце value
is.null(clean_prices_no_na) #проверяем есть ли пустые значения в столбце value

clean_prices_no_na2<-na.approx(zoo(clean_prices$value)) # вариант заполнения пустых значений через ф-ю zoo
is.na(clean_prices_no_na2)
clean_prices_no_na2_grouped<-clean_prices_no_na2 %>% group_by(year, items) #при группировке возникает ошибка нет подходящего метода для 'group_by' применяемого к объекту класса "zoo"
#подскажите, как сгруппировать такой датафрейм?

df1<-clean_prices_no_na %>% filter(items=="Сахар-песок" | items=="Рис шлифованный") %>% group_by(year, items)

plot <- ggplot(df1, aes(x=year, y=price, fill=items))+geom_col()+
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
clean_sber<-sber %>% mutate(value = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d'))
class(clean_sber$date)
class(clean_sber$value)

clean_sber_year<- clean_sber %>% mutate(year=year(date))

clean_sber_grouped <- group_by(clean_sber_year, name, region, year)
clean_sber_summary <- summarise(clean_sber_grouped, value=mean(value))
clean_sber_summary


#отбираем данные через pipe:
master_sber<-sber %>% mutate(value = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d')) %>% mutate(year=year(date)) %>% 
group_by(name, region, year) %>%
summarise(value=mean(value))
master_sber

# Объединяем таблицы
df_fin <- inner_join(master_sber, clean_prices_no_na, by = 'year')
df_fin_tr<-spread(df_fin, items, price)

#df_fin_tr<-df_fin_tr[,3:ncol(df_fin_tr)]
#rownames(df_fin_tr)<-as.vector(df_fin_tr[,1]) # Ошибка в `.rowNamesDF<-`(x, value = value) :неправильная длина 'row.names' Вдобавок: Предупреждение: Setting row names on a tibble is deprecated. 
#cor(df_fin_tr)

prices_Bread_salary<-filter(df_fin, name=='Средняя зарплата' & items=='Хлеб и булочные изделия из пшеничной муки высшего сорта' & region=='Россия') 
df_prices_Bread_salary<-spread(prices_Bread_salary, items, price)
colnames(df_prices_Bread_salary)[colnames(df_prices_Bread_salary) =='Хлеб и булочные изделия из пшеничной муки высшего сорта'] <-'Хлеб'
ggplot(df_prices_Bread_salary) + geom_point(mapping = aes(x = value, y = Хлеб), alpha=0.5) # в разрезе цен на хлеб и средней зп - наблюдаем линейную зависимость

df_prices_Bread_salary_tr<-df_prices_Bread_salary[,3:ncol(df_prices_Bread_salary)]
cor(df_prices_Bread_salary_tr)