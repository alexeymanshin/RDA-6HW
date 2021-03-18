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
library(BBmisc)
library(caret)

sber<-read.csv("opendata_sberbank.csv", fileEncoding = "Windows-1251")
sber<-as.data.frame(sber)
colnames(sber) #проверяем названия столбцов: все в порядке

summary(sber)

#сконвертировали год и отобрали данные
master_sber<-sber %>% filter(region!='Россия') %>% mutate(value = as.integer(value)) %>% mutate(date = as.Date(date,'%Y-%m-%d')) %>% mutate(year=year(date)) %>% 
  group_by(name, year) %>%
  summarise(value=mean(value)) #взял среднюю так как не ясно как формируются данные наростающи итогом или за каждый месяц в отдельности
master_sber

class(master_sber$year)

# Развернем датасет
df_sber<-spread(master_sber, name, value)

# Перенесем дату в название строк, чтобы подготовить данные к моделированию
rownames(df_sber) <- as.vector(df_sber[,1]) # регион на нулевой столбец: Ошибка в `.rowNamesDF<-`(x, value = value) :
#неправильная длина 'row.names'
#Вдобавок: Предупреждение:
#Setting row names on a tibble is deprecated. 
ready_prices <- df_sber[,3:ncol(df_sber)] # срезали лишние столбцы, но на первом дата перешла в числовое значение
View(ready_prices)

for (i in 1:ncol(ready_prices)) {
  ready_prices[,i] <- as.vector(na.approx(zoo(ready_prices[,i]), rule = 2))
}

summary(ready_prices)

cor_m<-cor(ready_prices)
corrplot(cor_m, method='number') #попробовал метод из лабораторной, но получилось не очень. Подскажите как исправить

ggplot(ready_prices) + geom_histogram(mapping = aes(x = 'В среднем депозитов в руб. на человека'), binwidth = 5, color = "black") #не совсем получилось посмотреть распределение:)

library(BBmisc)
ready_prices_n <- normalize(ready_prices, method = "standardize", range = c(0,1))
View(ready_prices_n)

# Разделим выборку на обучающую и тестовую
library(caret)
set.seed(123)
training.samples <- ready_prices_n$`Количество заявок на потребительские кредиты` %>% createDataPartition(p = 0.75, list = FALSE)

train.data <- ready_prices_n[training.samples,]
test.data <- ready_prices_n[-training.samples,] # в тестовом сете не остается ничего. Пробовал менять 0.75 на 0.5 но результат такой же. Почему?

corr_check <- ready_prices_n %>% select(loan = `Количество заявок на потребительские кредиты`,
                                        dep_quant = `В среднем депозитов в руб. на человека`,
                                        pension = `Средняя пенсия`,
                                        restaran = `Средние траты в ресторане`,
                                        salary = `Средняя зарплата`, 
                                        dep_amount = `Средняя сумма нового депозита`)

library(corrplot)
corrMatrixCheck <- cor(corr_check)
corrplot(corrMatrixCheck, method = 'number')

library(GGally)
ggpairs(corr_check)

View(train.data)

model <- lm(`Количество заявок на потребительские кредиты` ~ `В среднем депозитов в руб. на человека` + `Средняя пенсия` + `Средняя зарплата`, data = train.data)

summary(model)

result <- predict(model, test.data)
resultdf <- data.frame(date = names(result), result = result, type = "Модель", stringsAsFactors = FALSE)
resultdf <- rbind(resultdf, data.frame(date = rownames(test.data), result = test.data$`Количество заявок на потребительские кредиты`, type = "Тестовая выборка", stringsAsFactors = FALSE))

library(ggplot2)
ggplot(resultdf, aes(x = date, y = result, color = type)) +
  geom_point() + 
  theme_light() +
  theme(legend.position = 'bottom') + 
  labs(x = "Дата",
       y = "Потребительские кредиты",
       title = "Проверка результатов модели",
       fill = "Тип")

View(resultdf) # так как датасет не разделился и тестовая выборка пустая - результаты посмотреть не получилось:.(
