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


sber<-read.csv("opendata_sberbank.csv", fileEncoding = "Windows-1251")
sber<-as.data.frame(sber)
colnames(sber) #проверяем названия столбцов: все в порядке

summary(sber)

#сконвертировали год
master_sber<-sber %>% mutate(value = as.integer(value)) %>% mutate(date=as.Date(date, '%Y-%m-%d'))
#%>% group_by(name, date, region) %>% summarise(value=mean(value))

#отобрали данные
sber_monthly<-filter(master_sber, region=='Россия' & date>='2015-01-15')

class(sber_monthly$date)

# Развернем датасет
df_sber_monthly<-spread(sber_monthly, name, value)

# Перенесем дату в название строк, чтобы подготовить данные к моделированию
rownames(df_sber_monthly) <- as.vector(df_sber_monthly[,2]) # перекинули дату на нулевой столбец 
ready_prices <- df_sber_monthly[,3:ncol(df_sber_monthly)] # срезали лишние столбцы, но на первом дата перешла в числовое значение
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
test.data <- ready_prices_n[-training.samples,]

corr_check <- ready_prices_n %>% select(loan = `Количество заявок на потребительские кредиты`,
                                        deposit = `Количество новых депозитов`,
                                        restaran = `Средние траты в ресторане`,
                                        salary = `Средняя зарплата`, 
                                        pension = `Средняя пенсия`)

library(corrplot)
corrMatrixCheck <- cor(corr_check)
corrplot(corrMatrixCheck, method = 'number')

library(GGally)
ggpairs(corr_check)

View(train.data)

model <- lm(`Количество заявок на потребительские кредиты` ~ `Количество новых депозитов` + `Средние траты в ресторане` + `Средняя зарплата`, data = train.data)

summary(model)

result <- predict(model, test.data)
resultdf <- data.frame(date = names(result), result = result, type = "Модель", stringsAsFactors = FALSE)
resultdf <- rbind(resultdf, data.frame(date = rownames(test.data), result = test.data$`Количество заявок на потребительские кредиты`, type = "Тестовая выборка", stringsAsFactors = FALSE))

#res_df_tr<-spread(resultdf, type, result)

library(ggplot2)
ggplot(resultdf, aes(x = date, y = result, color = type)) +
  geom_point() + 
  theme_light() +
  theme(legend.position = 'bottom') + 
  labs(x = "Дата",
       y = "Потребительские кредиты",
       title = "Проверка результатов модели",
       fill = "Тип")

View(resultdf)

##### Кластеризация #####

# Информация по регионам

library(readr) # пакет для загрузки csv
person <- read.csv("opendata_sberbank.csv", fileEncoding = "Windows-1251")
person <- as.data.frame(person)

# Готовим датасет person
# Разделяем переменную на несколько
library(tidyr) # пакет для преобразования датафреймов 
clean_person <- spread(person, name, value) 
colnames(clean_person)[1:2] <- c("Регион", "Дата")

# Обрабатываем пропуски. 
# Так как по многим показателям нет данных за 2013-2014 год, удалим эти данные, для нашей задачи они не нужны)
library(dplyr) # пакет для работы с датафреймами
a <- as.Date("2014-12-31", format = '%Y-%m-%d')
ready_person <- filter(clean_person, `Дата` > a)
View(ready_person)
summary(ready_person)

for (i in 3:ncol(ready_person)) {
  x <- ave(ready_person[,i], ready_person$`Регион`, FUN = function(x) mean(x, na.rm = TRUE))
  ready_person[,i] <- ifelse(is.na(ready_person[,i]), x, ready_person[,i])
}

ready_person[is.na(ready_person)]

ready_person_n <- ready_person
ready_person_n[,3:ncol(ready_person)] <- normalize(ready_person_n[,3:ncol(ready_person)], method = "standardize", range = c(0,1))
View(ready_person_n)

person_for_clustering <- ready_person_n %>% filter(Дата == as.Date("2016-12-15"), Регион != "Россия")
View(person_for_clustering)
rownames(person_for_clustering) <- person_for_clustering[,1]
person_for_clustering <- person_for_clustering[,3:ncol(ready_person)] %>% as.matrix


person_kmeans <- kmeans(person_for_clustering, 3, nstart = 100)
person_kmeans


library(fpc)
plotcluster(person_for_clustering, person_kmeans$cluster)

plot(person_for_clustering, col = person_kmeans$cluster)

library(ggdendro)
library(cluster)

d <- dist(person_for_clustering, method = "euclidean")
hierarchy_person_1 <- hclust(d, method = "complete")
ggdendrogram(hierarchy_person_1, rotate = TRUE, size = 2)


hierarchy_person_2 <- hclust(d, method = "single")
ggdendrogram(hierarchy_person_2, rotate = TRUE, size = 2)
