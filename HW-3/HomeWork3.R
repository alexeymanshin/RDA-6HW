require(tidyverse)
require(readxl)
require(dplyr)
install.packages('quantmod')

# Загрузить файлы, проанализируйте и запишите в комментариях
cafe<-read_excel('cafe.xlsx', sheet='Sheet0')

people <- read.csv2("data-6783-2020-02-17.csv", fileEncoding = "Windows-1251")

summary(cafe)
colnames(cafe)
str(cafe) # В колонках сформировались данные типа char, т.е. текстовые. Char необходимо перевести в numeric для вычислений. Есть значения N/A. 
head(cafe)
tail(cafe)

summary(people)
colnames(people)
str(people) # В колонках сформировались данные типа char, т.е. текстовые. Char необходимо перевести в numeric для вычислений. Столбец X не заполнен
# данные по некоторым районам за разные и разное кол-во периодов, необходимо агрегировать, население в тысячах, а кафе в штуках - несоответствие для при при операциях исчисления
head(people)
tail(people)
class(people$QuantityInThousandPeoples)

# Конвертируем тип данных char в numeric 
people$QuantityInThousandPeoples<-as.integer(people$QuantityInThousandPeoples)
class(people$QuantityInThousandPeoples)
class(cafe$SeatsCount)
cafe$SeatsCount<-as.integer(cafe$SeatsCount)
class(cafe$SeatsCount)

# Агрегируем данные по населению
people_grouped <- group_by(people, Territory)
people_summary <- summarise(people_grouped, AdmPop = mean(QuantityInThousandPeoples)*1000)

# Territory переименовываем в AdmArea и выполняем join
colnames(people_summary)[colnames(people_summary) == 'Territory'] <-'AdmArea'
cafe_x_people <- left_join(cafe, people_summary, by = 'AdmArea')
cafe_x_people

# Отбираем необходимые колонки
res_df<-select(cafe_x_people, Name, TypeObject, AdmArea, AdmPop, District, Address, SeatsCount)
res_df

# Попробуем через Pipe
pipe<-people %>% group_by(Territory) %>% summarise(AdmPop = mean(QuantityInThousandPeoples))
colnames(pipe)[colnames(pipe) == 'Territory'] <-'AdmArea'

cafe_x_people_via_pipe <- left_join(cafe, pipe, by = 'AdmArea')
cafe_x_people_via_pipe

res_df2<-filter(res_df, TypeObject=='кафе') #практиковался писать фильтрацию

# Агрегируем данные по кафе, группируя по территории
cafe2<-select(cafe, Name, TypeObject, AdmArea, SeatsCount)
cafe_grouped <- group_by(cafe2, AdmArea)
cafe_summary <- summarise(cafe_grouped, CafePerAdm = n(), SeatsCount=sum(SeatsCount))
cafe_summary

# Выполняем join и рассчитываем данные из дополнительного задания
cafes_and_people <- right_join(cafe_summary, people_summary, by = 'AdmArea')
cafes_and_people$CafesPerPerson<-cafes_and_people$CafePerAdm/cafes_and_people$AdmPop1000
cafes_and_people$PeoplePerSeat<-cafes_and_people$AdmPop/cafes_and_people$SeatsCount
cafes_and_people

# Попробуем через Pipe
pipe2<-cafe2 %>% group_by(AdmArea) %>% summarise(CafePerAdm = n(), SeatsCount=sum(SeatsCount))
cafes_and_people_pipe2 <- right_join(pipe2, people_summary, by = 'AdmArea')
cafes_and_people_pipe2$CafesPerPerson<-cafes_and_people$CafePerAdm/cafes_and_people$AdmPop
cafes_and_people_pipe2$PeoplePerSeat<-cafes_and_people$AdmPop/cafes_and_people$SeatsCount
cafes_and_people