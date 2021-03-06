# библиотеки
```{r}
set.seed(9)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(recommenderlab)
```

# проитали json и привели в божеский вид
```{r}
df <- fromJSON("~/couchsurf/infol1.json", flatten=TRUE)
df <- bind_rows(df, .id = 'id_friend')
```

#оставили только айди друганов и их страны
```{r}
attach(df)
df = select(df, id_friend, about.countries.visited)
df = filter(df, about.countries.visited != "list()" )

#about.countries.lived, referenceCount, status, gender, publicAge, publicName, publicAddress.description)

# разлистили (перевели из list в обычный формат)
df = unnest(df, about.countries.visited)
df = select(df, id_friend, name)
```

#дамифицировали
```{r}
data_without_id = select(df, name)
id = select(df, id_friend)


dmy = dummyVars("~.", data = data_without_id)
data.main = data.frame(predict(dmy, newdata = data_without_id))
data.main = bind_cols(id, data.main)
data.main = data.main %>% group_by(id_friend) %>% summarise_each(funs(sum))
data.main$id_friend = as.numeric(data.main$id_friend)
```

# попытались перевести данные в формат realRatingMatrix
```{r}
data_matrix = as.matrix(data.main)
data_matrix = as(data_matrix, "realRatingMatrix")
```

# разделение на тестовую и обучающую выборки
```{r}
test_ind <- sample(1:nrow(data_matrix), size = nrow(data_matrix)*0.2)
recc_data_train <- data_matrix[-test_ind, ]
recc_data_test <- data_matrix[test_ind, ]
```

# попытка построить рекоммендательную систему
```{r}
recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model

model_details <- getModel(recc_model)
model_details$description
model_details$sim[1:5, 1:5]

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = 6)
recc_predicted

str(recc_predicted)

recc_user_1 <- recc_predicted@items[[1]]
recc_user_1
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1
```

