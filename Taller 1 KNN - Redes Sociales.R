install.packages("class")

library(class)
library(caret)
library(readr)

names(Redes_Sociales)[1] = 'ID'
names(Redes_Sociales)[2] = 'Resultado'
names(Redes_Sociales)[3] = 'HorasDia'
names(Redes_Sociales)[4] = 'DiasxSemana'
names(Redes_Sociales)[5] = 'RangoEdad'

head(Redes_Sociales)

str(Redes_Sociales)

set.seed(123)

# Splitting de los datos y creacion de modelos de test y training
t.ids = createDataPartition(Redes_Sociales$Resultado, p=0.5, list = F)
train = Redes_Sociales[t.ids, ]
temp = Redes_Sociales[-t.ids, ]
v.ids = createDataPartition(temp$Resultado, p=0.5, list = F)
val = temp[v.ids, ]
test = temp[-v.ids, ]


# Definir valores de K
pred1 = knn(train[,3:5], val[,3:5], train[,2], k = 1)
pred2 = knn(train[,3:5], val[,3:5], train[,2], k = 5)
pred3 = knn(train[,3:5], val[,3:5], train[,2], k = 10)
pred4 = knn(train[,3:5], val[,3:5], train[,2], k = 25)
pred4 = knn(train[,3:5], val[,3:5], train[,2], k = 35)

# Definiendo los primeros modelos de prediccion:
conf_mat1 = base::table(val$Resultado, pred1)
conf_mat1
cat("Precision: ", sum((diag(conf_mat1))/sum(conf_mat1))*100)

conf_mat2 = base::table(val$Resultado, pred2)
conf_mat2
cat("Precision: ", sum((diag(conf_mat2))/sum(conf_mat2)) *100)

conf_mat3 = base::table(val$Resultado, pred3)
conf_mat3
cat("Precision: ", sum((diag(conf_mat3))/sum(conf_mat3)) *100)


#Testeo y entrenamiento de los modelos
set.seed(123)
t.ids = createDataPartition(Redes_Sociales$Resultado, p=0.5, list = F)
train = Redes_Sociales[t.ids, ]
temp = Redes_Sociales[-t.ids, ]
v.ids = createDataPartition(temp$Resultado, p=0.5, list = F)
val = temp[v.ids, ]
test = temp[-v.ids, ]

# Modelos con distintos valores de k
pred1 = knn(train[,3:5], val[,3:5], train[,2], k = 1)
pred2 = knn(train[,3:5], val[,3:5], train[,2], k = 5)
pred3 = knn(train[,3:5], val[,3:5], train[,2], k = 10)
pred4 = knn(train[,3:5], val[,3:5], train[,2], k = 25)
pred4 = knn(train[,3:5], val[,3:5], train[,2], k = 35)

#Resultados y su precision final

conf_mat1 = base::table(val$Resultado, pred1)
conf_mat1
cat("Precision: ", sum((diag(conf_mat1))/sum(conf_mat1))*100)

conf_mat2 = base::table(val$Resultado, pred2)
conf_mat2
cat("Precision: ", sum((diag(conf_mat2))/sum(conf_mat2)) *100)

conf_mat3 = base::table(val$Resultado, pred3)
conf_mat3
cat("Precision: ", sum((diag(conf_mat3))/sum(conf_mat3)) *100)
