install.packages("class")

library(class)
library(caret)
library(readr)


names(Prostate_Cancer)
names(Prostate_Cancer)[2] = 'Diagnostico'

head(Prostate_Cancer)

str(Prostate_Cancer)


names(Prostate_Cancer)[3] = 'Radio'
names(Prostate_Cancer)[4] = 'Textura'
names(Prostate_Cancer)[5] = 'Perimetro'
names(Prostate_Cancer)[6] = 'Area'
names(Prostate_Cancer)[7] = 'Suavidad'
names(Prostate_Cancer)[8] = 'Compactacion'
names(Prostate_Cancer)[9] = 'Simetria'
names(Prostate_Cancer)[10] = 'Dimensionfractal'


head(Prostate_Cancer)

str(Prostate_Cancer)


set.seed(123)

t.ids = createDataPartition(Prostate_Cancer$Diagnostico, p = 0.5, list = F)

train = Prostate_Cancer[t.ids, ]
temp = Prostate_Cancer[-t.ids, ]

v.ids = createDataPartition(temp$Diagnostico, p = 0.5, list = F)

val = temp[v.ids, ]
test = temp[-v.ids, ]


# Creacion de modelos 

# Definir valores de K

pred1 = knn(train[,3:10], val[,3:10], train[,2], k = 1)
pred2 = knn(train[,3:10], val[,3:10], train[,2], k = 5)
pred3 = knn(train[,3:10], val[,3:10], train[,2], k = 10)
pred4 = knn(train[,3:10], val[,3:10], train[,2], k = 25)
pred5 = knn(train[,3:10], val[,3:10], train[,2], k = 50)


# Definiendo los primeros modelos de prediccion:

conf_mat1 = base::table(val$Diagnostico, pred1)
conf_mat1
cat("Precision: ", sum((diag(conf_mat1))/sum(conf_mat1))*100)


conf_mat2 = base::table(val$Diagnostico, pred2)
conf_mat2
cat("Precision: ", sum((diag(conf_mat2))/sum(conf_mat2)) *100)


conf_mat3 = base::table(val$Diagnostico, pred3)
conf_mat3
cat("Precision: ", sum((diag(conf_mat3))/sum(conf_mat3)) *100)


# Funcion de scale para los distintos parametros

Prostate_Cancer$Radio = scale(Prostate_Cancer$Radio)
Prostate_Cancer$Textura = scale(Prostate_Cancer$Textura)
Prostate_Cancer$Perimetro= scale(Prostate_Cancer$Perimetro)
Prostate_Cancer$Area = scale(Prostate_Cancer$Area)
Prostate_Cancer$Suavidad=scale(Prostate_Cancer$Suavidad)
Prostate_Cancer$Compactacion = scale(Prostate_Cancer$Compactacion)
Prostate_Cancer$Simetria = scale(Prostate_Cancer$Simetria)
Prostate_Cancer$Dimensionfractal = scale(Prostate_Cancer$Dimensionfractal)

#Testeo y entrenamiento de los modelos

set.seed(123)
t.ids = createDataPartition(Prostate_Cancer$Diagnostico, p=0.5, list = F)
train = Prostate_Cancer[t.ids, ]
temp = Prostate_Cancer[-t.ids, ]
v.ids = createDataPartition(temp$Diagnostico, p=0.5, list = F)
val = temp[v.ids, ]
test = temp[-v.ids, ]

# Modelos con distintos valores de k

pred1 = knn(train[,3:10], val[,3:10], train[,2], k = 1)
pred2 = knn(train[,3:10], val[,3:10], train[,2], k = 5)
pred3 = knn(train[,3:10], val[,3:10], train[,2], k = 10)
pred4 = knn(train[,3:10], val[,3:10], train[,2], k = 25)
pred5 = knn(train[,3:10], val[,3:10], train[,2], k = 50)

#Resultados y su precision final

conf_mat1 = base::table(val$Diagnostico, pred1)
conf_mat1
cat("Precision: ", sum((diag(conf_mat1))/sum(conf_mat1))*100)


conf_mat2 = base::table(val$Diagnostico, pred2)
conf_mat2
cat("Precision: ", sum((diag(conf_mat2))/sum(conf_mat2)) *100)


conf_mat3 = base::table(val$Diagnostico, pred3)
conf_mat3
cat("Precision: ", sum((diag(conf_mat3))/sum(conf_mat3)) *100)
