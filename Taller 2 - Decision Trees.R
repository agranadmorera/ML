library(caret) 
library(rpart.plot) 
library(rpart) 
library(class) 

# Preparar y abrir el file csv de Social Network Ads

data = Social_Network_Ads 
data
set.seed(2018)

# Realizar la exlusion de la columna que no se va a usar 
data=subset(data,select=-c(User.ID))

# Cambiar los nombres de las columnas para mejor comprension nuestra
names(data)[1] <- "Genero"
names(data)[2] <- "Edad"
names(data)[3] <- "SalarioAprox"
names(data)[4] <- "Compras"

# Separacion de datos de test y de training
training.ids = createDataPartition(data$Compras, p = 0.5, list = F) 


# Creacion del modelo 
mod = rpart(Compras ~ .,
            data = data[training.ids,], 
            method = "class", 
            control = rpart.control(minsplit = 10, cp =0.01)) 

# "Plantando" el arbol... aqui se crea el arbol
prp(mod, type=2, extra=104, 
    fallen.leaves=TRUE, 
    shadow.col="black") 

# Uso de la funcion Scale para ver las diferencias del primer conjunto de datos
data$Edad = scale(data$Edad) 
data$SalarioEstimado = scale(data$SalarioAprox) 
data$Comprado = scale(data$Compras) 

# Creacion de modelo
mod = rpart(Comprado~ ., 
            data = data[training.ids,], 
            method = "class", 
            control = rpart.control(minsplit = 10, cp =0.01)) 

# Preparacion del grafico
prp(mod, type=2, extra=104, 
    fallen.leaves=TRUE, 
    shadow.col="black")