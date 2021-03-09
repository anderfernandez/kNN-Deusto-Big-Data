
########################################################
# Conociendo algoritmos de Machine Learning con R: kNN #
# Autor: Ander Fernández Jauregui                      #
# Contacto: ander@anderfernandez.com                   #
########################################################


# Programar el algoritmo kNN desde 0 --------------------------------------

################################################
# 1. Creo la distancia euclídea                #
################################################

euclidean_distance = function(x1, x2){
  sqrt(sum((x1-x2)^2)) 
}

euclidean_distance(1:20, 11:30)


################################################
# 2. Encuentro los vecinos más cercanos        #
################################################
nearest_neighbors = function(x,obs, k, FUN){
  
  # Calculamos las distancias  
  dist = apply(x,1, FUN,obs) 
  
  # Encontramos los vecinos más cercanos
  distances = sort(dist)[1:k]
  neighbor_ind = which(dist %in% sort(dist)[1:k])
  
  return(neighbor_ind)
}


# Buscamos los vecinos más cercanos de una observación del dataset iris
data.pred = iris[1,1:4]
data.train = iris[2:nrow(iris), ]

nn = nearest_neighbors(x = data.train[,1:4],
                       obs = data.pred,
                       k = 3,
                       FUN = euclidean_distance
                       )
nn

# Comparo el dato de la observación con sus vecinos
data.pred
data.train[nn,]

################################################
# 3. Creo la función de predicción             #
################################################
knn_prediction = function(x,y){
  
  x = as.matrix(x) 
  
  # Calculamos la predicción en clasificación
  if(is.factor(x[,y]) | is.character(x[,y])){    
    groups = table(x[,y]) 
    pred = names(groups[groups == max(groups)])
  } 
  
  # Calculamos la predicción en regresión
  if(is.numeric(x[,y])){ 
    pred = mean(x[,y])
  } 
  
  return(pred)
}

# Hago la predicción
knn_prediction(iris[nn,], "Species")


##################################################################
# 4. Creo mi función de kNN que combina todo el proceso anterior #
##################################################################

knn = function(x, obs, y, k,FUN){
  
  train_columns = which(colnames(x) != y)
  
  nn = nearest_neighbors(x = x[,train_columns], 
                         obs = obs,
                         k = k,
                         FUN = FUN
                         )
  
  prediccion = knn_prediction(x[nn,], y = y)
  
  return(prediccion)
  
}

# Uso el algoritmo que acabo de crear para hacer el mismo proceso anterior
# En este caso, en vez de usar 3 funciones, usaremos solo 1
knn(x = data.train,
    obs = data.pred,
    y = "Species",
    k = 4,
    FUN = euclidean_distance
    )


# Como vemos, el algoritmo ha hecho una buena clasificación. 
# Ahora, podríamos probar el algoritmo con una variable numérica

 
################################################
# 5. kNN aplicado a una variable numérica      #
################################################

# Vamos a predecir la esperanza de vida usando la población y el PIB per Cápita
# install.packages("gapminder")

library(gapminder)

gapminder

columnas_elegir = c( "lifeExp","pop","gdpPercap")
datos_knn_num = gapminder[gapminder$year == "2007", columnas_elegir]


data.pred = datos_knn_num[5,]
data.train = datos_knn_num[6:nrow(datos_knn_num), ]

# Hacemos la predicción
knn(x = data.train,
    obs = data.pred,
    y = "lifeExp",
    k = 4,
    FUN = euclidean_distance
)

# Comparamos la predicción con el valor real
data.pred$lifeExp


summary(data.train)

# Vemos que en este caso la diferencia es un poco grande... ¿qué pasa? 


################################################
# 6. Normalización de datos                    #
################################################

# Creamos la función de normalizar
normalizar = function(x){
  norm = (x - min(x))/(max(x) - min(x))
  return(norm)
}

# Aplicamos la función a nuestros datos (sin aplicar a la variable objetivo)
datos_knn_num[,2:3] = apply(datos_knn_num[, 2:3],2, normalizar)


# Repetimos el proceso
data.pred = datos_knn_num[5,]
data.train = datos_knn_num[6:nrow(datos_knn_num), ]

knn(x = data.train,
    obs = data.pred,
    y = "lifeExp",
    k = 4,
    FUN = euclidean_distance
)

data.pred$lifeExp


# Simplemente escalando los datos mejoramos la capacidad predictiva, 
# ya que con los datos escalados calculamos mejor las distancias.
# Ahora bien, hasta ahora la elección de k ha sido completamente arbitraria. 
# ¿Cómo podemos elegir el número de k?


################################################
# 7. Elección del número de k                  #
################################################

# Si elegimos un número de k muy alto, la predicción será siempre la misma: 
#   - En el caso de clasificación, la moda (valor que más aparece)
#   - En el caso de regresión la media.

# Ejemplo con k alta:

# Hacemos la predicción
knn(x = data.train,
    obs = data.pred,
    y = "lifeExp",
    k = nrow(data.train),
    FUN = euclidean_distance
)

# Comparamos con la media
mean(data.train$lifeExp)

# Con una k muy grande, tendemos al underfitting. 
# Sin embargo, con un k muy pequeña, predeciremos lo mismo que un vecino.
# Esto nos llevará a casos de overfitting. 


# Método 1


## 1. Aplicamos la fórmula
k = sqrt(nrow(data.train))
k

## 2.Redondeamos el valor
k = round(k)
k

## 3.Hacemos la predicción
knn(x = data.train,
    obs = data.pred,
    y = "lifeExp",
    k = k,
    FUN = euclidean_distance
)

data.pred$lifeExp


################################################
# 8. Usar kNN en R                             #
################################################

# Eliminamos la función anterior para que no moleste
rm(knn)

# install.packages("FNN")
library(FNN)

# Regresión con kNN

## 1.Creamos los datasets, igual que antes
library(gapminder)
columnas_elegir = c( "lifeExp","pop","gdpPercap")
datos_knn_num = gapminder[gapminder$year == "2007", columnas_elegir]
data.pred = datos_knn_num[5,]
data.train = datos_knn_num[6:nrow(datos_knn_num), ]

## 2.Modificamos los datos para que se ajusten a la función
labels = data.train$lifeExp 
data.train$lifeExp = NULL
data.pred$lifeExp = NULL

## 3. Hacemos la predicción
knn.reg(
  data.train, 
  data.pred,
  y = labels,
  k = 6
)

# Clasificación con kNN

## 1.Creamos los datasets
data.pred = iris[1,]
data.train = iris[2:nrow(iris), ]

## 2.Modificamos los datos para que se ajusten a la función
labels = data.train$Species
data.train$Species = NULL
data.pred$Species = NULL


FNN::knn(
  data.train, 
  data.pred,
  cl = labels,
  k = 3
)

















