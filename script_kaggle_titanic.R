# Script de TITANIC KAGGLE COMPETITION# 

library(easypackages)
my_packages <- c("tidyverse")
libraries(my_packages)


# Loading the data


train <- read_csv("train.csv")

# Observamos la tabla de datos

glimpse(train)

# Recodificamos la variable sexo

rcd_train <- train %>%
  mutate(Sex = as.numeric(fct_recode(Sex, "1" = "male" , "2" = "female")))


# Hay que ver cuales son las variables que tienen más valores perdidos

per



plot_Missing(rcd_train)


# Observamos


View(rcd_train)



dta_conv <- train




pr <- glm(Survived ~ Pclass + Sex + log(Age) + Fare^2, data = train, family = binomial(link = "logit"))

prob.ajustadas <- predict(pr, type = "response", se.fit = TRUE)

# Valores ajustados

head(prob.ajustadas[[1]])

# prediccion con un punto de corte de .5

prediccion <- ifelse(fitted.values(pr) >= 0.5, 1, 0)


length(prediccion)

sin_eda <- train %>%
  filter(!is.na(Age))




table(sin_eda$Survived, prediccion)

tabla.clasif <- table(sin_eda$Survived, prediccion)
tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc



prediccion


# First we have to know what kind of data and the quality that we have. 

glimpse(train)


# Cuantos NA's hay en los datos

sapply(train, function(x){sum(is.na(x))})


# Observamos que las variables que tienen mayor cantidad e NA's son las variables
# "CABIN", "AGE", "EMBARKED" con 687, 177, 2 valores perdidos consecutivamente. 


train_dt <- train %>%
  mutate(Sex = as.factor(Sex),
         Survived = as.factor(Survived),
         Pclass = as.ordered(Pclass))


# Cuantas personas sobrevivieron a la tragedia del Titanic. 

ggplot(train_dt[!is.na(train_dt$Survived),], aes(x = factor(Survived), fill = factor(Survived))) +
  geom_bar(stat='count') +
  labs(x = 'How many people died and survived on the Titanic?') +
        geom_label(stat='count',aes(label=..count..), size=7) +
        theme_grey(base_size = 18)



# Cuantas de las personas por sexo 


ggplot(train_dt, aes(x = Sex, fill = Survived)) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  theme_grey() +
  labs(x = 'All data') + 
  geom_label(stat = 'count', aes(label = ..count..))




# Lo primero que observamos es que doce variables de las cuales 7 variables son 
# numericas y las otras son cualitativas.

# Vamos a trabajar con las variables cualitativas tomando como referencia el kernel posteado en 
# kaggle por "Megan Risdal" se puede revisar en el siguiente link


dta_conv$title <- gsub(pattern = "(.*, )|(\\..*)", replacement = " ", dta_conv$Name)


lalo <- train %>%
  mutate(title =  str_trim(str_remove(string = Name, pattern = "(\\)")))
  
pr <- train %>%
  filter(str_detect(Name,"Elizabeth"))


dta_conv <- train %>%
  mutate(surname =  str_extract(Name, "(.*,)|(\\..*)"),
         title = str_extract(Name, "(,.\\D+\\.)"),
         title = str_remove(title, "(,.)"),
         title = str_remove(title, ("\\.+")))


# Ahora vamos a relacionar las tablas 

dta_conv %>%
  select(Sex, title) %>%
  filter(Sex %in% c("female")) %>%
  table()

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + 
    scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") +
    theme_light() + ylab("") + xlab("") + ggtitle(title)
}



temp_df <- as.data.frame(ifelse(is.na(train),0,1)) # Esta parte de la función convierte los valores perdidos en ceros
# y unos, si es un valor perdido es cero, si no es valor perdido es uno

temp_df <- temp_df[,order(colSums(temp_df))] # Parece que esta función ordena las columnas según la suma de los 
# Valores perdidos


data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df))) # Esta funcion hace primero una tabla de datos
# con dos columnas una llamada "x" y otra llamada "y" transpone las columnas para que se puedan dibujar en una sola gráfica


data_temp$m <- as.vector(as.matrix(temp_df)) # Esta columna agrega los unos y ceros que se habian calculado previamente 
# si eran valores perdidos o no


data_temp <- data.frame(x=unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))

# Esta le quita el formato de lista


plot_Missing(train[,colSums(is.na(train)) > 0])


glimpse(train)


```{r}




```




En la siguiente gráfica por ejemplo podemos observar diferentes aspectos relacionados con los pasajeros del Titanic y sus probabilidades de supervivencia. 

En el primer rectángulo están los pasajeros en primera clase, el color turquesa representa la proporción de
pasajeros que no sobrevivieron y en color naranja la proporcion que si lo hicieron. 

Observamos por ejemplo que si eres un varón y viajas en primera, segunda o tercera clase tienes pocas prosibilidades de sobrevivir sin embargo si hay una diferencia significativa si eres varon y viajas en tercera clase a si eres varon y viajas en primera clase. 

Entonces podemos suponer que las variables sexo y clase son variables relevantes dentro de nuestro análisis.


```{r}


rf <- randomForest(factor(Survived) ~ Pclass + !is.na(train$Age) , data = train)

plot(rf, ylim=c(0,0.36))

```



```{r}

ref(train)

contrasts(factor(train$Sex))
levels(train$Sex)
```



Antes de realizar cualquier transformación a los datos vamos a correr una regresión logística para evaluar la capacidad predictiva de las variables que tenemos a la mano y que no tienen valores perdidos, posteriormente vamos a utilizar otras variables haciendo algunos cambios. 

```{r}

mod1 <- glm(Survived ~ Pclass + Sex + Fare + Parch + SibSp  , family = binomial(link = "logit"), data = train)

summary(mod1)

```

Después de correr el modelo obtenemos un resumen del mismo que nos permite observar las variables más relevantes por ejemplo observamos que las variables **Pclass** que se refiere a la clase en la que viajaban los pasajeros, el sexo y si tenían familiares **SibSp** son las variables que tienen una influencia estadísticamente significativa en el modelo de regresión. 


```{r}
confint.default(mod1, level = 0.95)

```

Si el intervalo de confianza incluye el 0, significa que al nivel $alpha$ elegido no se podría rechazar la hipótesis nula de que betar=0. En este caso los intervalos de confianza que incluyen al cero son **Fare**, y **Parch**. 


```{r}


exp(confint.default(mod1, level = 0.95))


```


En este caso todas las variables caen dentro del intervalo de confianza. 

# Valores ajustados, predicciones del modelo y residuos


En R podemos obtener las probabilidades ajustadas para el modelo las predicciones y los residuos a través de la función **fitted.values** y también la función **predict**
  
  ```{r}

head(mod1$fitted.values)
head(predict(mod1, type = "response"))


```

Otra forma de evaluar el modelo es determinar la capacidad de clasificar los casos individuales. 


```{r}


prediction <- if_else(fitted.values(mod1)>=.5,1,0)
table(prediction)


```

Y la tabla de clasificación sería la siguiente. 

```{r}

table(train$Survived, prediction)


```

En este caso tenemos 478 verdaderos positivos, 71 falsos negativos, 110 falsos positivos, 232 verdaderos negativos. 


Es decir clasificamos incorrectamente 181 casos. Podemos calcular la tas de clasificaciones correctas de la siguiente manera


```{r}

tabla.clasif <- table(train$Survived, prediction)
tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc


```

tenemos un 79% de clasificaciones correctas según nuestros parámetros de clasificación

Ahora vamos a observar la distribución de las variables cuantitativas.


También podemos utilizar la librería **_ROCR_** que nos permitirá analizar varias medidas relacionadas con la tabla de clasificación y, representarla gráficamente. 


```{r}
library(ROCR)

pred <- prediction(fitted.values(mod1), train$Survived)

perf <- performance(pred, measure = "acc")


#El punto de corte que maximiza "acc" es

(posicion.max <- sapply(perf@y.values, which.max))


(punto.corte <- sapply(perf@x.values, "[", posicion.max))


```

Podemos obtener una gráfica con la tasa de clasificaciones correctas. 


```{r}
plot(perf, col = "darkred")
abline(h = 0.8, lty = 2)
abline(v=punto.corte, lty = 2)
```



Otra forma de evaluar al modelo es representar la fraccion de falsos positivos a través de la curva ROC. Se considera que un modelo es mejor que otro si la curva ROC se acerca al borde superior izquierdo o lo que es lo mismo, que el área bajo la curva se a mayor. 


```{r}

AUC <- performance(pred, "auc")
AUC@y.values


```


```{r}

perf <- performance(pred, "tpr", "fpr")
plot(perf2, colorize = TRUE)
abline(a=0, b=1)
text(0.4, 0.6, paste(AUC@y.name, "\n", round(unlist(AUC@y.values), 3)), cex = 0.7)

```



Podemos representar en un mismo gráfico en un mismo gráfico las curvas ROC de diferentes modelos, lo que permite una comparación rápida de la eficacia de cada modelo. 



```{r}
mod2 <- glm(Survived ~ Pclass  , family = binomial(link = "logit"), data = train)

pred2 <- prediction(fitted.values(mod2), train$Survived)

perf3 <- performance(pred2, measure = "tpr", "fpr")
plot(perf2, col = "darkred")
abline(a = 0, b = 1)
plot(perf3, col = "darkblue", lty = 2, add = TRUE)


```

Ya observamos que las variables disponibles dentro del modelo nos permiten realizar predicciones acerca de los pasajeros del Titanic, sin embargo no hemos realizado un análisis a profundidad de las mismas para saber de qué manera se comportan. Y posterior a las transformaciones vemos si mejoran el modelo las variables que estamos utilizando actualmente son _Pclass_, _Sex_, _Fare_, _Parch_, _SibSp_ 





