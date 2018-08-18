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




