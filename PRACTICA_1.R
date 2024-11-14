# Librerías

library(dplyr)
library(janitor)
library(visdat)
library(corrplot)
library(tidyr)
library(ggplot2)
install.packages("visdat")
install.packages("janitor")
install.packages('knitr')
install.packages("rmarkdown")



# Cargamos los datos

data <- read.csv("C:/Users/pipes/OneDrive/Documentos/R Estadistica/P1-BIKES-STUDENTS/p1_train.csv", sep = ";")


# install.packages("remotes")
remotes::install_github("sfirke/janitor")
# or from r-universe
install.packages("janitor", repos = c("https://sfirke.r-universe.dev", "https://cloud.r-project.org"))

# GRAFICAMOS

ggplot(data, aes(x = count)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribución de la Demanda (count)", x = "Número de bicicletas alquiladas", y = "Frecuencia")


data_long <- data %>% 
  select(temp, atemp, humidity, windspeed, count) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot de Variables Continuas para Detectar Outliers", x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none")

#vamos a ver la correlacion entre las variables 

round(cor(data),2)
numeric_data=data %>% select(where(is.numeric))
round(cor(numeric_data), 2)


correlacion=round(cor(numeric_data),2)

corrplot(correlacion, method="number", type="upper")

#en el grafico de correlacion podemos ver que podemos quitar la variable atemp o la variable temp

# Función para identificar outliers basados en IQR
identify_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data %>% filter(data[[column]] < lower_bound | data[[column]] > upper_bound)
}

outliers_count <- identify_outliers(data, "count")
print(outliers_count$count)


conteo <- table(outliers_count$count)
print(conteo)


# Función para identificar y eliminar outliers usando el rango intercuartílico (IQR)
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filtrar datos dentro de los límites
  data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

datos_limpios <- remove_outliers(data, "count")

dim(datos_limpios)



'--------------------------------------------------------------------------------------------------------------------'

# PRE-PROCESADO DE DATOS

data <- data %>% 
  clean_names()

vis_dat(data)

data %>% 
  get_dupes()

'''data <- data %>% 
  distinct()'''

summary(data)

data %>% 
  summarise(across(everything(), n_distinct))

'''data <- data %>% 
  mutate(across(where(is.character), as.factor))'''

anyNA(data)