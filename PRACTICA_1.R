# Librerías

library(dplyr)
library(janitor)
library(visdat)
library(tidyr)
install.packages("visdat")
install.packages("janitor")
install.packages('knitr')
install.packages("rmarkdown")
library(ggplot2)

# install.packages("remotes")
remotes::install_github("sfirke/janitor")
# or from r-universe
install.packages("janitor", repos = c("https://sfirke.r-universe.dev", "https://cloud.r-project.org"))


# PRE-PROCESADO DE DATOS

# Cargamos los datos

data <- read.csv("C:/Users/pipes/OneDrive/Documentos/R Estadistica/P1-BIKES-STUDENTS/p1_train.csv", sep = ";")

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
