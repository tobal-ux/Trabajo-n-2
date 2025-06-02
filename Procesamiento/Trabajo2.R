# Trabajo II 
# Integrantes: cristobal astorga, matilde hermosilla y daniela pavez 

# 0.0 Ajustes iniciales 
rm(list=ls())
options(scipen=999) 

# 1.0 Cargar librerias 
pacman::p_load(dplyr, 
               sjmisc, 
               sjPlot,
               sjlabelled, 
               kableExtra, 
               GGally, 
               corrplot,
               car,
               summarytools,
               psych, 
               haven)

# 1.1 Cargar base de datos
load(url("https://raw.githubusercontent.com/tobal-ux/Trabajo-n-2/main/Input/ola_2022.RData"))

# 2.0 Exploracion inicial de la BBDD
dim(ola_2022)
names(ola_2022)

# 2.1 Seleccion de las variables a utilizar 
proc_data <- ola_2022 %>% select(ingresos=m13, 
                                 sexo=m0_sexo, 
                                 conf_gob=c05_01, 
                                 conf_car=c05_03, 
                                 conf_poder_judicial=c05_05)
# Comprobamos el paso anterior
dim(proc_data)
names(proc_data)

# 2.3 Cambiar etiquetas

# Variable ingresos
proc_data$ingresos <- set_label(x = proc_data$ingresos,label = "Ingresos: Ingresos ")
get_label(proc_data$ingresos)

# Variable confianza en el gobierno
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

# Variable confianza en carabineros 
proc_data$conf_car  <- set_label(x = proc_data$conf_car, label = "Confianza: Carabineros")
get_label(proc_data$conf_car)

# Variable confianza en el poder judicial 
proc_data$conf_poder_judicial  <- set_label(x = proc_data$conf_poder_judicial, label = "Confianza: Poder Judicial")
get_label(proc_data$conf_poder_judicial)

# Variable sexo 
frq(proc_data$sexo)

# Recodificar
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

# Etiquetar
proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))

get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

# Revisar
frq(proc_data$sexo)

# 3.0 Descripcion general de la base de datos
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 4.0 Tratamiento de NA

# Primero vemos la frecuencia de cada variable 
frq(proc_data$ingresos)
frq(proc_data$sexo)
frq(proc_data$conf_gob)
frq(proc_data$conf_car)
frq(proc_data$conf_poder_judicial)

# 4.1 Respaldamos la base original 
proc_data_original <- proc_data
dim(proc_data)

# 4.1 Metodo elegido: listwise

# Revisamos los NA totales
sum(is.na(proc_data))

# Revisamos los NA por columnas 
colSums(is.na(proc_data))

# Borramos NA
proc_data <- na.omit(proc_data)
dim(proc_data)

# 5.0 Matrices de correlacion

# Clase de cada variable 
str(proc_data)

# Convertir sexo de factor a numerico
proc_data$sexo <- as.numeric(as.factor(proc_data$sexo))

# Convertir ingresos 
proc_data$ingresos <- as.numeric(as.character(proc_data$ingresos))

# 5.1 Correlacion
M <- cor(proc_data, use = "complete.obs")
M

# 5.2 Tabla de correlacion
sjPlot::tab_corr(proc_data, 
                 triangle = "lower")

sjPlot::tab_corr(proc_data, 
                 na.deletion = "listwise", # espeficicamos tratamiento NA
                 triangle = "lower")

# 5.3 Scatterplot
sjPlot::plot_scatter(proc_data, sexo, ingresos)

sjPlot::plot_scatter(proc_data, sexo, conf_car)

sjPlot::plot_scatter(proc_data, sexo, conf_gob)


# 6.0 Creacion de escala 
cor(proc_data)

# 6.1 Estimar alfa de cronbach 
psych::alpha(proc_data)

proc_data <- proc_data %>% 
  rowwise() %>% 
  mutate(confianza_instituciones = sum(conf_gob,conf_car,conf_poder_judicial))
summary(proc_data$confianza_instituciones)
