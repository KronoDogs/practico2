install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

rm(list=ls())       
options(scipen=999) 

#cargamos la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))


proc_data <- latinobarometro2020 %>% select(p15st_d, # Medios de comunicacion
                                            p37n_b, # A favor de recibir Inmigrantes Latinos
                                            p37n_d, # A favor de recibir Inmigrantes venezolanos
                                            s1, # Clase social
                                            sexo, # sexo
                                            edad, #edad
                                            idenpa) # pais

# Comprobar y solo datos para Chile
names(proc_data)

proc_data <- proc_data %>% dplyr::filter(idenpa==152)

frq(proc_data$conf.md)
frq(proc_data$afav.latin)
frq(proc_data$afav.venz)
frq(proc_data$clasesocial)

# recodificacion de variables

proc_data$p15st_d <- recode(proc_data$p15st_d, "c(-5,-4,-3,-2,-1)=NA")
proc_data$p37n_b <- recode(proc_data$p37n_b, "c(-5,-4,-3,-2,-1)=NA")
proc_data$p37n_d <- recode(proc_data$p37n_d, "c(-5,-4,-3,-2,-1,9)=NA")
proc_data$s1 <- recode(proc_data$s1, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p15st_d <- recode(proc_data$p15st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p37n_b <- recode(proc_data$p37_b, "1=3; 2=2; 3=1; 4=0")
proc_data$p37n_d <- recode(proc_data$p37n_d, "1=3; 2=2; 3=1; 4=0")
proc_data$s1 <- recode(proc_data$s1, "1=5; 2=4; 3=3; 4=2; 5=1")


proc_data <- proc_data %>% rename("conf.md"=p15st_d, # Confianza en los medios de comunicacion
                                  "afav.latin"=p37n_b, # A favor de la inmigracion latina
                                  "afav.venz"=p37n_d, # A favor de la inmigracion venezolana
                                  "clasesocial"=s1) # Clase social 




proc_data$conf.md <- set_label(x = proc_data$conf.md,label = "Confianza en Medios")
get_label(proc_data$conf.md)

proc_data$afav.latin <- set_label(x = proc_data$afav.latin,label = "A favor: Latinos")
get_label(proc_data$afav.latin)

proc_data$afav.venz <- set_label(x = proc_data$afav.venz,label = "A favor: Venezolanos")
get_label(proc_data$afav.venz)

proc_data$clasesocial <- set_label(x = proc_data$clasesocial,label = "Clase Social")
get_label(proc_data$clasesocial)

install.packages("sjlabelled")
library(sjlabelled)

frq(proc_data$conf.md)
frq(proc_data$afav.latin)
frq(proc_data$afav.venz)
frq(proc_data$clasesocial)

proc_data$conf.md <- set_labels(proc_data$conf.md,
                                labels=c("Ninguna"=0,
                                          "Poca"=1,
                                          "Algo"=2,
                                          "Mucha"=3))

proc_data$afav.latin <- set_labels(proc_data$afav.latin,
                                labels=c("Muy negativo"=1,
                                         "Negativo"=2,
                                         "Positivo"=3,
                                         "Muy positivo"=4))


proc_data$afav.venz <- set_labels(proc_data$afav.venz,
                                   labels=c("Muy negativo"=1,
                                            "Negativo"=2,
                                            "Positivo"=3,
                                            "Muy positivo"=4))


proc_data$clasesocial <- set_labels(proc_data$clasesocial,
                                  labels=c("Alta"=5,
                                           "Media alta"=4,
                                           "Media"=3,
                                           "Media baja"=2,
                                           "Baja"=1))


frq(proc_data$conf.md)
frq(proc_data$afav.latin)
frq(proc_data$afav.venz)
frq(proc_data$clasesocial)

frq(proc_data$sexo)

#recodificacion para la variable de sexo
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
get_label(proc_data$sexo)

frq(proc_data$sexo)
#recodificacion para la variable de edad
frq(proc_data$edad)

get_label(proc_data$edad)

proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")
