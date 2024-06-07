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
                                            idenpa, # pais
                                            p37n_c, # A favor de recibir Inmigrantes haitianos
                                            p39st_b, # Indice negativo inmigracion: trabajo
                                            p39n_c, # Indice negativo inmigracion: crimen
                                            p39n_e) # Indice negativo inmigracion: carga E


# Comprobar y solo datos para Chile
names(proc_data)

proc_data <- proc_data %>% dplyr::filter(idenpa==152)



# --------------------------------recodificacion de variables

proc_data$p15st_d <- recode(proc_data$p15st_d, "c(-5,-4,-3,-2,-1)=NA")
proc_data$p37n_b <- recode(proc_data$p37n_b, "c(-5,-4,-3,-2,-1)=NA")
proc_data$p37n_d <- recode(proc_data$p37n_d, "c(-5,-4,-3,-2,-1,9)=NA")
proc_data$p37n_c <- recode(proc_data$p37n_c, "c(-5,-4,-3,-2,-1,0)=NA")
proc_data$s1 <- recode(proc_data$s1, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p15st_d <- recode(proc_data$p15st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p37n_b <- recode(proc_data$p37n_b, "1=3; 2=2; 3=1; 4=0")
proc_data$p37n_d <- recode(proc_data$p37n_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p37n_c <- recode(proc_data$p37n_c, "1=3; 2=2; 3=1; 4=0")
proc_data$s1 <- recode(proc_data$s1, "1=5; 2=4; 3=3; 4=2; 5=1")


proc_data <- proc_data %>% rename("conf.md"=p15st_d, # Confianza en los medios de comunicacion
                                  "afav.latin"=p37n_b, # A favor de la inmigracion latina
                                  "afav.venz"=p37n_d, # A favor de la inmigracion venezolana
                                  "clasesocial"=s1, # Clase social
                                  "afav.hait"=p37n_c) # A favor de la inmigración hatiana 



proc_data$conf.md <- set_label(x = proc_data$conf.md,label = "Confianza en Medios")
get_label(proc_data$conf.md)

proc_data$afav.latin <- set_label(x = proc_data$afav.latin,label = "A favor: Latinos")
get_label(proc_data$afav.latin)

proc_data$afav.venz <- set_label(x = proc_data$afav.venz,label = "A favor: Venezolanos")
get_label(proc_data$afav.venz)

proc_data$clasesocial <- set_label(x = proc_data$clasesocial,label = "Clase Social")
get_label(proc_data$clasesocial)

proc_data$afav.hait <- set_label(x = proc_data$afav.hait,label = "A favor: Haitianos")
get_label(proc_data$afav.hait)

frq(proc_data$afav.hait)

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
                                labels=c("Muy en desacuerdo"=0,
                                         "En desacuerdo"=1,
                                         "De Acuerdo"=2,
                                         "Muy de acuerdo"=3))


proc_data$afav.venz <- set_labels(proc_data$afav.venz,
                                   labels=c("Muy en desacuerdo"=0,
                                            "En desacuerdo"=1,
                                            "De Acuerdo"=2,
                                            "Muy de acuerdo"=3))


proc_data$afav.hait <- set_labels(proc_data$afav.hait,
                                  labels=c("Muy en desacuerdo"=0,
                                           "En desacuerdo"=1,
                                           "De Acuerdo"=2,
                                           "Muy de acuerdo"=3))



proc_data$clasesocial <- set_labels(proc_data$clasesocial,
                                  labels=c("Alta"=5,
                                           "Media alta"=4,
                                           "Media"=3,
                                           "Media baja"=2,
                                           "Baja"=1))

#--------------- Ahora recodificamos más variables para el posterior indice
frq(proc_data$in.trabajo)

proc_data$p39st_b <- recode(proc_data$p39st_b, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p39st_b <- recode(proc_data$p39st_b, "1=3; 2=2; 3=1; 4=0")

proc_data <- proc_data %>% rename("in.trabajo"=p39st_b)
                                  
proc_data$in.trabajo <- set_label(x = proc_data$in.trabajo,label = "Inmigrantes quitan trabajo")
get_label(proc_data$in.trabajo)
                                  
                                  
proc_data$in.trabajo <- set_labels(proc_data$in.trabajo,
                                   labels=c("Muy en desacuerdo"=0,
                                            "En desacuerdo"=1,
                                            "De Acuerdo"=2,
                                            "Muy de acuerdo"=3))

#----------------------- CRIMEN
## p39st_c

frq(proc_data$in.crimen)

proc_data$p39n_c <- recode(proc_data$p39n_c, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p39n_c <- recode(proc_data$p39n_c, "1=3; 2=2; 3=1; 4=0")

proc_data <- proc_data %>% rename("in.crimen"=p39n_c)

proc_data$in.crimen <- set_label(x = proc_data$in.crimen,label = "Inmigrantes aumentan el crimen")
get_label(proc_data$in.crimen)


proc_data$in.crimen <- set_labels(proc_data$in.crimen,
                                   labels=c("Muy en desacuerdo"=0,
                                            "En desacuerdo"=1,
                                            "De Acuerdo"=2,
                                            "Muy de acuerdo"=3))




#----------- carga para el estado
## p39st_e

frq(proc_data$p39n_e)

proc_data$p39n_e <- recode(proc_data$p39n_e, "c(-5,-4,-3,-2,-1)=NA")

proc_data$p39n_e <- recode(proc_data$p39n_e, "1=3; 2=2; 3=1; 4=0")

proc_data <- proc_data %>% rename("in.carga"=p39n_e)

proc_data$in.carga <- set_label(x = proc_data$in.carga,label = "Inmigrantes son una carga para el Estado")
get_label(proc_data$in.carga)


proc_data$in.carga <- set_labels(proc_data$in.carga,
                                  labels=c("Muy en desacuerdo"=0,
                                           "En desacuerdo"=1,
                                           "De Acuerdo"=2,
                                           "Muy de acuerdo"=3))




frq(proc_data$conf.md)
frq(proc_data$afav.latin)
frq(proc_data$afav.venz)
frq(proc_data$clasesocial)
frq(proc_data$afav.hait)
frq(proc_data$in.trabajo)
frq(proc_data$in.crimen)
frq(proc_data$in.carga)
#recodificacion para la variable de sexo
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
get_label(proc_data$sexo)

proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
frq(proc_data$sexo)
#recodificacion para la variable de edad
frq(proc_data$edad)

get_label(proc_data$edad)

proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

#Generacion de base de datos procesada para el analisis

proc_data <- as.data.frame(proc_data)
stargazer(proc_data, type ="text")


proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(afav.latin, na.rm=TRUE))

proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(afav.venz, na.rm=TRUE))

proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(conf.md, na.rm=TRUE))

#ahora para otra variable demografica, la clase social

proc_data %>% dplyr::group_by(clasesocial) %>% summarise(mean(conf.md, na.rm=TRUE))

#instalamos sjplot para la representación, segun clase social: conf.md - afav.latin - afav.venz (output carpet)
#Representacion de tablas descriptivas para las variables
library(sjPlot)

sjt.xtab(proc_data$clasesocial, proc_data$conf.md, encoding = "UTF-12")

sjt.xtab(proc_data$clasesocial, proc_data$afav.latin, encoding = "UTF-12")

sjt.xtab(proc_data$clasesocial, proc_data$afav.venz, encoding = "UTF-12")

#lo mismo pero con la variable sexo

sjt.xtab(proc_data$sexo, proc_data$conf.md, encoding = "UTF-12")

sjt.xtab(proc_data$sexo, proc_data$afav.latin, encoding = "UTF-12")

sjt.xtab(proc_data$sexo, proc_data$afav.venz, encoding = "UTF-12")


#Tabla descriptiva de las medidas de tendencia central

# Calcular la moda
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular las medidas de tendencia central
media <- c(mean(proc_data$clasesocial), mean(proc_data$afav.venz), mean(proc_data$afav.latin), mean(proc_data$conf.md))
mediana <- c(median(proc_data$clasesocial), median(proc_data$afav.venz), median(proc_data$afav.latin), median(proc_data$conf.md))
moda <- c(get_mode(proc_data$clasesocial), get_mode(proc_data$afav.venz), get_mode(proc_data$afav.latin), get_mode(proc_data$conf.md))

# Crear la tabla descriptiva
tabla_descriptiva <- data.frame(
  Variable = c("Clase Social", "afav.venz", "afav.latin", "conf.md"),
  Media = media,
  Mediana = mediana,
  Moda = moda
)


# Formatear la tabla con kableExtra
tabla_mtc<- tabla_descriptiva %>%
  kbl() %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE)

# Mostrar la tabla en el Viewer de RStudio
tabla_mtc

#tablas de frecuencias: Afav.venz, afav.latin, conf.md

frq(proc_data$conf.md)

frq(proc_data$afav.latin)

frq(proc_data$afav.venz)

#Librerias para graficos y tablas
pacman::p_load(sjlabelled,
               dplyr, 
               stargazer, 
               sjmisc, 
               summarytools, 
               kableExtra, 
               sjPlot,
               corrplot, 
               sessioninfo, 
               ggplot2,
               haven,
               tidyverse) 

proc_data <-na.omit(proc_data) #Borramos casos perdidos ya que no bajan tanto 
# el numero total de observaciones.


#Visualización de Variables

ggplot(proc_data, aes(x = conf.md))

proc_data %>% ggplot(aes(x = conf.md)) + geom_bar()


proc_data %>% ggplot(aes(x = conf.md)) + geom_bar(fill="purple")

proc_data %>% ggplot(aes(x = afav.latin)) + geom_bar(fill="purple")

proc_data %>% ggplot(aes(x = afav.venz)) + geom_bar(fill="purple")

proc_data %>% ggplot(aes(x = clasesocial)) + geom_bar(fill="purple")

#grafico a favor de la inmigracion en Chile: Este se utilizó para Quarto
graf.in <- sjPlot::plot_stackfrq(dplyr:: select(proc_data, afav.venz,
                                                afav.latin),
                                                title= "A favor de la inmigración: Chile") + 
  theme(legend.position="bottom")

graf.in

#grafico, comportamiento de la variable confianza en los medios de comunicacion

graf.conf.md <- sjPlot::plot_stackfrq(dplyr:: select(proc_data,conf.md),
                                 title= "Confianza en los medios de comunicación") + 
  theme(legend.position="bottom")

graf.conf.md
#=============
ggsave(graf.in, file="output/graficoinmigracion.pdf")
ggsave(graf.conf.md, file="output/graficomedios.pdf")
#los demas graficos y tablas guardados en la carpeta output

#grafico, confianza en los medios de comunicacion por sexo       

graf.insex <- proc_data %>% ggplot(aes(x = conf.md, fill = sexo)) + 
  geom_bar() +
  xlab("Confianza en los medios de comunicación") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graf.insex

ggsave(graf.insex, file="output/graficoinsex.pdf")

#Grafico a favor de la inmigracion latina segun clase social

proc_data %>% ggplot(aes(x = afav.latin)) + 
  geom_bar() +
  xlab("A favor de la inmigración latina") +
  ylab("Cantidad")+
  facet_wrap(~clasesocial)

frq(proc_data$afav.latin)
frq(proc_data$clasesocial)

#A favor de la inmigración latina en función por la confianza
#en los medios de comunicación: Este se utilizó para Quarto

proc_data %>% ggplot(aes(x = afav.latin)) + 
  geom_bar() +
  xlab("A favor de la inmigración latina") +
  ylab("Cantidad")+
  facet_wrap(~conf.md)

frq(proc_data$conf.md)
frq(proc_data$afav.latin)

#Lo mismo, pero con la inmigracion venezolana: Este se utilizó para Quarto
proc_data %>% ggplot(aes(x = afav.venz)) + 
  geom_bar() +
  xlab("A favor de la inmigración latina") +
  ylab("Cantidad")+
  facet_wrap(~conf.md)

frq(proc_data$afav.venz)
frq(proc_data$conf.md)

#guardar base proc_data para quarto document

save(proc_data, file="input/data/proc_data.RData")



#-----parte de hacer correlación y indices

pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

#forzar a la base que todas sus variables sean de tipo numérica


class(proc_data$conf.md)
class(proc_data$in.carga)

#forzar a la base que todas sus variables sean de tipo numérica
basecor <- proc_data %>% mutate_all(~(as.numeric(.)))

Mcor <- cor(basecor, use = "complete.obs")

corrplot.mixed(Mcor)

#estos dos codigos son para la correlacion de todas las variables

#ahora veremos correlación en variables en especifico. - Este para Quarto


Mcor2 <- cor(dplyr::select(basecor,conf.md, afav.latin, afav.hait, afav.venz,
                           in.trabajo, in.crimen, in.carga))
corrplot.mixed(Mcor2)


#Indice ponderado.
#Pesos para las variables en c(
afavlatin <- basecor$afav.latin
afavvenz <- basecor$afav.venz
afavhait <- basecor$afav.hait


pesosvar <- c(0.30, 0.40, 0.30)

indicexen1 <- (afavlatin * pesosvar[1] + afavvenz * pesosvar[2] + afavhait * pesosvar[3])

basecor$indicexen1 <- indicexen1


#ahora para las de afirmacion

intrabajo <- basecor$in.trabajo
incarga <- basecor$in.carga
incrimen <- basecor$in.crimen


pesosvar2 <- c(0.30, 0.40, 0.30)

indicexen2 <- (intrabajo * pesosvar[1] + incarga * pesosvar[2] + incrimen * pesosvar[3])

basecor$indicexen2 <- indicexen2




#una vez hecho el indice ponderado "Indice de Xenofobia" guardaremos la base "basecor" para quarto

save(basecor, file="input/data/basecor.RData")


#ahora crearemos una base para un gráfico de dispersión con variables de interés

basecor2 <- basecor %>% select(afav.latin, afav.venz, afav.hait, in.trabajo, in.crimen, in.carga, conf.md)

basecor2 <- basecor2 %>%
  rowwise() %>%
  mutate(afav.venz = sum(c(afav.hait, afav.latin), na.rm = TRUE))

ggpairs(basecor2)

# Cargar la base de datos basecor si aún no lo has hecho
# Suponiendo que basecor es un data frame con la variable indicexen

# Crear el histograma: Indice de Xenofobia

hist(basecor$indicexen, 
     main = "Histograma de Índice de Xenofobia en Chile", 
     xlab = "Índice de Xenofobia",
     ylab = "Frecuencia",
     col = "purple", 
     border = "yellow", 
     breaks = 10 
)


#MTC
summary(basecor$indicexen1)

summary(basecor$indicexen2)

#Modelo de regresion multiple - último trabajo


#Se realizará una nueva base de datos en que se invertirá la recodificación
#hecha en el proyecto

lastbase <- latinobarometro2020 %>% select(p15st_d, # Medios de comunicacion
                               p37n_b, # A favor de recibir Inmigrantes Latinos
                               p37n_d, # A favor de recibir Inmigrantes venezolanos
                               s1, # Clase social
                               sexo, # sexo
                               edad, #edad
                               idenpa, # pais
                               p37n_c, # A favor de recibir Inmigrantes haitianos
                               p39st_b, # Indice negativo inmigracion: trabajo
                               p39n_c, # Indice negativo inmigracion: crimen
                               p39n_e, # Indice negativo inmigracion: carga E
                               s16,
                               p18st) # Nivel educacional alcanzado

lastbase <- lastbase %>% dplyr::filter(idenpa==152)

lastbase <-na.omit(lastbase)

# --------------------------------recodificacion de variables

lastbase$p15st_d <- recode(lastbase$p15st_d, "c(-5,-4,-3,-2,-1)=NA")
lastbase$p37n_b <- recode(lastbase$p37n_b, "c(-5,-4,-3,-2,-1)=NA")
lastbase$p37n_d <- recode(lastbase$p37n_d, "c(-5,-4,-3,-2,-1,9)=NA")
lastbase$p37n_c <- recode(lastbase$p37n_c, "c(-5,-4,-3,-2,-1,0)=NA")
lastbase$s1 <- recode(lastbase$s1, "c(-5,-4,-3,-2,-1)=NA")

lastbase$p15st_d <- recode(lastbase$p15st_d, "1=3; 2=2; 3=1; 4=0")
lastbase$p37n_b <- recode(lastbase$p37n_b, "1=3; 2=2; 3=1; 4=0")
lastbase$p37n_d <- recode(lastbase$p37n_d, "1=3; 2=2; 3=1; 4=0")
lastbase$p37n_c <- recode(lastbase$p37n_c, "1=3; 2=2; 3=1; 4=0")
lastbase$s1 <- recode(lastbase$s1, "1=5; 2=4; 3=3; 4=2; 5=1")


lastbase <- lastbase %>% rename("conf.md"=p15st_d, # Confianza en los medios de comunicacion
                                  "afav.latin"=p37n_b, # A favor de la inmigracion latina
                                  "afav.venz"=p37n_d, # A favor de la inmigracion venezolana
                                  "clasesocial"=s1, # Clase social
                                  "afav.hait"=p37n_c) # A favor de la inmigración hatiana 



lastbase$conf.md <- set_label(x = lastbase$conf.md,label = "Confianza en Medios")
get_label(lastbase$conf.md)

lastbase$afav.latin <- set_label(x = lastbase$afav.latin,label = "A favor: Latinos")
get_label(lastbase$afav.latin)

lastbase$afav.venz <- set_label(x = lastbase$afav.venz,label = "A favor: Venezolanos")
get_label(lastbase$afav.venz)

lastbase$clasesocial <- set_label(x = lastbase$clasesocial,label = "Clase Social")
get_label(lastbase$clasesocial)

lastbase$afav.hait <- set_label(x = lastbase$afav.hait,label = "A favor: Haitianos")
get_label(lastbase$afav.hait)



lastbase$conf.md <- set_labels(lastbase$conf.md,
                                labels=c("Ninguna"=3,
                                         "Poca"=2,
                                         "Algo"=1,
                                         "Mucha"=0))

lastbase$afav.latin <- set_labels(lastbase$afav.latin,
                                   labels=c("Muy en desacuerdo"=3,
                                            "En desacuerdo"=2,
                                            "De Acuerdo"=1,
                                            "Muy de acuerdo"=0))


lastbase$afav.venz <- set_labels(lastbase$afav.venz,
                                  labels=c("Muy en desacuerdo"=3,
                                           "En desacuerdo"=2,
                                           "De Acuerdo"=1,
                                           "Muy de acuerdo"=0))


lastbase$afav.hait <- set_labels(lastbase$afav.hait,
                                  labels=c("Muy en desacuerdo"=3,
                                           "En desacuerdo"=2,
                                           "De Acuerdo"=1,
                                           "Muy de acuerdo"=0))



lastbase$clasesocial <- set_labels(lastbase$clasesocial,
                                    labels=c("Alta"=5,
                                             "Media alta"=4,
                                             "Media"=3,
                                             "Media baja"=2,
                                             "Baja"=1))

#--------------- Ahora recodificamos más variables para el posterior indice
frq(proc_data$in.trabajo)

lastbase$p39st_b <- recode(lastbase$p39st_b, "c(-5,-4,-3,-2,-1)=NA")

lastbase$p39st_b <- recode(lastbase$p39st_b, "1=3; 2=2; 3=1; 4=0")

lastbase <- lastbase %>% rename("in.trabajo"=p39st_b)

lastbase$in.trabajo <- set_label(x = lastbase$in.trabajo,label = "Inmigrantes quitan trabajo")
get_label(lastbase$in.trabajo)


lastbase$in.trabajo <- set_labels(lastbase$in.trabajo,
                                   labels=c("Muy en desacuerdo"=0,
                                            "En desacuerdo"=1,
                                            "De Acuerdo"=2,
                                            "Muy de acuerdo"=3))







#----------------------- CRIMEN
## p39st_c

frq(lastbase$in.crimen)

lastbase$in.crimen <- recode(lastbase$p39n_c, "c(-5,-4,-3,-2,-1)=NA")

lastbase$in.crimen <- recode(lastbase$in.crimen, "1=3; 2=2; 3=1; 4=0")


lastbase$in.crimen <- set_label(x = lastbase$in.crimen,label = "Inmigrantes aumentan el crimen")
get_label(lastbase$in.crimen)


lastbase$in.crimen <- set_labels(lastbase$in.crimen,
                                  labels=c("Muy en desacuerdo"=0,
                                           "En desacuerdo"=1,
                                           "De Acuerdo"=2,
                                           "Muy de acuerdo"=3))




#----------- carga para el estado
## p39st_e

lastbase$in.carga <- recode(lastbase$p39n_e, "c(-5,-4,-3,-2,-1)=NA")

lastbase$in.carga <- recode(lastbase$in.carga, "1=3; 2=2; 3=1; 4=0")

lastbase$in.carga <- set_label(x = lastbase$in.carga,label = "Inmigrantes son una carga para el Estado")
get_label(lastbase$in.carga)


lastbase$in.carga <- set_labels(lastbase$in.carga,
                                 labels=c("Muy en desacuerdo"=0,
                                          "En desacuerdo"=1,
                                          "De Acuerdo"=2,
                                          "Muy de acuerdo"=3))



#Indice ponderado.
#Pesos para las variables en c(
afavlatin <- lastbase$afav.latin
afavvenz <- lastbase$afav.venz
afavhait <- lastbase$afav.hait

intrabajo <- lastbase$in.trabajo
incarga <- lastbase$in.carga
incrimen <- lastbase$in.crimen


pesosvar <- c(0.18, 0.20, 0.18, 0.18, 0.20, 0.18)

indicexen1 <- (afavlatin * pesosvar[1] + afavvenz * pesosvar[2] + afavhait * pesosvar[3] + intrabajo * pesosvar[4] + incarga * pesosvar[5] + incrimen * pesosvar[6])

lastbase$indicexen1 <- indicexen1

frq(lastbase$indicexen1)



pacman::p_load(dplyr, car, sjmisc, sjPlot, sjlabelled, stargazer, kableExtra, corrplot, texreg, ggplot2, ggpubr)

lastbase <- na.omit(lastbase) #abse lista para los modelos de regresión


#poner en quarto
regresion1 <- lm(indicexen1 ~ conf.md, data=lastbase)
stargazer(regresion1, type = "text")
regresion2 <- lm(indicexen1 ~ edad, data=lastbase)
stargazer(regresion2, type = "text")
regresion3 <- lm(indicexen1 ~ clasesocial, data=lastbase)
stargazer(regresion3, type = "text")

#poner en quarto
knitreg(list(regresion1, regresion2, regresion3), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "confianza en los medios",
                              "Edad", 
                              "clase social"),
        caption = "Xenofobia",
        caption.above = TRUE)

#una vez hecho el indice ponderado "Indice de Xenofobia" guardaremos la base "basecor" para quarto

save(basecor, file="input/data/lastbase.RData")


# Recode the variable s6 in the lastbase dataset
lastbase$Educacion <- cut(lastbase$s16,
                         breaks = c(0, 1, 8, 12, 13, 14, 15, 16, 17),
                         labels = c("Sin estudios", "Nivel básico", "Nivel Medio", "Nivel Medio", 
                                    "Universitario incompleto", "Universitario Completo",
                                    "Institutos incompletos", "Institutos completos"),
                         right = FALSE)

# Convert to factor to ensure it is categorical
lastbase$Educacion <- factor(lastbase$Educacion,
                            levels = c("Sin estudios", "Nivel básico", "Nivel Medio",
                                       "Universitario incompleto", "Universitario Completo",
                                       "Institutos incompletos", "Institutos completos"))



#Ahora con regresión lineal múltiple

regresion4 <- lm(indicexen1 ~ conf.md + edad + clasesocial, data=lastbase)
stargazer(regresion4, type = "text")

regresion5 <- lm(indicexen1 ~ conf.md + clasesocial + Educacion, data=lastbase)
stargazer(regresion5, type = "text")


knitreg(list(regresion4, regresion5), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "confianza en los medios",
                              "Edad", 
                              "clase social",
                              "Educacion Nivel Medio",
                              "Educacion Universitario Incompleto",
                              "Educacion Universitario Completo",
                              "Educación Institutos incompletos",
                              "Educación Institutos completos"),
        caption = "Índice de Xenofobia",
        caption.above = TRUE)
#graficar

plot_model(regresion5,
           title = "", #quitar titulo
           show.values = TRUE, #mostrar valor de efectos
           dot.size = 3, #tamaño circulos
           line.size = 1, #tamaño CI
           value.size = 4, #tamaño valor efectoss
           spacing = 1, #espacio entre efectos
           vline.color = "red")
           
          

#Cálculo de valores predichos
pacman::p_load(dplyr, 
               car, 
               summarytools, 
               sjPlot, 
               texreg, 
               corrplot, 
               ggplot2, 
               sjlabelled, 
               fastDummies, 
               ggeffects)

knitreg(list(regresion5), 
        custom.model.names = c("Modelo 1"),
        custom.coef.names = c("Intercepto",
                              "conf.md",
                              "clasesocial",
                              "Educacion Nivel Medio",
                              "Educacion Universitaria incompleto",
                              "Educacion Universitario completo",
                              "Institutos incompletos",
                              "Institutos completos"))



ggeffects::ggpredict(regresion5, terms = c("Educacion")) %>%
  ggplot(aes(x=x, y=predicted)) +
  geom_bar(stat="identity", color="grey", fill="grey")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.1) +
  labs(title="Educacion", x = "", y = "") +
  theme_bw() +
  scale_x_discrete(name = "",
                   labels = c("Intercepto", "conf.md", "clasesocial", "Educ. Nivel Medio", "Universitaria In", "Universitaria Com", "Institutos In", "Institutos Com"))+
  scale_y_continuous(limits = c(0,16), 
                     breaks = seq(0,16, by = 1))


load("input/data/lastbase.RData")
